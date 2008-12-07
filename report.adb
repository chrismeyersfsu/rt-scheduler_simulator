--  $Id: report.adb,v 1.3 2008/11/24 02:05:39 baker Exp 1332

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions;
with Tasks;
with Threads;
with Stats; use Stats;
with Sizes;
with Error_Log; use Error_Log;
with Simulator;
with Virtual_Times; use Virtual_Times;
package body Report is

   File : File_Type;

   --  value reserved to indicate erroneous output

   Erroneous : constant Integer := -99999;

   --  statistics are indexed by type of server

   type Float_Cube is array (Servers) of Float_Table;
   type Int_Table is array (Runs) of Int_Array (Loads);
   type Int_Cube  is array (Servers) of Int_Table;

   --  average response time of aperiodic request
   Avg :  Float_Cube := (others => (others => (others => 0.0)));

   --  standard deviation of response time
   SD :   Float_Cube := (others => (others => (others =>  0.0)));

   --  maximum response time
   Max :  Float_Cube := (others => (others => (others =>  0.0)));

   --  utilization
   Util : Float_Cube := (others => (others => (others =>  0.0)));


   --  time usage break down, indexed by:
   --  1) type of server
   --  2) run (corresponding to periodic load level)
   --  These are used to check consistency of the simulation,
   --  to verify that we have not used more or less time
   --  than should have elapsed on the clock.

   Comptd : Int_Cube := (others => (others => (others =>  0)));
   Missed : Int_Cube := (others => (others => (others =>  0)));

   --  execution time consumed

   Exec :   Int_Cube := (others => (others => (others =>  0)));

   --  constants used to compute various confidence levels

   Z90 : constant:= 1.6449; -- for 90% confidence interval
   Z95 : constant:= 1.9600; -- for 95% confidence interval
   Z99 : constant:= 2.5758; -- for 99% confidence interval

   Tex : File_Type;
   Plot : File_Type;
   Prefix : String := "EDF";

   procedure New_IAT is
      --  re-initialize all tables
   begin
      for R in Runs loop
         for L in Loads loop
            for S in BGS .. DSS loop
               Exec (S)(R)(L) := 0;
               Missed (S)(R)(L) := 0;
               Comptd (S)(R)(L) := 0;
               Avg (S)(R)(L) := 0.0;
               SD (S)(R)(L) := 0.0;
               Max (S)(R)(L) := 0.0;
               Util (S)(R)(L) := 0.0;
            end loop;
         end loop;
      end loop;
   end New_IAT;

   procedure Collect_Periodic
     (R : Runs;
      L : Loads;
      S : Servers;
      D : Stats.Data) is
   begin
      if D.Missed_Deadlines > 0 then
         Put (Log, "*** missed ");
         Put (Log, D.Missed_Deadlines, 0);
         Put_Line (Log, " deadlines");
         raise Program_Error;
      end if;
      Exec (S)(R)(L) := Exec (S)(R)(L) +
        Integer (D.Total_Exec_Time);
      Missed (S)(R)(L) := Missed (S)(R)(L) + D.Missed_Deadlines;
      pragma Assert (D.Missed_Deadlines = 0);
      pragma Assert (D.Total_Exec_Time > 0);
   end Collect_Periodic;

   procedure Collect_Server
     (R : Runs;
      L : Loads;
      S : Servers;
      D : Stats.Data) is
     use Ada.Numerics.Elementary_Functions;
   begin
      pragma Assert (Simulator.Current_Time >= 1);
      pragma Assert (D.Job_Count >= 0);
      pragma Assert (D.Total_Resp_Time >= 0.0);
      if D.Job_Count = 0 then
         Trace (0, "report : *** no server jobs completed (apparent serious overload)");
         Put_Line ("report : *** no server jobs completed (apparent serious overload)");
      end if;
      Avg (S)(R)(L) :=
        Float (D.Total_Resp_Time) / Float (D.Job_Count);
      Max (S)(R)(L) := Float (D.Max_Resp_Time);
      Exec (S)(R)(L) := Exec (S)(R)(L)
        + Integer (D.Total_Exec_Time);
      Missed (S)(R)(L) := Missed (S)(R)(L) + D.Missed_Deadlines;
      if (D.Job_Count > 0) then
        SD (S)(R)(L) := Sqrt ((D.Total_Sq_Resp_Time -
         (D.Total_Resp_Time**2) / Float (D.Job_Count))/
         Float (D.Job_Count - 1));
      else
        SD (S)(R)(L) := float (Erroneous);
      end if;
      Util (S)(R)(L) := Float (D.Total_Exec_Time)
        * 100.00 / Float (Simulator.Current_Time);
      Comptd (S)(R)(L) := Comptd (S)(R)(L) + D.Job_Count;
   end Collect_Server;

   procedure Validate
     (R : Runs;
      L : Loads;
      S : Servers) is
      Idle : Time := Threads.Total_Idle_Time;
      Exe : Time := Time (Exec (S)(R)(L));
      Now : Time := Simulator.Current_Time;
   begin
      if Idle + Exe /= Now then
         Trace (0, "Idle + Exec /= Now");
         Put (Log, " Idle = ");
         Put (Log, Integer (Idle), 0);
         Put (Log, " Exec = ");
         Put (Log, Integer (Exe), 0);
         Put (Log, " Now = ");
         Put (Log, Integer (Now), 0);
         New_Line (Log);
         Put_Line (Log, "reoprt : time inconsistency -- probable overload");
         Put_Line ("report : time inconsistency -- probable overload");
      end if;
   end Validate;

   function Plot_File_Name (R: Runs; I: IATS) return String is
   begin
     return Prefix & character'val (character'pos ('A') + R - 1) &
            character'val (character'pos ('A') + I - 1);
   end Plot_File_Name;

   procedure Start_Plot
     (I : IATS;
      R : Runs) is
      Filename_Root : constant String :=
        Plot_File_Name (R, I);
   begin
      --  open or create the Plot output file
      begin
         create (Plot, out_file, Filename_Root & ".pts");
      exception when others=>
         open (Plot, out_file, Filename_Root & ".pts");
      end;
   end Start_Plot;

   procedure Put_Plot (F : Float) is
   begin
      if F < 0.0 then
         Put (Plot, Float (Erroneous));
      else
         Put (Plot, F, 4, 3, 0);
     end if;
     Put (Plot, " ");
   end Put_Plot;

   procedure Validate_Plot_Line
     (I : IATS;
      R : Runs;
      L : Loads;
      M : Float) is
      -- check for average response times that are better than M/M/1
   begin
      for S in BGS .. DSS loop
         if Avg (S)(R)(L) < M then
            Put (Log, Plot_File_Name (R, I));
            Put (Log, " at utilization ");
            Put (Log, Util (Sizes.BGS)(R)(L), 4, 3, 0);
            Put (Log, ' ');
            Put (Log, Sizes.Servers'image (S));
            Put (Log, " response time is below M/M/1 by ");
            Put (Log, M - Avg (S)(R)(L), 2, 2, 0);
            New_line (Log);
         end if;
      end loop;
   end Validate_Plot_Line;

   procedure Put_Plot
     (I : IATS;
      R : Runs;
      L : Loads) is
      IAT :  Float := Interarrival_Times (I);
      AL :  Float := Aperiodic_Load (R)(L);
      M : Float := -- M/M/1, for full server
         AL * IAT / (1.0 - AL);
      -- M/M/1, relative to server capacity =
      -- S: constant Float := float (EDF_Size (R)(Sizes.PLS));
      -- M: constant Float := S*AL*IAT/ (S-AL*float (Server_Period));
   begin
      --  put line of plot file

      pragma Assert (1.0 - AL > 0.0);
      pragma Assert (IAT > 0.0);
      Validate_Plot_Line (I, R, L, M);
      pragma Assert (AL > 0.0);
      put_Plot (AL);                                  -- 1
      put_Plot (Avg (Sizes.BGS)(R)(L));               -- 2
      put_Plot (Avg (Sizes.PLS)(R)(L));               -- 3
      pragma Assert (Avg (Sizes.DSS)(R)(L) >= 0.0);
      put_Plot (Avg (Sizes.DSS)(R)(L));               -- 4
      pragma Assert (M > 0.0);
      put_Plot (M);                                   -- 6
      --  put_Plot (Avg (Sizes.DDS)(R)(L));           -- 7
      --  put_Plot (Avg (Sizes.DXS)(R)(L));            -- 8
      new_line (Plot);

   end Put_Plot;

   procedure Finish_Plot is
   begin
       Close (Plot);
   end Finish_Plot;

   function Tex_File_Name (I: IATS) return String is
   begin
     return Prefix & character'val (character'pos ('A') + I - 1);
   end Tex_File_Name;

   procedure Start_Tex
     (I : IATS) is
      Filename_Root : constant String :=
        Tex_File_Name (I);
   begin
      --  open or create the LaTeX output file
      begin
         create (Tex, out_file, Filename_Root & ".tex");
      exception when others=>
         open (Tex, out_file, Filename_Root & ".tex");
      end;
      --  put out the Latex table header
      put_line (Tex, "\begin{table}\begin{center}");
      put_line (Tex, "\begin{tabular}{ccr@{}rr@{}rr@{}rr@{}rr@{}r}");
      put_line (Tex, "periodic&aperiodic");
      for S in BGS .. DSS loop
         --  ??? suppress output for unused polices?
         Put (Tex, "&\twocol{" & Servers'Image (S) & "}");
      end loop;
      put_line (Tex, "\\[10pt]\hline");
   end Start_Tex;

   procedure Put_Tex (S : Sizes.Servers; R : Runs; L : Loads) is
      F : constant Float := Avg (S)(R)(L);
      use Ada.Numerics.Elementary_Functions;
   begin
      if F > 0.0 then
         pragma Assert (Comptd (S)(R)(L) >= 1);
         put (Tex, " & "); put (Tex, Integer (F), 5); put (Tex, " & $\PM$ ");
         put (Tex, Integer (Z99 * SD (S)(R)(L) /
                              Sqrt (float (Comptd (S)(R)(L)))), 3);
      else
         put (Tex, " & \overload & \overload ");
      end if;
   end Put_Tex;

   procedure Put_Tex
     (I : IATS;
      R : Runs;
      L : Loads) is
   begin

      --  put out section of LaTeX table file

      --  periodic task utilization level for this run
       put (Tex, float (U (R))/100.0, 1, 2, 0);
       put (Tex, " & ");
       --  average aperiodic server utlization level for this run
       put (Tex, Util (Sizes.BGS)(R)(L), 1, 2, 0);
       for S in BGS .. DSS loop
          --  for each server size
          --  put out the results of the experiment
          Put_Tex (S, R, L);
      end loop;
      put (Tex, "\\");
      new_line (Tex);

   end Put_Tex;

   procedure Break_Tex is
   begin
      Put_Line (Tex, "\\");
   end Break_Tex;

   procedure Finish_Tex (I : IATS) is
   begin
       Put (Tex, "\end{tabular}\end{center}");
       Put (Tex, "\caption{EDF response times, mean aperiodic IAT ");
       Put (Tex, Integer (Float (Simulator.Current_Time)
                            / Float (Sizes.Interarrival_Times (I))), 1);
       Put (Tex, ", after ");
       Put (Tex, Integer (Simulator.Current_Time), 1);
       Put_line (Tex, " time units.}");
       Put (Tex, "\label{tab:");
       Put (Tex, character'val (character'pos ('A')+I-1));
       Put_line (Tex, "}\end{table}");
       Close (Tex);
   end Finish_Tex;

end Report;
