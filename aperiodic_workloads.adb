--  $Id: aperiodic_workloads.adb,v 1.6 2008/11/24 02:05:39 baker Exp $

with Random_Tools;  use Random_Tools;
with Error_Log; use Error_Log;
with Ada.Text_IO; use Ada.Text_IO;
package body Aperiodic_Workloads is

   procedure Bind_Parms (M : in out Object;
                         P : Parameters) is
   begin
      M.Parms := P;
      Pragma Assert (P.Mean_Interarrival_Time > P.Mean_Execution_Time);
   end Bind_Parms;

   function Utilization (M : Object) return Float is
   begin
      return M.Parms.Mean_Execution_Time / M.Parms.Mean_Interarrival_Time;
   end Utilization;

   procedure Arrive (M : in out Object;
                     T : in Time;
                     J : out Jobs.Job;
                     Next_Arrival_Time : out Time) is
      IAT : Time;
   begin
      J.Arrival_Time := T;
      J.Absolute_Deadline := Time'Last;
      loop
         J.Execution_Time :=
           Time (Exp_Dist (M.Parms.Mean_Execution_Time));
           exit when J.Execution_Time > 0;
      end loop;
      loop
         IAT := Time (Exp_Dist (M.Parms.Mean_Interarrival_Time));
         pragma Debug (Debug (11, "next inter-arrival_time = " & Time 'Image(IAT)));
         exit when IAT > 0;
      end loop;
      Next_Arrival_Time := T + IAT;
   end Arrive;

   function Start_Time (M : Object) return Time is
   begin
      return 0;
   end Start_Time;

   function Name (Model : Object) return String is
   begin
      return "aperiodic";
   end Name;

end Aperiodic_Workloads;
