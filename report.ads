--  $Id: report.ads,v 1.4 2008/11/24 16:46:29 baker Exp baker $

with Sizes; use Sizes;
with Stats;
package Report is

   --  for data collection

   procedure New_IAT;

   procedure Collect_Server
     (R : Runs;
      L : Loads;
      S : Servers;
      D : Stats.Data);

   procedure Collect_Periodic
     (R : Runs;
      L : Loads;
      S : Servers;
      D : Stats.Data);

   procedure Validate
     (R : Runs;
      L : Loads;
      S : Servers);

   --  for Latex output

   procedure Start_Tex
     (I : IATS);

   procedure Put_Tex
     (I : IATS;
      R : Runs;
      L : Loads);

   procedure Break_Tex;

   procedure Finish_Tex
     (I : IATS);

   --  for Gunplot output

   procedure Start_Plot
     (I : IATS;
      R : Runs);

   procedure Put_Plot
     (I : IATS;
      R : Runs;
      L : Loads);

   procedure Finish_Plot;

end Report;
