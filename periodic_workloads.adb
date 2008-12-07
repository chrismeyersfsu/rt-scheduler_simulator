--  $Id: periodic_workloads.adb,v 1.4 2008/11/24 02:05:39 baker Exp $

package body Periodic_Workloads is

   procedure Bind_Parms (M : in out Object;
                         P : Parameters) is
   begin
      M.Parms := P;
      Pragma Assert (P.WCET in 0 .. P.Period);
   end Bind_Parms;

   function Utilization (M : Object) return Float is
   begin
      return Float (M.Parms.WCET) / Float (M.Parms.Period);
   end Utilization;

   procedure Arrive (M : in out Object;
                     T : in Time;
                     J : out Jobs.Job;
                     Next_Arrival_Time : out Time) is
   begin
      J.Arrival_Time := T;
      J.Execution_Time := M.Parms.WCET;
      J.Absolute_Deadline := T + M.Parms.Deadline;
      Next_Arrival_Time := T + M.Parms.Period;
   end Arrive;

   function Start_Time (M : Object) return Time is
   begin
      return 0;
   end Start_Time;

   function Name (Model : Object) return String is
   begin
      return "periodic";
   end Name;

end Periodic_Workloads;
