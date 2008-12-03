-- $Id: aperiodic_workloads.ads,v 1.4 2008/11/24 02:05:39 baker Exp $

with Jobs;
with Virtual_Times; use Virtual_Times;
with Workload_Models;
package Aperiodic_Workloads is

   type Object is new Workload_Models.Object with private;

   type Parameters is record
      Mean_Interarrival_Time : Float;
      Mean_Execution_Time : Float;
   end record;

   procedure Bind_Parms (M : in out Object;
                         P : Parameters);

   procedure Arrive (M : in out Object;
                     T : in Time;
                     J : out Jobs.Job;
                     Next_Arrival_Time : out Time);

   function Utilization (M : Object) return Float;

   function Start_Time (M : Object) return Time;

   function Name (Model : Object) return String;

private

   type Object is new Workload_Models.Object with record
      Parms: Parameters;
   end record;

end Aperiodic_Workloads;
