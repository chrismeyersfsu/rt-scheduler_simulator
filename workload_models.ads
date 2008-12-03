--  $Id: workload_models.ads,v 1.3 2008/11/24 02:05:39 baker Exp $

--  This is just a root for deriving actual classes.

with Jobs;
with Virtual_Times; use Virtual_Times;
package Workload_Models is

   type Object is abstract tagged null record;
   type Class_Ref is access all Object'Class;

   function Start_Time (M: Object) return Time is abstract;

   procedure Arrive (M : in out Object;
                     T : in Time;
                     J : out Jobs.Job;
                     Next_Arrival_Time : out Time) is abstract;

   function Utilization (M : Object) return Float is abstract;

   function Name (Model : Object) return String is abstract;

end Workload_Models;
