--  $Id: threads-sched_edf.ads,v 1.2 2008/11/23 01:54:28 baker Exp $

-- EDF scheduler policy plug-in

package Threads.Sched_EDF is

   type Object is new
     Threads.Scheduling_Policies.Object with private;

   procedure Init
     (P : in out Object);
   procedure New_Job
     (P : in out Object;
      J : in Jobs.Job);

private

   type Object is new
     Threads.Scheduling_Policies.Object with record
     Current_Job : Jobs.Job;
   end record;

end Threads.Sched_EDF;
