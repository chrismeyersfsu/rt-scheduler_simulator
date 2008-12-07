--  $Id: threads-sched_pls.ads,v 1.2 2008/11/24 16:46:29 baker Exp $

-- deadline polling server scheduler policy plug-in

with Aperiodic_Server_Parameters; use Aperiodic_Server_Parameters;
with Threads.Aperiodic_Policies;
with Tasks;
package Threads.Sched_TBS is

   type Object is new
     Threads.Aperiodic_Policies.Object with private;

   procedure Bind_Parms
     (P : in out Object;
      Parms : Aperiodic_Server_Parameters.Parameters);
   procedure Init
     (P : in out Object);

   procedure Suspend
     (P : in out Object);
   procedure Unsuspend
     (P : in out Object);
   procedure Go
     (P : in out Object);
   procedure Stop
     (P : in out Object);
   procedure New_Job
     (P : in out Object;
      J : in Jobs.Job);
   procedure Idle
     (P : in out Object);

private

--   package Job_Completion_Events 
--	procedure Handler (E : in out Object);
--   end Job_Completion_Events;

--   package Job_Arrival_Events is
--	type Object is new Events.Object with record
--	  T : Thread_ref;
--	end record;
--	procedure Handler (E : in out Object);
--   end Job_Arrival_Events;

   type Object is new
     Threads.Aperiodic_Policies.Object with record
        Parms : Aperiodic_Server_Parameters.Parameters;
	Current_Job : Jobs.Job;
	Utilization : Float;
   end record;


end Threads.Sched_TBS;
