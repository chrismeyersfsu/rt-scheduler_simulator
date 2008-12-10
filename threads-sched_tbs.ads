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

   procedure New_Job
     (P : in out Object;
      J : in Jobs.Job);

private
   type Object is new
     Threads.Aperiodic_Policies.Object with record
        Parms : Aperiodic_Server_Parameters.Parameters;
	Server_Utilization : Float;
   end record;
end Threads.Sched_TBS;
