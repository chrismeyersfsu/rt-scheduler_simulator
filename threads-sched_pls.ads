--  $Id: threads-sched_pls.ads,v 1.2 2008/11/24 16:46:29 baker Exp $

-- deadline polling server scheduler policy plug-in

with Replenishments;
with Aperiodic_Server_Parameters; use Aperiodic_Server_Parameters;
with Threads.Aperiodic_Policies;
package Threads.Sched_PLS is

   type Object is new
     Threads.Aperiodic_Policies.Object with private;

   procedure Bind_Parms
     (P : in out Object;
      Parms : Aperiodic_Server_Parameters.Parameters);
   procedure Init
     (P : in out Object);

   procedure Enter_Scheduler
     (P : in out Object);
   procedure Suspend
     (P : in out Object);
   procedure Go
     (P : in out Object);
   procedure Stop
     (P : in out Object);

private

   package Replenishment_Events is
      type Object is new Events.Object with record
         T : Thread_Ref;
      end record;
      procedure Handler (E : in out Object);
      function Name (E : Object) return String;
   end Replenishment_Events;

   package Budget_Exhaustion_Events is
      type Object is new Events.Object with record
         T : Thread_Ref;
      end record;
      procedure Handler (E : in out Object);
      function Name (E : Object) return String;
   end Budget_Exhaustion_Events;

   type Object is new
     Threads.Aperiodic_Policies.Object with record
        -- parameters
        Parms : Aperiodic_Server_Parameters.Parameters;
        --  events
        Replenishment : aliased Replenishment_Events.Object;
        Exhaustion: aliased Budget_Exhaustion_Events.Object;
        --  for recent budget usage accounting
        Usage : Time;
        Last_Usage_Update_Time : Time;
        --  The most recent time we checked this thread's
        --  budget usage and updated T.P.Usage.
   end record;

end Threads.Sched_PLS;
