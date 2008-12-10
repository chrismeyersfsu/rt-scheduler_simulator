-- Baker's Ideal Server
-- Implemented by Michael Serritella
-- COP5642
-- Project 4
-- 12/7/08
------------------------------------

with Replenishments;
with Aperiodic_Server_Parameters; use Aperiodic_Server_Parameters;
with Threads.Aperiodic_Policies;
package Threads.Sched_BIS is

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

private

   package Budget_Exhaustion_Events is
      type Object is new Events.Object with record
         T : Thread_Ref;
      end record;
      procedure Handler (E : in out Object);
      function Name (E : Object) return String;
   end Budget_Exhaustion_Events;

   function tStar (P : Object) return Time;

   type Object is new
     Threads.Aperiodic_Policies.Object with record
        -- parameters
        Parms : Aperiodic_Server_Parameters.Parameters;
        -- execution/budget quantum (basically a copy/alias of a param)
        Qs : Time;
        -- period of the server (basically a copy/alias of a param)
        Ps : Time;
        --  events
        Exhaustion: aliased Budget_Exhaustion_Events.Object;
        --  for recent budget usage accounting
        Usage : Time;
        --  The amount of the current chunk that has
        --  been used.  Invariant:
        --  P.Usage <= P.R_Queue.Front_Of.R_Amount.
        Last_Usage_Update_Time : Time;
        --  The most recent time we checked this thread's
        --  budget usage and updated T.P.Usage.
   end record;

end Threads.Sched_BIS;
