--  $Id: threads-sched_bgs.ads,v 1.1 2008/11/24 02:05:39 baker Exp $

-- background scheduler policy plug-in

with Replenishments;
with Aperiodic_Server_Parameters; use Aperiodic_Server_Parameters;
with Threads.Aperiodic_Policies;
package Threads.Sched_BGS is

   type Object is new
     Threads.Aperiodic_Policies.Object with private;

   procedure Bind_Parms
     (P : in out Object;
      Parms : Aperiodic_Server_Parameters.Parameters);
   procedure Init
     (P : in out Object);

private

   type Object is new
     Threads.Aperiodic_Policies.Object with record
        -- parameters
        Parms : Aperiodic_Server_Parameters.Parameters;
        --  replenishment queue
   end record;

end Threads.Sched_BGS;
