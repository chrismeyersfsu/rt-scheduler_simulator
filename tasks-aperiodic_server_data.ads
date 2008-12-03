with Events;
with Threads; use Threads;
with Replenishments;
with Virtual_Times; use Virtual_Times;
package Tasks.Aperiodic_Server_Data is
   type Policy_Data is new Threads.Scheduling_Policies_State.Object with record
      R_Queue : Replenishments.Queues.Object;
      Remaining_Budget : Time;
   end record;
   type Policy_Data_Ref is access all Policy_Data;
   type Parameters is new Threads.Scheduling_Parameters.Object with record
      Budget : Time;
      Budget_Interval : Time;
   end record;
end Tasks.Aperiodic_Server_Data;
