---  $Id: threads-sched_pls.adb,v 1.2 2008/11/24 15:53:04 baker Exp baker $

--  polling server scheduler

with Replenishments; use Replenishments;
with Simulator;
with Error_Log; use Error_Log;
with Ada.Text_IO; use Ada.Text_IO;
package body Threads.Sched_PLS is

   type Policy_Ref is access all Object;

   --  Debugging Support

   procedure Trace
     (Threshold : Integer;
      P : in Object;
      Msg : String) is
   begin
      if Debug_Level >= Threshold then
         Put (Log, Name (P.T.all));
         Put (Log, Msg);
         New_Line (Log);
      end if;
   end Trace;

   procedure Trace_Time
     (Threshold : Integer;
      P : in Object;
      Msg : String;
      T : Time) is
   begin
      if Debug_Level >= Threshold then
         Put (Log, Name (P.T.all));
         Put (Log, Msg);
         Put (Log, " at");
         Put (Log, Time'Image (T));
         New_Line (Log);
      end if;
   end Trace_Time;

   procedure Trace_Priority
     (Threshold : Integer;
      P : in Object;
      Msg : String) is
   begin
      if Debug_Level >= Threshold then
         Put (Log, Name (P.T.all));
         Put (Log, "priority ->");
         Put (Log, Time'Image (P.T.Priority));
         Put (Log, " in ");
         Put (Log, Msg);
         New_Line (Log);
      end if;
   end Trace_Priority;

   ----------------------------
   --  Replenishment_Events  --
   ----------------------------

   package body Replenishment_Events is

      procedure Handler (E : in out Object) is
         --  Cast type of policy
         --  to the specific type for this policy.
         P : constant Policy_Ref := Policy_Ref (E.T.Policy);
         Now : constant Time := Simulator.Current_Time;
      begin
         pragma Debug (Trace (3, P.all, "replenishment handler (pls)"));

         --  Schedule the next replenishment.
         P.Replenishment.Event_Time := P.Replenishment.Event_Time +
           P.Parms.Budget_Interval;
         Simulator.Schedule_Event (P.Replenishment);

         --  Replenish it, but only if it has work to do.
         if not P.T.Is_Suspended then
            --  Update the thread's priority (deadline) and
            --  let it contend for the processor.
            P.T.Priority := Now + P.Parms.Budget_Interval;
            pragma Debug (Trace_Priority (5, P.all, "replenishment handler"));
            --  Do the replenishment.
            P.Usage := 0;
            --  Policy unsuspend the thread
            Policy_Unsuspend (E.T);
            Schedule;
         end if;

      end Handler;

      function Name (E : Object) return String is
      begin
         return Name (E.T.all) & "replenishment " & Events.Object (E).Name;
      end Name;

   end Replenishment_Events;

   --------------------------------
   --  Budget_Exhaustion_Events  --
   --------------------------------

   package body Budget_Exhaustion_Events is

      procedure Handler (E : in out Object) is
         P : constant Policy_Ref := Policy_Ref (E.T.Policy);
      begin
         pragma Debug (Trace (3, P.all, "chunk exhaustion handler (pls)"));
         Schedule;
      end Handler;

      function Name (E : Object) return String is
      begin
         return Name (E.T.all)
           & "budget_exhaustion " & Events.Object (E).Name;
      end Name;

   end Budget_Exhaustion_Events;

   -----------------------
   --  Enter_Scheduler  --
   -----------------------

   --  Enter_Scheduler is called whenever scheduler is
   --  entered from a non-idle thread.  P corresponds to
   --  Current, the currently executing task.

   procedure Enter_Scheduler
     (P : in out Object) is
      Used : Time;
      Now : constant Time := Simulator.Current_Time;
   begin
      pragma Debug (Trace (9, P, "enter scheduler (pls)"));

      --  Update the cpu time usage of the curent thread,
      --  and check for budget exhaustion.

      Used := Now - P.Last_Usage_Update_Time;
      P.Usage := P.Usage + Used;
      P.Last_Usage_Update_Time := Now;
      pragma Assert (P.Usage <= P.Parms.Budget);

      if P.Usage = P.Parms.Budget then
         --  budget is all consumed
         if not P.T.Is_Policy_Suspended then
            --  Suspend the server until the replenishment.
            Policy_Suspend (P.T);
         end if;
      end if;

   end Enter_Scheduler;

   ----------
   --  Go  --
   ----------

   --  This is called whenever Current_Thread is set to a new
   --  value, for the new thread.  It indicates that the
   --  thread has just started or resumed executing, after
   --  a suspension or preemption.

   procedure Go
     (P : in out Object) is
      Now : constant Time := Simulator.Current_Time;
   begin
      pragma Debug (Trace (9, P, "go handler (pls)"));

      --  Schedule a timer for when the current replenishment
      --  chunk will run out, assuming the thread runs that long.

      P.Exhaustion.Event_Time := Now + (P.Parms.Budget - P.Usage);
      Simulator.Schedule_Event (P.Exhaustion);
      pragma Debug (Trace_Time (5, P,
                                "chunk will exhaust (Go)", P.Exhaustion.Event_Time));
      P.Last_Usage_Update_Time := Now;
      pragma Debug (Trace (7, P, "last_usage_update_time (2)"));

   end Go;

   ------------
   --  Stop  --
   ------------

   --  This is called whenever Current_Thread is set to a new
   --  value, for the old current thread.  It indicates that the
   --  thread has been self-suspended, preempted, or
   --  policy-suspended.

   procedure Stop
     (P : in out Object) is
   begin
      pragma Debug (Trace (9, P, "stop handler (pls)"));

      --  Cancel budget timeout, since this task is no longer running.

      if P.Exhaustion.Enqueued then
         Simulator.Cancel_Event (P.Exhaustion);
         pragma Debug (Trace (5, P, "cancelled exhaustion"));
      end if;

   end Stop;

   ------------------
   --  Bind_Parms  --
   ------------------

   procedure Bind_Parms
     (P : in out Object;
      Parms : Aperiodic_Server_Parameters.Parameters) is
   begin
      P.Parms := Parms;
      pragma Assert (Parms.Budget < Parms.Budget_Interval);
   end Bind_Parms;

   ------------
   --  Init  --
   ------------

   procedure Init
     (P : in out Object) is
   begin
      P.Usage := 0;
      P.T.Priority :=  Time'Last;
      P.Replenishment.T := P.T;
      P.Exhaustion.T := P.T;
      --  set up first replenishment, at time zero
      P.Replenishment.Event_Time := 0;
      Simulator.Schedule_Event (P.Replenishment);
      P.T.Is_In_Ready_Queue := False;
      Policy_Suspend (P.T);
      --  to catch failure to set real value before use
      P.Last_Usage_Update_Time := Time'Last;
   end Init;

   ---------------
   --  Suspend  --
   ---------------

   --  This is called whenever a thread has suspended itself.  For
   --  a server, this means the server has no jobs in its queue.

   procedure Suspend (P : in out Object) is
   begin
      pragma Debug (Trace (9, P, "suspend handler (pls)"));

      --  Suspend the server until the next replenishment.

      if not P.T.Is_Policy_Suspended then
         Policy_Suspend (P.T);
      end if;

   end Suspend;

end Threads.Sched_PLS;
