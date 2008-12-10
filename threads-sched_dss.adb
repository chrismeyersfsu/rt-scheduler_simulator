---  $Id: threads-sched_dss.adb,v 1.6 2008/11/24 02:05:39 baker Exp baker $

--  deadline sporadic scheduler

--  \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

--  This version needs more work.  (1) ????  It is currently
--  missing the call(s) to Replenishments.Merge_R_Queue
--  (P.R_Queue, Now); that are needed to coalesce chunks.

--  Now that I have added a thread scheduler call-out
--  Enter_Scheduler, which simplified implementation of the
--  polling (PLS) server policy, I think it could be used
--  advantageously to simplify this code also.

--  /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

--  ----------
--  Overview
--  ----------

--  The policy is implemented by call-backs that correspond
--  to events that control server priority changes.
--  For each such event there is a scheduler call-back to this policy.

--  In the comments,  Ds = server deadline.
--  In the code, Ds = -P.T.Priority;

--  In the code, server_period = P.T.Policy_Data.Budget_Interval
--  In the code, server_budget = P.T.Policy_Data.Budge

--  In the DSS paper, this is Tz + server_period, wheren
--  Tz = time at which the current server priority became active.
--  Here, everything is in terms of Ds.


--  -----------
--  Initially
--  ------------

--  Ds = Time'last (corresponds to "undefined" in the paper)
--  This is implemented in procedure Bind.

--  -----------------------
--  Server replenishments
--  -----------------------

--  If the server is ready (has work to do)
--  and Ds = Time'Last, then

--  Ds = current_time + server_period

--  This implemented by Replenishment_Events.Handler().

--  -------------------------------------
--  Arrival of an aperiodic job request
--  -------------------------------------

--  If the server has non-empty budget
--  and Tz is not defined (Ds = Time'Last), then do the same as above.

--  This will be detected by the task's arrival event handler, which
--  will eventually call Unsuspend().

--  ---------------------------------
--  Choice of a new task to execute
--  ---------------------------------

--  Let Dc = the deadline of the new task

--  (1) if Tz is undefined

--  If Dc <= current_time + server_period
--  (Ds = Time'Last)
--  Tz = current_time
--  Ds = current time + server_period

--  (2) if Tz is defined

--  If Dc > current_time + server_period
--  Tz = undefined (Time'Last)
--  Ds = Time'Last

--  If Dc <= current_time + server_period
--  and Ds <= Dc,
--  Ds = Dc - server_period

--  This is implemented by New_Current_Thread().

--  ------------------------------
--  Server priority "activation"
--  ------------------------------

--  Actually, any time the server priority is inactive,
--  or the instant where the server priority becomes active.

--  Coalesce all the available chunks into a single chunk
--  with replenishment time equal to the current time.

--  These events are not easy to detect, since the
--  server priority varies.  Since this is only an
--  optimization to reduce fragmentation, we have some
--  flexibility about when we do it, but
--  the results will differ in replenishment time,
--  which might affect serer performance.

--  This is implemented by New_Current_Thread().

--  ---------------------------------------------
--  Server starts to execute a new budget chunk
--  ---------------------------------------------

--  Tz = chunk replenishment time
--  Ds = chunk.replenishment_time + server_period

--  This is implemented in Budget_Exhaustion_Events.Handler().

--  ---------------------------------
--  Server completes a budget chunk
--  ---------------------------------

--  (1) The amount of time consumed by the server is split off
--  and scheduled for replenishment at the current deadline of
--  the server

--  (2) If there is another chunk, the server executes using that

--  (3) If there are no more chunks, the server becomes inactive

--  This is implemented in Budget_Exhaustion_Events.Handler().

--  -------------------------------------------------
--  Server suspends itself (for an empty job queue)
--  -------------------------------------------------

--  Do the same as (1) above, except that we can't assume the chunk
--  has been exhausted.

--  This is implemented in Suspend().

--  --------------------------------
--  The entire system becomes idle
--  --------------------------------

--  This event is not important for the DSS algorithm, but
--  notification of this event may be needed for some other server
--  algorithms, to merge all server replenishments and set
--  the replenishment amount to full budget.

--  To catch this event, one can use the New_Current_Thread call-out,
--  and check for Current_Thread = Idle_Thread.

with Replenishments; use Replenishments;
with Simulator;
with Error_Log; use Error_Log;
with Ada.Text_IO; use Ada.Text_IO;
package body Threads.Sched_DSS is

   type Policy_Ref is access all Object;

   --  Debugging Support

   procedure Trace_R_Queue
     (Threshold : Integer;
      P : Policy_Ref;
      Msg : String) is
   begin
      if Debug_Level >= Threshold then
         Put (Log, Name (P.T.all));
         Put (Log, Msg);
         Put (Log, " with R_Queue = ");
         Show_R_Queue (P.R_Queue);
         New_Line (Log);
      end if;
   end Trace_R_Queue;

   procedure Trace
     (Threshold : Integer;
      P : Policy_Ref;
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
      P : Policy_Ref;
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
      P : Policy_Ref;
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

   procedure Check_Usage
     (Threshold : Integer;
      Used : Time;
      P : Policy_Ref;
      R : R_Info) is
   begin
      if Debug_Level >= Threshold and then
        Used > R.R_Amount - P.Usage then
         Put_Line (Log, "* used time > budget: used =" & Time'Image (Used)
                & " R.R_Amount =" & Time'Image (R.R_Amount)
                & " P.Usage" & Time'Image (P.Usage)
                & " P.Last_Update_Time" & Time'Image (P.Last_Usage_Update_Time));
         pragma Assert (False);
      end if;
   end Check_Usage;

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
         pragma Debug (Trace (3, P, "replenishment handler (dss)"));
         pragma Assert (P.R_Queue.Front_Of.R_Time = Now);
         --  Update the thread's priority (deadline) and
         --  let it contend for the processor.
         Change_Priority (P.T, Now + P.Parms.Budget_Interval);
         pragma Debug (Trace_Priority (5, P, "replenishment handler"));
         Policy_Unsuspend (E.T);
         Schedule;
      end Handler;

      function Name (E : Object) return String is
      begin
         return E.T.all.Name & "replenishment " & Events.Object (E).Name;
      end Name;

   end Replenishment_Events;

   --------------------
   --  Update_Usage  --
   --------------------

   --  Called for both budget exhaustion (from timer)
   --  and for self-suspension, to check for and handle
   --  budget exhaustion.  It also updates the current
   --  budget usage.

   --  So, it may be executed twice in succession, first from the
   --  Budget_Exhaustion event handler, and then again from the
   --  Stop event handler as a consequence of policy-suspension
   --  causing the scheduler to stop the thread.  The second call
   --  should fall through without any action.

   procedure Update_Usage (P : Policy_Ref;
                           Msg : String) is
      Used : Time;
      Now : constant Time := Simulator.Current_Time;
      R, R2 : R_Info;
   begin

      if P.Exhaustion.Enqueued then
         Simulator.Cancel_Event (P.Exhaustion);
         pragma Debug (Trace (5, P, "cancelled exhaustion"));
      end if;

      R := P.R_Queue.Front_Of;
      Used := Now - P.Last_Usage_Update_Time;
      pragma Debug (Check_Usage (3, Used, P, R));
      if Used = 0 then
         pragma Debug (Trace (2, P, "skipping second call to Update_Usage"));
         null;
      else
         --  cancel budget timeout, if one is pending.

         P.Usage := P.Usage + Used;
         P.Last_Usage_Update_Time := Now;
         pragma Debug (Trace_R_Queue (7, P, "last_usage_update_time (1)"));

         --  If thread is suspended, split off the used part of budget.
         if P.T.Is_Suspended then
            pragma Debug (Trace (5, P, "is suspended"));
            P.R_Queue.Pop;
            R2.R_Amount := P.Usage;
            R2.R_Time := P.T.Priority;
            P.R_Queue.Add (R2);
            R.R_Amount := R.R_Amount - P.Usage;
            P.Usage := 0;
            pragma Assert (R.R_Amount >= 0);
            if R.R_Amount = 0 then
               R := P.R_Queue.Front_Of;
               --  Update thread deadline to match new chunk.
               --  Since this can only be later, it cannot alter
               --  a decision to preempt this thread.
               Change_Priority (P.T, R.R_Time + P.Parms.Budget_Interval);
               pragma Debug (Trace (7, P, "starting new budget chunk"));
               pragma Debug (Trace_Priority (5, P, Msg));
            else
               P.R_Queue.Add (R);
            end if;
         elsif P.Usage = R.R_Amount then
            --  update replenishment time of consumed chunk
            P.R_Queue.Pop;
            P.Usage := 0;
            R.R_Time := P.T.Priority;
            P.R_Queue.Add (R);
            R := P.R_Queue.Front_Of;
         else
            pragma Debug (Trace_R_Queue (7, P, "usage < amount?" &
                    " P.Usage =" & Time'Image (P.Usage) &
                    " R.R_Amount =" & Time'Image (R.R_Amount)));
            null;
         end if;

         -- R holds the earliest chunk in P.R_Queue

         pragma Assert (R.R_Amount > 0);

         if R.R_Time > Now and then
            not P.T.Is_Policy_Suspended then
            --  Suspend the server until the replenishment.
            Policy_Suspend (P.T);
            --  Schedule a replenishment event.
            P.Replenishment.Event_Time := R.R_Time;
            Simulator.Schedule_Event (P.Replenishment);
            pragma Debug (Trace_Time (5, P, "next replenishment due", R.R_Time));
         else
            pragma Debug (Trace (5, P, "not policy suspended"));
            null;
         end if;
      end if;

      pragma Debug (Check_R_Sum
                    (P.R_Queue, P.Parms.Budget, Msg));
      pragma Debug (Trace_R_Queue (6, P, "leaving update_usage"));

   end Update_Usage;

   --------------------------------
   --  Budget_Exhaustion_Events  --
   --------------------------------

   package body Budget_Exhaustion_Events is

      procedure Handler (E : in out Object) is
         P : constant Policy_Ref := Policy_Ref (E.T.Policy);
      begin
         pragma Debug (Trace (3, P, "chunk exhaustion handler (dss)"));
         --  Update budget usage, and replenishment info.
         Update_Usage (P, "Budget_Exhaustion");
         if P.T.Is_Policy_Suspended then
            Schedule;
         else
            --  Set up exhaustion event for the new chunk
            P.Exhaustion.Event_Time := Simulator.Current_Time
              + P.R_Queue.Front_Of.R_Amount;
            Simulator.Schedule_Event (P.Exhaustion);
            pragma Debug (Trace_Time (5, P, "chunk will exhaust", P.Exhaustion.Event_Time));
         end if;
      end Handler;

      function Name (E : Object) return String is
      begin
         return Name (E.T.all)
           & "budget_exhaustion " & Events.Object (E).Name;
      end Name;

   end Budget_Exhaustion_Events;

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
      pragma Debug (Trace (9, P'Unchecked_Access, "go handler (dss)"));

      --  Schedule a timer for when the current replenishment
      --  chunk will run out, assuming the thread runs that long.

      P.Exhaustion.Event_Time := Now + (P.R_Queue.Front_Of.R_Amount - P.Usage);
      Simulator.Schedule_Event (P.Exhaustion);
      pragma Debug (Trace_Time (5, P'Unchecked_Access, "chunk will exhaust (Go)", P.Exhaustion.Event_Time));

      P.Last_Usage_Update_Time := Now;
      pragma Debug (Trace_R_Queue (7, P'Unchecked_Access, "last_usage_update_time (2)"));

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
      pragma Debug (Trace (9, P'Unchecked_Access, "stop handler (dss)"));

      --  Cancel budget timeout, Update budget usage, and schedule
      --  replenishments, if necessary.

      Update_Usage (P'Unchecked_Access, "Stop");

   end Stop;

   ------------------
   --  Bind_Parms  --
   ------------------

   procedure Bind_Parms
     (P : in out Object;
      Parms : Aperiodic_Server_Parameters.Parameters) is
   begin
      P.Parms := Parms;
   end Bind_Parms;

   ------------
   --  Init  --
   ------------

   procedure Init
     (P : in out Object) is
   begin
      P.T.Priority :=  Time'Last;
      P.Replenishment.T := P.T;
      P.Exhaustion.T := P.T;
      P.R_Queue.Clear;
      --  set up first replenishment
      P.R_Queue.Add ((R_Time => 0,
                      R_Amount => P.Parms.Budget));
      P.T.Is_In_Ready_Queue := False;
      P.T.Is_Policy_Suspended := False;
      P.Usage := 0;
      P.Last_Usage_Update_Time := Time'Last;
      -- to catch failure to set real value before use
   end Init;

   -------------------------
   --  New_Current_Thread --
   -------------------------

   --  This is called for *ALL* threads :-( whenever the value of
   --  Current_Thread has changed.

   procedure New_Current_Thread
     (P : in out Object) is
      DC : Time;
      DS : Time := P.T.Priority;
      TD : constant Time :=
        Simulator.Current_Time + P.Parms.Budget_Interval;
   begin
      if Current /= null then
         DC := Current.Priority;
      else
         DC := Time'Last;
      end if;
      if DC > TD then
         DS := Time'Last; -- "undefined"
      elsif DS <= DC then
         DS := Time'Min (DC, TD);
      end if;
      Change_Priority (P.T, DS);
      pragma Debug (Trace_Priority (5, P'Unchecked_Access, "new_current_thread"));
   end New_Current_Thread;

   -----------------
   --  Unsuspend  --
   -----------------

   --  This is called whenever a thread that earlier suspended
   --  itself wakes up. For a server, this means a job has arrived
   --  for a server previously had no jobs in its queue.

   procedure Unsuspend (P : in out Object) is
   begin

      if P.T.Priority = Time'Last then
         Change_Priority (P.T, Simulator.Current_Time + P.Parms.Budget_Interval);
         pragma Debug (Trace_Priority (5, P'Unchecked_Access, "unsuspend"));
      end if;

   end Unsuspend;

   ---------------
   --  Suspend  --
   ---------------

   --  This is called whenever a thread has suspended itself.  For
   --  a server, this means the server has no jobs in its queue.

   procedure Suspend (P : in out Object) is
   begin
      null;
   end Suspend;

end Threads.Sched_DSS;
