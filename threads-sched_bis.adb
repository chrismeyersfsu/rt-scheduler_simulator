-- Baker's Ideal Server
-- Implemented by Michael Serritella
-- COP5642
-- Project 4
-- 12/7/08
------------------------------------

with Replenishments; use Replenishments;
with Simulator;
with Error_Log; use Error_Log;
with Ada.Text_IO; use Ada.Text_IO;
package body Threads.Sched_BIS is

   type Policy_Ref is access all Object;

   --  Debugging Support

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

   ---------------------
   --  t* Generation  --
   ---------------------

   function tStar (P : Object) return Time is
      Now : constant Time := Simulator.Current_Time;
   begin
      if Current = null or else Current.Priority <= (Now + P.Ps) then
         return Now + 2*(P.Ps);
      else
         return Now + P.Ps;
      end if;
   end tStar;

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
   begin

      if P.Exhaustion.Enqueued then
         Simulator.Cancel_Event (P.Exhaustion);
         pragma Debug (Trace (5, P, "cancelled exhaustion"));
      end if;

      Used := Now - P.Last_Usage_Update_Time;
      if Used = 0 then
         pragma Debug (Trace (2, P, "skipping second call to Update_Usage"));
         null;
      else
         --  cancel budget timeout, if one is pending.

         P.Usage := P.Usage + Used;
         P.Last_Usage_Update_Time := Now;

         --  If the budget is expended, replenish immediately and set a far-out
         --  deadline
         if P.Usage >= P.Qs then
            P.Usage := 0;
            P.Exhaustion.Event_Time := Now + P.Qs;
            Change_Priority (P.T, P.T.Priority + P.Ps);
            Simulator.Schedule_Event (P.Exhaustion);
            pragma Debug (Trace_Time (5, P, "chunk will exhaust", P.Exhaustion.Event_Time));
         end if;

      end if;

   end Update_Usage;

   --------------------------------
   --  Budget_Exhaustion_Events  --
   --------------------------------

   package body Budget_Exhaustion_Events is

      procedure Handler (E : in out Object) is
         P : constant Policy_Ref := Policy_Ref (E.T.Policy);
         Now : constant Time := Simulator.Current_Time;
      begin
         pragma Debug (Trace (3, P, "chunk exhaustion handler (dss)"));
         --  Update budget usage, and replenishment info.
         Update_Usage (P, "Budget_Exhaustion");
         if P.T.Is_Policy_Suspended then
            Schedule;
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
      pragma Debug (Trace (9, P'Unchecked_Access, "go handler (bis)"));

      --  Schedule a timer for when the current replenishment
      --  chunk will run out, assuming the thread runs that long.

      if P.Exhaustion.Enqueued then
         Simulator.Cancel_Event (P.Exhaustion);
         pragma Debug (Trace (5, P'Unchecked_Access, "cancelled exhaustion (hooha)"));
      end if;

      P.Exhaustion.Event_Time := Now + (P.Qs - P.Usage);
      Simulator.Schedule_Event (P.Exhaustion);
      pragma Debug (Trace_Time (5, P'Unchecked_Access, "chunk will exhaust (Go)", P.Exhaustion.Event_Time));

      P.Last_Usage_Update_Time := Now;

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
      pragma Debug (Trace (9, P'Unchecked_Access, "stop handler (bis)"));

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
      P.Qs := Parms.Budget;
      P.Ps := Parms.Budget_Interval;
   end Bind_Parms;

   ------------
   --  Init  --
   ------------

   procedure Init
     (P : in out Object) is
   begin
      P.T.Priority :=  Time'Last;
      P.Exhaustion.T := P.T;
      P.T.Is_In_Ready_Queue := False;
      P.T.Is_Policy_Suspended := False;
      P.Usage := 0;
      P.Last_Usage_Update_Time := Time'Last;
      -- to catch failure to set real value before use
   end Init;

   -----------------
   --  Unsuspend  --
   -----------------

   --  This is called whenever a thread that earlier suspended
   --  itself wakes up. For a server, this means a job has arrived
   --  for a server previously had no jobs in its queue.

   procedure Unsuspend (P : in out Object) is
   begin
      if P.T.Priority = Time'Last then
         P.Usage := 0;
         -- If Go is called next, it will take care of the rest, like setting
         -- the last usage check time to Now

         Change_Priority (P.T, P.tStar + P.Ps);
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
      --  Represent our budget as being completely expended
      P.Usage := P.Qs;
   end Suspend;

end Threads.Sched_BIS;
