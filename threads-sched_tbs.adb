---  $Id: threads-sched_pls.adb,v 1.2 2008/11/24 15:53:04 baker Exp baker $

--  polling server scheduler

with Simulator;
with Error_Log; use Error_Log;
with Ada.Text_IO; use Ada.Text_IO;
package body Threads.Sched_TBS is

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

 
   ----------
   --  Go  --
   ----------

   --  This is called whenever Current_Thread is set to a new
   --  value, for the new thread.  It indicates that the
   --  thread has just started or resumed executing, after
   --  a suspension or preemption.

   procedure Go
     (P : in out Object) is
      Now : Time := Simulator.Current_Time;
   begin
      pragma Debug (Trace (9, P, "go handler (pls)"));
      Now := Simulator.Current_Time;
      --  Schedule a timer for when the current replenishment
      --  chunk will run out, assuming the thread runs that long.

--      P.Exhaustion.Event_Time := Now + (P.Parms.Budget - P.Usage);
--      Simulator.Schedule_Event (P.Exhaustion);
--      pragma Debug (Trace_Time (5, P,
--                                "chunk will exhaust (Go)", P.Exhaustion.Event_Time));
--      P.Last_Usage_Update_Time := Now;
--      pragma Debug (Trace (7, P, "last_usage_update_time (2)"));

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
      Now : Time := Simulator.Current_Time;
   begin
      pragma Debug (Trace (9, P, "stop handler (pls)"));
      Now := Simulator.Current_Time;
      --  Cancel budget timeout, since this task is no longer running.

--      if P.Exhaustion.Enqueued then
--         Simulator.Cancel_Event (P.Exhaustion);
--         pragma Debug (Trace (5, P, "cancelled exhaustion"));
--      end if;

   end Stop;

   ------------------
   --  Bind_Parms  --
   ------------------

   procedure Bind_Parms
     (P : in out Object;
      Parms : Aperiodic_Server_Parameters.Parameters) is
   begin
      P.Parms := Parms;
      p.Server_Utilization := Float (Float (P.Parms.Budget) / Float (P.Parms.Budget_Interval));
      pragma Assert (Parms.Budget < Parms.Budget_Interval);
   end Bind_Parms;

   ------------
   --  Init  --
   ------------

   procedure Init
     (P : in out Object) is
   begin
      P.T.Priority :=  Time'Last;
--      P.Completion.T := P.T;
      P.T.Is_In_Ready_Queue := False;
--      Policy_Suspend (P.T);
   end Init;

   ---------------
   --  Suspend  --
   ---------------

   --  This is called whenever a thread has suspended itself.  For
   --  a server, this means the server has no jobs in its queue.

   procedure Suspend (P : in out Object) is
      Now : Time := Simulator.Current_Time;
   begin
      pragma Debug (Trace (9, P, "suspend handler (pls)"));
      Now := Simulator.Current_Time;

      --  Suspend the server until the next replenishment.
--      Put("Chris: Suspend() called");
--	Put("");
--      if not P.T.Is_Policy_Suspended then
--         Policy_Suspend (P.T);
--      end if;

   end Suspend;

   -----------------
   --  Unsuspend  --
   -----------------

   --  This is called whenever a thread that earlier suspended
   --  itself wakes up. For a server, this means a job has arrived
   --  for a server previously had no jobs in its queue.

   procedure Unsuspend (P : in out Object) is
      Now : Time := Simulator.Current_Time;
   begin
       pragma Debug (Trace_Priority (9, P, "unsuspend"));
      Now := Simulator.Current_Time;
--      Put("Chris: Unsuspend() called");
--	Put("");
--      if P.T.Priority = Time'Last then
--         P.T.Priority := Simulator.Current_Time + P.Parms.Budget_Interval;
--           Put("Priority HUUUUUUGE");
--      end if;

   end Unsuspend;

-- Job arrives to empty server, set new deadline
-- Job completes and new job is selected (new job is J), set new deadline
   procedure New_Job(P : in out Object; J : in Jobs.Job) is
	Max : Time;
   begin
-- I would like to actually change J.Absolute_Deadline here
-- but J is immutable (in but not out)
--      	P.T.Priority := J.Absolute_Deadline;

	if P.T.Priority = Time'Last then
		Max := J.Arrival_time;
	elsif J.Arrival_time >= P.T.Priority then
		Max := J.Arrival_time;
	else
		Max := P.T.Priority;
	end if;

	-- Round down instead of - 1
	-- Not sure how ADA casts an Integer to a Float.
        pragma Debug(Trace_Priority (1, P, "New_Job"));	
	P.T.Priority := Max + Time ( Float ( Float (J.Execution_Time) / P.Server_Utilization)) - 1;

-- This is a bit tricky
-- Since we updated the Tasks Priority we need to take it off the Schedulers Queue
-- and put it back on to update the tasks location (priority) in the priority queue.
-- Again, this is because the priority queue only looks at the priority of the task
-- when the task is placed on the queue.  If this priority changes, which it does in
-- this case, then we must take it off the queue and place it back on.
	if not P.T.Is_Policy_Suspended then
		Policy_Suspend(P.T);
		Policy_Unsuspend(P.T);
	end if;
        pragma Debug(Trace_Priority (1, P, "New_Job"));
   end New_Job;

--   procedure Idle(P : in out Object) is

--   begin
--	P.T.Priority := Time'Last;
--        Put("");
--	P.T.Priority := P.T.Priority;
--   end Idle;

--   package body Job_Completion_Events is
	
--	procedure Handler (E : in out Object) is
--      	   Now : Time := Simulator.Current_Time;
--	begin
--      	   Now := Simulator.Current_Time;
--	   Handler(Object (E.T));
--	end Handler;

--   end Job_Completion_Events;

--   procedure New_Job (P: in out Object; J : in out Jobs.Job) is
--	Max : Time;
--   begin

-- 	if J.Arrival_time >= P.Current_Job.Absolute_Deadline then
--	  Max := J.Arrival_Time;
--	else
--	  Max := P.Current_Job.Absolute_Deadline;
--	end if;

--	J.Absolute_Deadline := Max + (J.Execution_Time / (1/4));

--   end New_Job;

--   package body Job_Arrival_Events is

--	procedure Handler (E : in out Object) is
--           P : constant Policy_Ref := Policy_Ref (E.T.Policy);
--           Now : Time := Simulator.Current_Time;
--	   J : Jobs.Job;
--	   Max : Time;
--	begin
--      	   Now := Simulator.Current_Time;

--	   if J.Arrival_Time >= P.T.Current_Job.Absolute_Deadline then
--	     Max := J.Arrival_Time;
--	   else
--	     Max := P.T.Current_Job.Absolute_Deadline;
--	   end if;
	   
--		
--	   Handler (E.T.all);	-- Call 'parent' (Task) Handler method/function
--	end Handler;

--   end Job_Arrival_Events;


end Threads.Sched_TBS;
