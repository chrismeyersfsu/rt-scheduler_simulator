--  $Id: threads.adb,v 1.8 2008/11/24 02:05:39 baker Exp baker $

--  The core of the logic is in procedure Schedule.  If you want
--  to understand, that is the best place to start reading.

--  See Threads.Sched_DSS (file threads-sched_dss.adb) for an
--  example of the detailed work of a scheduling policy.

with Simulator;
with Error_Log; use Error_Log;  -- for file Log
with Ada.Text_IO; use Ada.Text_IO;  -- for file Log
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
package body Threads is

   --  debugging tools

   Scheduler_Active : Boolean := False;

   procedure Trace_Context_Switch
     (Threshold : Integer;
      Current, Top : Thread_Ref) is
   begin
      if Debug_Level >= Threshold then
         Put (Log, "   threads.schedule ");
         if Current = null then
            Put (Log, "null");
         else
            Put (Log, Trim (Current.Name, Right));
         end if;
         Put (Log, " -> ");
         if Top = null then
            Put (Log, "null");
         else Put (Log, Trim (Top.Name, Right));
         end if;
         New_Line (Log);
      end if;
   end Trace_Context_Switch;

   --  ordering relation for thread queues

   function ">" (L, R : Thread_Ref) return Boolean is
   begin
      return L.Priority > R.Priority;
   end ">";

   package body Scheduling_Policies is

      procedure Bind_Thread
        (P : in out Object;
         T : Thread_Ref) is
      begin
         P.T := T;
      end Bind_Thread;

      procedure New_Job
        (P : in out Object;
         J : Jobs.Job) is
      begin
         null;
      end New_Job;

      procedure Suspend
        (P : in out Object) is
      begin
         null;
      end Suspend;

      procedure Unsuspend
        (P : in out Object) is
      begin
         null;
      end Unsuspend;

      procedure Enter_Scheduler
        (P : in out Object) is
      begin
         null;
      end Enter_Scheduler;

      procedure New_Current_Thread
        (P : in out Object) is
      begin
         null;
      end New_Current_Thread;

      procedure Go
        (P : in out Object) is
      begin
         null;
      end Go;

      procedure Stop
        (P : in out Object) is
      begin
         null;
      end Stop;

      procedure Idle
        (P : in out Object) is
      begin
         null;
      end Idle;

   end Scheduling_Policies;

   procedure Bind_Policy
     (T : in out Thread_Ref;
      P : Policies_Class_Ref) is
   begin
      T.Policy := P;
   end Bind_Policy;

   procedure Init (T : Thread_Ref) is
   begin
      T.Is_Suspended := True;
      T.Is_Policy_Suspended := False;
      T.Is_In_Ready_Queue := False;
      T.Policy.Init;
   end Init;

   procedure Reset_All is new
     Threads_Queues.For_All (Init);

   procedure Initialize is
   begin
      Current := Idle_Thread;
      Total_Idle := 0;
      Last_Idle_Time := Simulator.Current_Time;
      Ready_Queue.Clear;
      Policy_Suspended_Queue.Clear;
      Reset_All (All_Threads);
      Scheduler_Active := False;
   end Initialize;

   function New_Thread
     (Go : Events.Class_Ref;
      Stop : Events.Class_Ref;
      Name : String)
     return Thread_Ref is
      T : Thread_Ref;
   begin
      T := new Thread;
      T.Go := Go;
      T.Stop := Stop;
      T.Name := Name (Name'First .. Name'First + T.Name'Length - 1);
      All_Threads.Add (T);
      return T;
   end New_Thread;

   --  The following provides a mechanism for notifying all
   --  threads of a context-switch event.  This is potentially
   --  very inefficient, but it might be necessary for some
   --  scheduling policies. With restricted visibility of
   --  information, this may be needed to provide early
   --  replenishment to multiple server threads if the system
   --  becomes idle, for example.

   --  ??? Consider keeping a separate list of threads for
   --  policies that require this notification, to avoid the
   --  overhead of processing the other threads?  Or, find some
   --  even better way to reduce overhead?

   procedure Notify_NCT (T : Thread_Ref) is
   begin
      T.Policy.New_Current_Thread;
   end Notify_NCT;

   procedure Notify_All_NCT is
      new Threads_Queues.For_All (Notify_NCT);

   procedure Notify_Idle (T : Thread_Ref) is
   begin
      T.Policy.Idle;
   end Notify_Idle;

   procedure Notify_All_Idle is
      new Threads_Queues.For_All (Notify_Idle);

   procedure Schedule is
      Top : Thread_Ref;
      Now : constant Time := Simulator.Current_Time;
   begin

      pragma Assert (not Scheduler_Active);
      Scheduler_Active := True;

      if Current /= null then
         Current.Policy.Enter_Scheduler;
      end if;

      <<Restart_Schedule>>

      --  find highest priority thread

      if Ready_Queue.Is_Empty then
          --  idle system
          Top := Idle_Thread;
          --  this is an ideal time to terminate simulation
          if Policy_Suspended_Queue.Is_Empty then
             Simulator.Check_Time;
          end if;
      else
         Top := Ready_Queue.Front_Of;
      end if;

      if Top = Current then
         pragma Debug (Trace_Context_Switch (5, Current, Top));
         Scheduler_Active := False;
         return;
      end if;

      if Current = Idle_Thread then
         --  transition from idle to non-idle system
         Total_Idle := Total_Idle + (Now - Last_Idle_Time);
         --  ??? consider giving Idle_Thread a thread
         --  and task object, so that we
         --  can let Tasks track idle time?
      else
         Current.Stop.Event_Time := Now;
         --  Tell policy plug-in that this thread is preempted or
         --  suspended.
         Current.Policy.Stop;
         --  Tell task that is has stopped.
         Current.Stop.Handler;
      end if;

      pragma Debug (Trace_Context_Switch (5, Current, Top));

      -- ""context switch"
      Current := Top;

      --  Notify interested schedulers of the context switcht.
      --  This gives them a chance to replenish budgets etc.  if
      --  the system is becoming idle or if it has just dropped to
      --  a lower priority level.

      Notify_All_NCT (All_Threads);

      --  ???  Can we do without the above?  Most or all of them
      --  might not be interested!  This is a price we seem to be
      --  paying for modularization.  ???  A real kernel scheduler
      --  could not afford this kind of inefficient algorithm.
      --  Think of a better way for schedulers to get the
      --  information they need.

      if Current = Idle_Thread then

         --  Make the transistion to an idle system.

         --  In case there are any scheduling policies that
         --  require action in this case, we need provide
         --  the following call-out.

         Notify_All_Idle (All_Threads);

         --  ??? Like Notify_All_NCT, the above is inefficient,
         --  but it is not quite so bad here, since it only happens
         --  when the system would otherwise be idle.

         --  If a policy made some thread ready that
         --  was not previously ready, we need to restart
         --  the scheduler here, since it is not safe for
         --  the policy to call the scheduler directly.

         if not Ready_Queue.Is_Empty then
            goto Restart_Schedule;
         end if;

         Last_Idle_Time := Now;
      else
         Current.Go.Event_Time := Now;
         Current.Policy.Go;
         Current.Go.Handler;
      end if;

      Scheduler_Active := False;

   end Schedule;

   procedure New_Job (T : Thread_Ref;
                      J : Jobs.Job) is
   begin
      T.Policy.New_Job (J);
   end New_Job;

   procedure Suspend (T : Thread_Ref) is
   begin
      pragma Debug (Trace (2, Name (T.all) & "threads.suspend "));
      pragma Assert (not T.Is_Suspended);
      T.Is_Suspended := True;
      if T.Is_In_Ready_Queue then
         Ready_Queue.Delete (T);
         T.Is_In_Ready_Queue := False;
      else
         Policy_Suspended_Queue.Delete (T);
      end if;
      --  call out to policy to indicate suspension
      T.Policy.Suspend;
   end Suspend;

   procedure Unsuspend (T : Thread_Ref) is
   begin
      pragma Debug (Trace (2, Name (T.all) & "threads.unsuspend "));
      pragma Assert (T.Is_Suspended);
      T.Is_Suspended := False;
      --  call out to policy, to possibly recompute priority
      T.Policy.Unsuspend;
      if not T.Is_Policy_Suspended then
         Ready_Queue.Add (T);
         T.Is_In_Ready_Queue := True;
      else
         Policy_Suspended_Queue.Add (T);
      end if;
   end Unsuspend;

   procedure Policy_Suspend (T : Thread_Ref) is
   begin
      pragma Debug (Trace (2, Name (T.all) & "threads.policy_suspend"));
      pragma Assert (not T.Is_Policy_Suspended);
      T.Is_Policy_Suspended := True;
      if T.Is_In_Ready_Queue then
         Ready_Queue.Delete (T);
         T.Is_In_Ready_Queue := False;
         Policy_Suspended_Queue.Add (T);
      end if;
   end Policy_Suspend;

   procedure Policy_Unsuspend
     (T : Thread_Ref) is
   begin
      pragma Debug (Trace (2, Name (T.all) & "threads.policy_unsuspend"));
      pragma Assert (T.Is_Policy_Suspended);
      pragma Assert (not T.Is_In_Ready_Queue);
      T.Is_Policy_Suspended := False;
      if not T.Is_Suspended then
         Ready_Queue.Add (T);
         T.Is_In_Ready_Queue := True;
         Policy_Suspended_Queue.Delete (T);
      end if;
   end Policy_Unsuspend;

   procedure Policy_Unsuspend  -- obsolete
     (T : Thread_Ref;
      New_Priority : Time) is
   begin
      pragma Debug (Trace (2, Name (T.all) & "threads.policy_unsuspend"));
      pragma Assert (New_Priority < Time'Last);
      pragma Assert (T.Is_Policy_Suspended);
      pragma Assert (not T.Is_In_Ready_Queue);
      T.Is_Policy_Suspended := False;
      if not T.Is_Suspended then
         T.Priority := New_Priority;
         Ready_Queue.Add (T);
         T.Is_In_Ready_Queue := True;
      end if;
   end Policy_Unsuspend;

   function Name (T : Thread) return String is
   begin
      return Trim (T.Name, Right) & ' ';
   end Name;

   function Total_Idle_Time return Time is
   begin
      return Total_Idle;
   end Total_Idle_Time;

end Threads;
