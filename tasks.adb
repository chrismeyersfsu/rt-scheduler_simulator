--  $Id: tasks.adb,v 1.7 2008/11/24 02:05:39 baker Exp $

with Simulator;
with Threads;
with Generic_Queues;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Error_Log; use Error_Log;
package body Tasks is

   ----------------
   --  All_Tasks --
   ----------------

   --  A list of all the tasks we have bound.  This is convenient
   --  for (re) initializing all the tasks.
   --  The order is not important.

   function ">" (L, R : Task_Ref) return Boolean is
   begin
      return L.Name > R.Name;
   end ">";
   package Task_Queues is new Generic_Queues (Task_Ref);

   All_Tasks : Task_Queues.Object;

   --  to catch "recursive" event handling
   Handling_Event : Integer := 0;

   procedure Up (Level : Integer; Msg : String) is
   begin
      Trace (11,
             Integer'Image (Handling_Event) &
               "->" & Integer'Image (Level) & ' ' & Msg);
      pragma Assert (Handling_Event < Level);
      Handling_Event := Handling_Event + 1;
   end Up;

   procedure Down is
   begin
      Trace (11, "<-");
      Handling_Event := Handling_Event - 1;
   end Down;

   ------------
   --  Bind  --
   ------------

   procedure Bind
     (T : Task_Ref;
      M : Workload_Models.Class_Ref;
      P : Threads.Policies_Class_Ref;
      Name : String) is
      use Threads;
   begin
      T.Model := M;
      T.Arrival.T := T;
      T.Completion.T := T;
      --  Set the ordinals to force arrivals to be simulated after
      --  completions, but leave some room in between for use by
      --  extensions.
      T.Completion.Ordinal := 0;
      T.Arrival.Ordinal := 2;
      --  We don't need ordinals on the Stop and Go events,
      --  because they are not "real" events.
      --  They are just a convenient way for the thread
      --  scheduler to signal back to the task when it is
      --  to stop and go.
      T.Go.T := T;
      T.Stop.T := T;
      Move (Trim (Name, Right), T.Name,
            Drop => Right, Justify => Left, Pad => ' ');
      if T.Thread = null then
         T.Thread := Threads.New_Thread
           (Go => T.Go'Unchecked_Access,
            Stop => T.Stop'Unchecked_Access,
            Name => T.Name);
      end if;
      Threads.Bind_Policy (T.Thread, P);
      P.Bind_Thread (T.Thread);
      if not T.Is_In_All_Tasks then
         All_Tasks.Add (T);
         T.Is_In_All_Tasks := True;
      end if;
   end Bind;

   -------------------
   --  Reset_Stats  --
   -------------------

   procedure Reset_Stats_Once
     (T : Task_Ref) is
   begin
      T.Stats_Data :=
        (Max_Resp_Time => 0,
         Total_Resp_Time => 0.0,
         Total_Sq_Resp_Time => 0.0,
         Total_Exec_Time => 0,
         Job_Count => 0,
         Missed_Deadlines => 0);
   end Reset_Stats_Once;

   procedure Reset_Queue is
      new Task_Queues.For_All (Reset_Stats_Once);
   procedure Reset_Stats is
   begin
      Reset_Queue (All_Tasks);
   end Reset_Stats;

   ------------
   --  Init  --
   ------------

   procedure Init
     (T : Task_Ref) is
      use Threads;
   begin
      T.Running := False;
      Reset_Stats_Once (T);
      T.Current_Job :=
        (Arrival_Time => 0,
         Execution_Time => 0,
         Absolute_Deadline => 0);
      T.Remaining_Exec_Time := 0;
      T.Total_Exec_Time := 0;
      T.Pending_Jobs.Clear;
      --  We don't need to initialize T.Thread here
      --  since Initialize will call Threads.Initialize,
      --  and that will (re)initialize all threads.
      T.Arrival.Event_Time := T.Model.Start_Time;
      pragma Debug (Trace (3, Name (T.all) &  "scheduling"));
      Simulator.Schedule_Event (T.Arrival);
      pragma Debug (Trace (3, Name (T.all) &  "scheduled"));
   end Init;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize_Queue is new Task_Queues.For_All (Init);
   procedure Initialize is
   begin
      Handling_Event := 0;
      Threads.Initialize;
      Initialize_Queue (All_Tasks);
   end Initialize;

   -------------------------
   --  Do_For_Every_Task  --
   -------------------------

   --   generic
   --     with procedure P (Item: in Task_Ref);
   procedure Do_For_Every_Task is
      procedure Q is new Task_Queues.For_All (P);
   begin
      Q (All_Tasks);
   end Do_For_Every_Task;

   -----------------
   --  Start_Job  --
   -----------------

   procedure Start_Job (T : in out Task_Object) is
      Now : constant Time := Simulator.Current_Time;
   begin
      pragma Assert (not T.Pending_Jobs.Is_Empty);
      T.Current_Job := T.Pending_Jobs.Front_Of;
      T.Remaining_Exec_Time := T.Current_Job.Execution_Time;
      --  set up completion event in simulator
      Threads.New_Job (T.Thread, T.Current_Job);
      --  allows the scheduling policy
      --  to recompute T's priority, if necessary
      --  e.g., to the absolute deadline for simple EDF
      pragma Debug (Trace (3,
         Name (T) & "starts job that arrived at"
         & Time'Image (T.Current_Job.Arrival_Time)
         & " with execution time"
         & Time'Image (T.Current_Job.Execution_Time)));
   end Start_Job;

   ----------------
   --  Arrivals  --
   ----------------

   package body Job_Arrival_Events is

      procedure Handler (E : in out Object) is
         Now : constant Time := Simulator.Current_Time;
         J : Jobs.Job;
         Busy : Boolean;
      begin
         pragma Debug (Up (1, "Arrival"));
         pragma Assert (E = E.T.Arrival);
         pragma Assert (Now = E.Event_Time);
         --  get job parameters
         E.T.Model.Arrive
           (T => Now,
            J => J,
            Next_Arrival_Time => E.T.Arrival.Event_Time);
         --  set up next arrival in simulator
         Simulator.Schedule_Event (E.T.Arrival);
         --  make this task busy if job is arriving to an empty work queue
         Busy := not E.T.Pending_Jobs.Is_Empty;
         E.T.Pending_Jobs.Add (J);
         if not Busy then
            Start_Job (E.T.all);
            Threads.Unsuspend (E.T.Thread);
            Threads.Schedule;
         end if;
         pragma Debug (Down);
      end Handler;

      function Name (E : Object) return String is
      begin
         return Name (E.T.all)
           & "arrival " & Events.Object (E).Name;
      end Name;

   end Job_Arrival_Events;

   -------------------
   --  Completions  --
   -------------------

   package body Job_Completion_Events is

      procedure Handler (E : in out Object) is
         Now : constant Time := Simulator.Current_Time;
         RET : Time renames E.T.Remaining_Exec_Time;
         Elapsed_Time : Time;
      begin
         pragma Debug (Up (1, "Completion"));
         pragma Assert (E = E.T.Completion);
         --  update remaining execution time
         Elapsed_Time := Now - E.T.Last_Time_Check;
         RET := RET - Elapsed_Time;
         E.T.Last_Time_Check := Now;
         pragma Assert (RET = 0);
         --  track total time, for assertion-checking
         E.T.Total_Exec_Time :=
           E.T.Total_Exec_Time + Elapsed_Time;
         --  record statistics for this job
         Stats.End_Of_Job_Update
           (D => E.T.Stats_Data,
            RT => Now - E.T.Current_Job.Arrival_Time,
            Deadline => E.T.Current_Job.Absolute_Deadline,
            Exec_Time => E.T.Current_Job.Execution_Time,
            Now => Now);
         --  Remove it from the job queue.
         E.T.Pending_Jobs.Pop;
         if E.T.Pending_Jobs.Is_Empty then
            --  suspend this task if there are no more jobs
            Threads.Suspend (E.T.Thread);
         else
            --  otherwise, start the next job
            Start_Job (E.T.all);
            --  set up completion event in simulator
            E.T.Completion.Event_Time :=
              Now + E.T.Remaining_Exec_Time;
            Simulator.Schedule_Event (E.T.Completion);
         end if;
         Threads.Schedule;
         pragma Debug (Down);
      end Handler;

      function Name (E : Object) return String is
      begin
         return  Name (E.T.all) & "completion " & Events.Object (E).Name;
      end Name;

   end Job_Completion_Events;

   ---------------------
   --  Go Events  --
   ---------------------

   --  should not call Threads.Schedule, since
   --  these events are triggered by the thread
   --  scheduler

   package body Go_Events is

      procedure Handler (E : in out Object) is
         Now : constant Time := Simulator.Current_Time;
      begin
         pragma Debug (Up (2, "Go"));
         pragma Debug (Trace (3, Name (E)));
         pragma Assert (E = E.T.Go);
         pragma Assert (not E.T.Running);
         E.T.Running := True;
         E.T.Last_Time_Check := Now;
         --  set up completion event in simulator
         E.T.Completion.Event_Time :=
           Now + E.T.Remaining_Exec_Time;
         Simulator.Schedule_Event (E.T.Completion);
         pragma Debug (Down);
      end Handler;

      function Name (E : Object) return String is
      begin
         return Name (E.T.all) & "go " & Events.Object (E).Name;
      end Name;

   end Go_Events;

   ---------------------
   --  Stop Events  --
   ---------------------

   --  should not call Threads.Schedule, since
   --  these events are triggered by the thread
   --  scheduler

   package body Stop_Events is

      procedure Handler (E : in out Object) is
         Now : constant Time := Simulator.Current_Time;
         RET : Time renames E.T.Remaining_Exec_Time;
         Elapsed_Time : Time;
      begin
         pragma Debug (Up (2, "Stop"));
         pragma Debug (Trace (3, Name (E)));
         pragma Assert (E = E.T.Stop);
         pragma Assert (E.T.Running);
         E.T.Running := False;
         --  update remaining execution time
         Elapsed_Time := Now - E.T.Last_Time_Check;
         RET := RET - Elapsed_Time;
         E.T.Last_Time_Check := Now;
         --  track total time, for assertion-checking
         E.T.Total_Exec_Time :=
           E.T.Total_Exec_Time + Elapsed_Time;
         if RET > 0 then
            Simulator.Cancel_Event (E.T.Completion);
            --  Consider simultaneious suspension and
            --  completion events, where the suspension comes
            --  ahead of the completion.  In this case we
            --  want to allow the completion to be processed.
         end if;
         pragma Debug (Down);
      end Handler;

      function Name (E : Object) return String is
      begin
         return Name (E.T.all) & "stop " & Events.Object (E).Name;
      end Name;

   end Stop_Events;

   -----------------
   --  Get_Stats  --
   -----------------

   function Get_Stats (T : Task_Object) return Stats.Data is
   begin
      return T.Stats_Data;
   end Get_Stats;

   ------------
   --  Name  --
   ------------

   function Name (T : in Task_Object) return String is
   begin
      return Trim (T.Name, Right) & " ";
   end Name;

end Tasks;
