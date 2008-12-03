--  $Id: threads.ads,v 1.5 2008/11/24 02:05:39 baker Exp baker $

--  Provides thread abstraction, implementing services typically
--  provided by an operating system to schedule the execution of a
--  thread.

with Virtual_Times; use Virtual_Times;
with Generic_Queues;
with Events;
with Jobs;
--  but *not* Tasks!
--  To avoid mutual dependence, tasks (arrival and workload
--  generation objects) can see the threads (system processes) that
--  they influence, but not vice versa.
package Threads is

   type Thread is limited private;
   type Thread_Ref is access all Thread;

   --------------------------
   --  Scheduling Policies --
   --------------------------

   --  Allow us to define plug-in modules to implement
   --  a variety of per-thread scheduling policies.
   --  Each policy may need policy-specific
   --  per-thread parameters and state information.

   package Scheduling_Parameters is
      --  used to specify scheduling parameters, such as budget
      --  and replenishment periodic for aperiodic servers
      type Object is abstract tagged null record;
   end Scheduling_Parameters;

   type Parameters_Class_Ref is access all
     Scheduling_Parameters.Object'Class;

   package Scheduling_Policies_State is
      --  used to store policy-specific per-thread data
      --  such as budget replenishment info
      type Object is abstract tagged null record;
   end Scheduling_Policies_State;

   type Policy_State_Class_Ref is access all
     Scheduling_Policies_State.Object'Class;

   package Scheduling_Policies is

      type Object is abstract tagged record
         Data : Policy_State_Class_Ref;
         T : Thread_Ref;
      end record;

      -----------------------
      --  Initializations  --
      -----------------------

      --  Bind_Thread is called to bind A scheduling policy to a
      --  thread.  There should be exactly one policy object to
      --  each thread object.

      procedure Bind_Thread
        (P : in out Object;
         T : Thread_Ref);

      --  Init is called to initialize or reset the
      --  policy-specific state of a thread.  It assumes the poicy
      --  has been previously bound to a thread.

      procedure Init
        (P : in out Object) is abstract;

      --  The following are "hooks" that allow insertion of
      --  policy-specific actions at key points during execution.
      --  They are provided for a wide range of points in the
      --  execution and scheduling of a thread.  Most scheduling
      --  policies will not need to use more than a few of these.

      --------------------
      --  Task Actions  --
      --------------------

      --  The following are triggered by actions
      --  at the task level.

      --  New_Job is called when a task starts executing a new
      --  job. For plain EDF scheduling this is needed, because
      --  the deadline of the job determines the thread priority.
      --  See threads-sched_edf.adb for an example of use.

      procedure New_Job
        (P : in out Object;
         J : in Jobs.Job);

      --  Suspend is called when a thread has suspended itself.
      --  For a polling server, this indicates that the thread
      --  should give up its remaining budget.  See
      --  threads-sched_pls.adb for an example of use.

      procedure Suspend
        (P : in out Object);

      --  Unsuspend is called when a thread wakes up.  For any
      --  aperiodic server, this occurs when a job arrives to an
      --  empty server job queue, and the server wakes up to
      --  resume contention for execution.

      procedure Unsuspend
        (P : in out Object);

      --------------------------------
      --  Thread Scheduler Actions  --
      --------------------------------

      --  The following are triggered by actions at the thread
      --  scheduler level.

      procedure Enter_Scheduler
        (P : in out Object);

      --  Enter_Scheduler is called whenever scheduler is
      --  entered from a non-idle thread.  P corresponds to
      --  Current, the currently executing task.

      procedure New_Current_Thread
        (P : in out Object);

      --  New_Current_Thread is called when the thread scheduler
      --  has switched threads.  e.g., if it switches to the idle
      --  thread, this may be a time to replenish server budgets
      --  It is the different from the other callout, in being
      --  for *every* thread in the system, for every context switch.
      --  That is, P is not necessarily the current thread.

      procedure Go
        (P : in out Object);

      --  Go is called at the point the scheduler is ready to
      --  transfer control to the thread corresponding to P.  That
      --  is, the thread is resuming execution.  This may also be a
      --  point to set a timer to check for budget exhaustion, to
      --  lower the thread's priority when it has used up a chunk
      --  of budget.  You can count on Current pointing to the
      --  current thread.  See threads-sched_pls.adb for an
      --  example of use.

      procedure Stop
        (P : in out Object);

      --  Stop is called at the point that a thread has stopped
      --  execution.  This may be a time to schedule a
      --  replenishment, and to cancel a timer that would notify
      --  us of budget exhaustion.  You can count on Current
      --  pointing to the stopped thread.

      procedure Idle
        (P : in out Object);

      --  Idle is called when the system is idle.  This may be a
      --  time to do an early replenishment, for some variants of
      --  Sporadic Server.

   end Scheduling_Policies;

   type Policies_Class_Ref is access all
     Scheduling_Policies.Object'Class;

   -------------------------
   --  Thread operations  --
   -------------------------

   procedure Initialize;  -- of the entire threads world
   --  also call this to reset the threads for a new run;
   --  does not delete any existing threads, but does
   --  reset their internal states

   procedure Bind_Policy
     (T : in out Thread_Ref;
      P : Policies_Class_Ref);

   procedure Schedule;

   function New_Thread
     (Go : Events.Class_Ref;
      Stop : Events.Class_Ref;
      Name : String)
     return Thread_Ref;

   procedure New_Job (T : Thread_Ref;
                      J : Jobs.Job);
   --  tells thread (and its scheduler) that we have
   --  started a new job, and so may need to change
   --  deadline

   procedure Unsuspend (T : Thread_Ref);
   --  tells thread that it should not execute;
   --  the initial thread state is suspended

   procedure Suspend (T : Thread_Ref);

   function Name (T: Thread) return String;

   function Total_Idle_Time return Time;

private

   type Thread is record
      --  scheduling policy plug-ins
      Policy : Policies_Class_Ref;
      Policy_State : Policy_State_Class_Ref;
      --  interface for tasks
      Go : Events.Class_Ref;
      Stop : Events.Class_Ref;
      --  scheduling state:
      Is_Suspended : Boolean;
      --  is logically not ready to run
      --  for example, periodic thread waiting for next period
      Is_Policy_Suspended : Boolean;
      --  the thread may be logically ready to run,
      --  but is suspended by the scheduling policy
      --  e.g., it is out of budget
      Priority : Time;
      --  larger is higher
      Is_In_Ready_Queue : Boolean;  -- actually in queue
      Name : String (1 .. 4);
   end record;

   --  The following are for potential read-access by child
   --  packages

   Last_Idle_Time : Time;
   Current : Thread_Ref;
   Idle_Thread : Thread_Ref := null;
   Total_Idle : Time;

   function ">" (L, R : Thread_Ref) return Boolean;
   package Threads_Queues is new Generic_Queues (Thread_Ref);

   --  Thread queues have numerically smallest priority on top
   --  This works well for EDF and deadline monotonic scheduling

   Ready_Queue : Threads_Queues.Object;

   --  A list of all threads that are policy-suspended

   Policy_Suspended_Queue : Threads_Queues.Object;

   --  A list of all threads known to the system.  Should not
   --  include the idle thread.

   All_Threads : Threads_Queues.Object;

   --  The following are for the use of policies that need to
   --  prevent a thread from running, because it is out of budget,
   --  without actually suspending it.

   procedure Policy_Suspend (T : Thread_Ref);
   procedure Policy_Unsuspend  -- use this one
     (T : Thread_Ref);
   procedure Policy_Unsuspend  -- obsolete
     (T : Thread_Ref;
      New_Priority : Time);

end Threads;


