--  $Id: tasks.ads,v 1.6 2008/11/23 21:33:20 baker Exp $

with Virtual_Times; use Virtual_Times;
with Events;
with Jobs;
with Job_Queues;
with Workload_Models;
with Threads;
with Stats;
package Tasks is

   type Task_Object is limited private;
   type Task_Ref is access all Task_Object;

   procedure Bind
     (T : Task_Ref;
      M : Workload_Models.Class_Ref;
      P : Threads.Policies_Class_Ref;
      Name : String);

   --  bind workload model
   --  and thread scheduling policy to task

   procedure Initialize;
   --  sets (or resets) all bound tasks to their starting states.

   procedure Reset_Stats;
   --  resets all statistics
   --  can be used to gather statistics only after steady state is reached

   function Get_Stats (T : Task_Object) return Stats.Data;

   function Name (T : in Task_Object) return String;

   generic
     with procedure P (Item: in Task_Ref);
   procedure Do_For_Every_Task;

private

   package Job_Arrival_Events is
      type Object is new Events.Object with record
         T : Task_Ref;
      end record;
      procedure Handler (E : in out Object);
      function Name (E : Object) return String;
   end Job_Arrival_Events;

   package Job_Completion_Events is
      type Object is new Events.Object with record
         T : Task_Ref;
      end record;
      procedure Handler (E : in out Object);
      function Name (E : Object) return String;
   end Job_Completion_Events;

   package Go_Events is
      type Object is new Events.Object with record
         T : Task_Ref;
      end record;
      procedure Handler (E : in out Object);
      function Name (E : Object) return String;
   end Go_Events;

   package Stop_Events is
      type Object is new Events.Object with record
         T : Task_Ref;
      end record;
      procedure Handler (E : in out Object);
      function Name (E : Object) return String;
   end Stop_Events;

   type Task_Object is record

      -- internal execution state
      Current_Job : Jobs.Job;
      Pending_Jobs : Job_Queues.Object;
      Running : Boolean;
      Remaining_Exec_Time : Time;  --  of current job
      Last_Time_Check : Time;
      --  most recent time current job started/resumed execution
      --  used to compute amount by which to decrement
      --  remaining execution time

      -- interface to scheduling services
      Thread : Threads.Thread_Ref;

      --  interface for simulator
      Arrival : Job_Arrival_Events.Object;
      Completion : Job_Completion_Events.Object;

      --  interface for simulator and thread scheduler
      Go : aliased Go_Events.Object;
      Stop : aliased Stop_Events.Object;

      --  to generate arrivals and execution times
      Model : Workload_Models.Class_Ref;

      --  for gathering statistics
      Stats_Data : Stats.Data;

      --  for assertion checking
      Total_Exec_Time : Time;

      --  for tracing
      Name : String (1 .. 4);

      --  to make up for bad initialization and binding design
      Is_In_All_Tasks : Boolean := False;

   end record;

end Tasks;
