-- $Id: test_bgs.adb,v 1.1 2008/11/24 02:05:39 baker Exp baker $

--  This is a simple test of a single aperiodic task, similar to
--  test_zero.adb and test_one.adb but for the BGS policy.

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Error_Log; use Error_Log;
with Tasks;
with Simulator;
with Threads;
with Threads.Sched_BGS;
with Workload_Models;
with Aperiodic_Workloads;
with Random_Tools;
with Stats;
with Virtual_Times; use Virtual_Times;

procedure Test_bgs is

   Aperiodic_Task : aliased Tasks.Task_Object;

   Aperiodic_Load : aliased Aperiodic_Workloads.Object;

   BGS_Policy : aliased Threads.Sched_BGS.Object;

   Test_Name : constant String := "test_six";

begin

   Error_Log.Open (Test_Name);
   Error_Log.Debug_Level := 10;

   --  run one simulation
   --  just one task, for now

   Simulator.Initialize;

   --  set up one workload model object and
   --  scheduling policy object

   BGS_Policy.Bind_Parms
     ((Budget => 5, Budget_Interval => 8));

   Aperiodic_Load.Bind_Parms
     ((Mean_Interarrival_Time => 10.0, Mean_Execution_Time =>5.0));

   Tasks.Bind (Aperiodic_Task'Unchecked_Access,
         M => Aperiodic_Load'Unchecked_Access,
         P => BGS_Policy'Unchecked_Access,
         Name => "A1");

   --  start the pseudo-random number generator that
   --  determines the "random" events in the simulation

   Random_Tools.Start (3, 5, 7, 11, False);

   --  initialize/reset the tasks,
   --  and thereby everything that depends on them

   Tasks.Initialize;
   Simulator.Run (Normal_Stop_Time => 10000,
                  Latest_Stop_Time => 20000);

   Put ("Total_Idle_time       = ");
   Put (Integer (Threads.Total_Idle_Time));
   New_Line;
   Put ("Total_simulation_time = ");
   Put (Integer (Simulator.Current_Time));
   New_Line;
   declare
      S : Stats.Data := Tasks.Get_Stats (Aperiodic_Task);
   begin
      Put ("Max_Resp_Time         = ");
      Put (Integer (S.Max_Resp_Time));
      New_Line;
      Put ("Total_Resp_Time       = ");
      Put (Integer (S.Total_Resp_Time));
      New_Line;
      Put ("Job_Count             = ");
      Put (Integer (S.Job_Count));
      New_Line;
      Put ("Missed_Deadlines      = ");
      Put (Integer (S.Missed_Deadlines));
      New_Line;
      Put ("Total_Exec_Time       = ");
      Put (Integer (S.Total_Exec_Time));
      New_Line;
      pragma Assert (Threads.Total_Idle_Time
                     + Time (S.Total_Exec_Time)
                     = Simulator.Current_Time);
   end;

   Error_Log.Close;

exception
   when E : others =>
      Put (Log, "exception " & Ada.Exceptions.Exception_Name (E));
      Put (Log, " in " & Test_Name & " at ");
      Put_Line (Log, Ada.Exceptions.Exception_Message (E));
      Error_Log.Close;
      raise;
end Test_bgs;
