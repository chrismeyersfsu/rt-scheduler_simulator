-- $Id: test_one.adb,v 1.6 2008/11/23 21:33:20 baker Exp baker $

--  This is a first test program for aperiodic tasks. It only
--  tests one task, which is aperiodic, with one policy.  Anything
--  more complicated would be more difficult to debug.  This
--  version is for the DSS scheduling policy.  You can use it as a
--  model for testing other policies, by editing all the places
--  where the string "DSS" appears.

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Error_Log; use Error_Log;
with Tasks;
with Simulator;
with Threads;
with Threads.Sched_DSS;
with Workload_Models;
with Aperiodic_Server_Parameters; use Aperiodic_Server_Parameters;
with Aperiodic_Workloads;
with Random_Tools;
with Stats;

procedure Test_One is

   Aperiodic_Task : aliased Tasks.Task_Object;

   Aperiodic_Load : aliased Aperiodic_Workloads.Object;

   Aperiodic_Policy : aliased Threads.Sched_DSS.Object;

   Test_Name : constant String := "test_one";

begin

   Error_Log.Open (Test_Name);
   Error_Log.Debug_Level := 10;

   --  start the pseudo-random number generator that
   --  determines the "random" events in the simulation

   Random_Tools.Start (3, 5, 7, 11, False);

   Simulator.Initialize;

   --  set up one workload model object and one
   --  scheduling policy object per aperiodic task

   Aperiodic_Load.Bind_Parms
     ((Mean_Interarrival_Time => 3.0, Mean_Execution_Time => 2.0));
   Aperiodic_Policy.Bind_Parms
     ((Budget => 1, Budget_Interval => 2));
   Tasks.Bind
     (Aperiodic_Task'Unchecked_Access,
      M => Aperiodic_Load'Unchecked_Access,
      P => Aperiodic_Policy'Unchecked_Access,
      Name => "A1");

   --  initialize/reset the tasks,
   --  and thereby everything that depends on them

   Tasks.Initialize;

   --  run one simulation

   Simulator.Run (Normal_Stop_Time => 10,
                  Latest_Stop_Time => 20);

   declare
      S : Stats.Data := Tasks.Get_Stats (Aperiodic_Task);
   begin
      Put ("Max_Resp_Time    = ");
      Put (Integer (S.Max_Resp_Time));
      New_Line;
      Put ("Total_Resp_Time  = ");
      Put (Integer (S.Total_Resp_Time));
      New_Line;
      Put ("Job_Count        = ");
      Put (Integer (S.Job_Count));
      New_Line;
      Put ("Missed_Deadlines = ");
      Put (Integer (S.Missed_Deadlines));
      New_Line;
   end;

   Error_Log.Close;

exception
   when E : others =>
      Put (Log, "exception " & Ada.Exceptions.Exception_Name (E));
      Put (Log, " in " & Test_Name & " at ");
      Put_Line (Log, Ada.Exceptions.Exception_Message (E));
      Error_Log.Close;
      raise;
end Test_one;
