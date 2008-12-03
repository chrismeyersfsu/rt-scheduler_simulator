-- $Id: test_three.adb,v 1.4 2008/11/23 21:33:20 baker Exp baker $

--  This is a third test program for simulation packages.  It
--  tests three tasks, one aperiodic, and two periodic.  You can
--  use it as a model for testing other policies, by editing all
--  the places where the string "DSS" appears.  You may also want
--  to try different values for the periods, execution times,
--  minimum inter-arrival times, etc.

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
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
with Periodic_Workloads;
with Threads.Sched_EDF;
with Virtual_Times; use Virtual_Times;

procedure Test_Three is

   --  Aperiodic tasks

   Aperiodic_Task : aliased Tasks.Task_Object;

   Aperiodic_Load : aliased Aperiodic_Workloads.Object;

   Aperiodic_Policy : aliased Threads.Sched_DSS.Object;

   --  Periodic tasks

   subtype P_Range is Integer range 1 .. 1;

   Periodic_tasks : array (P_Range) of aliased Tasks.Task_Object;

   Periodic_Loads : array (P_Range) of aliased Periodic_Workloads.Object;

   Periodic_Load_Parameters : array (1 .. 2)
     of aliased Periodic_Workloads.Parameters :=
     ((Period => 25, Deadline => 25, WCET => 5),
      (Period => 35, Deadline => 35, WCET => 4)
     );

   Periodic_Policies : array (P_Range)
     of aliased Threads.Sched_EDF.Object;

   Test_Name : constant String := "test_three";

begin

   Error_Log.Open (Test_Name);
   Error_Log.Debug_Level := 10;

   --  set up one workload model object and one
   --  scheduling policy object per aperiodic task

   for K in Periodic_Tasks'Range loop
      Periodic_Loads (K).Bind_Parms (Periodic_Load_Parameters (K));
      Tasks.Bind
        (Periodic_Tasks (K)'Unchecked_Access,
         M => Periodic_Loads (K)'Unchecked_Access,
         P => Periodic_Policies (K)'Unchecked_Access,
         Name => 'P' & Trim (Integer'Image (K + 1), Left));
   end loop;

   --  run four simulations, for different aperiodic load levels

   Aperiodic_Load.Bind_Parms
     ((Mean_Interarrival_Time => 5.0, Mean_Execution_Time => 2.0));

   for I in 1 .. 4 loop

      New_Line;
      Put ("with aperiodic utilization ");
      Put (Float (I) / 5.0, 1,3,0);
      New_Line;

      Random_Tools.Start (3, 5, 7, 11, False);

      Simulator.Initialize;

      --  initialize/reset the tasks,
      --  and thereby everything that depends on them

      Aperiodic_Policy.Bind_Parms
         ((Budget => Time (I * 10),
           Budget_Interval => Time (I * 20)));

      Tasks.Bind
        (Aperiodic_Task'Unchecked_Access,
         M => Aperiodic_Load'Unchecked_Access,
         P => Aperiodic_Policy'Unchecked_Access,
         Name => 'A' & "1");

      --  start the pseudo-random number generator that
      --  determines the "random" events in the simulation

      Trace (3, "initializing tasks");
      Tasks.Initialize;

      Trace (3, "starting simulation");
      Simulator.Run (Normal_Stop_Time => 1000,
                     Latest_Stop_Time => 2000);

      declare
         S : Stats.Data := Tasks.Get_Stats (Aperiodic_Task);
      begin
         --  just the basics, for now
         Put (Tasks.Name (Aperiodic_Task)
                & "Max_Resp_Time         = ");
         Put (Integer (S.Max_Resp_Time));
         New_Line;
         Put (Tasks.Name (Aperiodic_Task)
                & "Average response time = ");
         Put (Float (S.Total_Resp_Time) / Float (S.Job_Count), 7, 3, 0);
         New_Line;
      end;

      for K in Periodic_Tasks'Range loop
         declare
            S : Stats.Data := Tasks.Get_Stats (Periodic_Tasks (K));
         begin
            Put (Tasks.Name(Periodic_Tasks (K))
                & "Max_Resp_Time         = ");
            Put (Integer (S.Max_Resp_Time));
            New_Line;
            Put (Tasks.Name (Periodic_Tasks (K))
                & "Missed_Deadlines      = ");
            Put (Integer (S.Missed_Deadlines));
            New_Line;
         end;
      end loop;

   end loop;

   Error_Log.Close;

exception
   when E : others =>
      Put (Log, "exception " & Ada.Exceptions.Exception_Name (E));
      Put (Log, " in " & Test_Name & " at ");
      Put_Line (Log, Ada.Exceptions.Exception_Message (E));
      Error_Log.Close;
      raise;
end Test_Three;
