-- $Id: test_five.adb,v 1.2 2008/11/24 16:46:29 baker Exp baker $

--  This is a cut-down version of test_four.adb.
--  It only runs the first experiment.  This can be useful for debugging.

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Aperiodic_Server_Parameters; use Aperiodic_Server_Parameters;
with Aperiodic_Workloads;
with Cycles; use Cycles;
with Error_Log; use Error_Log;
with Periodic_Workloads;
with Random_Tools;
with Report;
with Simulator;
with Sizes; use Sizes;
with Stats;
with Tasks;
with Threads; use Threads;
with Threads.Aperiodic_Policies; use Threads.Aperiodic_Policies;
with Threads.Sched_EDF;
with Threads.Sched_DSS;
with Threads.Sched_BGS;
with Threads.Sched_PLS;
with Virtual_Times; use Virtual_Times;
with Workload_Models;

procedure Test_Five is

   --  Aperiodic tasks

   Aperiodic_Task : aliased Tasks.Task_Object;

   Aperiodic_Work : aliased Aperiodic_Workloads.Object;

   DSS_Policy : aliased Threads.Sched_DSS.Object;
   BGS_Policy : aliased Threads.Sched_BGS.Object;
   PLS_Policy : aliased Threads.Sched_PLS.Object;

   Aperiodic_Policies : array (Servers)
     of aliased Threads.Aperiodic_Policies.Class_Ref :=
     (DSS => DSS_Policy'Unchecked_Access,
      BGS => BGS_Policy'Unchecked_Access,
      PLS => PLS_Policy'Unchecked_Access,
      others => null);

   Thread_Policies : array (Servers)
     of aliased Threads.Policies_Class_Ref :=
     (DSS => DSS_Policy'Unchecked_Access,
      BGS => BGS_Policy'Unchecked_Access,
      PLS => PLS_Policy'Unchecked_Access,
      others => null);



   --  Periodic tasks

   Periodic_Tasks : array (Periodic_Range) of aliased Tasks.Task_Object;

   Periodic_Works : array (Periodic_Range) of aliased Periodic_Workloads.Object;

   Periodic_Policies : array (Periodic_Range)
     of aliased Threads.Sched_EDF.Object;

   Test_Name : constant String := "test_five";

   Hyperperiod : Time;

begin

   Error_Log.Open (Test_Name);
   Error_Log.Debug_Level := 3;

   for I in IATS' First .. IATS'First loop
      Report.New_IAT;
      Report.Start_Tex (I);
      for R in Runs' First .. Runs'First loop
         Report.Start_Plot (I, R);
         for L in Loads'First .. Loads'First loop
            for S in BGS .. DSS loop
               if Aperiodic_Policies (S) /= null then
                  Trace (1, ">>>> testing server with policy: "
                           & Servers'Image (S)
                           & ", load"
                           & Integer'Image (Integer (Aperiodic_Load (R)(L) * 100.0))
                           & "%, mean IAT"
                           & Integer'Image (Integer (Interarrival_Times (I)))
                           & ", and periodic load"
                           & Integer'Image (U (R))
                           & "% <<<<");
                  Put_Line (">>>> testing server scheduling policy "
                           & Servers'Image (S)
                           & ", load"
                           & Integer'Image (Integer (Aperiodic_Load (R)(L) * 100.0))
                           & "%, mean IAT"
                           & Integer'Image (Integer (Interarrival_Times (I)))
                           & ", and periodic load"
                           & Integer'Image (U (R))
                           & "% <<<<");
                  --  start the pseudo-random number generator that
                  --  determines the "random" events in the simulation
                  Random_Tools.Start (3, 5, 7, 11, False);
                  Trace (2, "test_five: --0--");
                  Simulator.Initialize;
                  Trace (2, "test_five: --1--");
                  --  set up the aperiodic server task
                  Aperiodic_Work.Bind_Parms
                    ((Mean_Interarrival_Time => Interarrival_Times (I),
                      Mean_Execution_Time =>
                        Aperiodic_Load (R)(L) * Interarrival_Times (I)));
                  Trace (2, "test_five: --2--");
                  Aperiodic_Policies (S).Bind_Parms
                     ((Budget => EDF_Size (R)(S),
                       Budget_Interval => Server_Period));
                  Trace (2, "test_five: --3--");
                  Tasks.Bind
                    (Aperiodic_Task'Unchecked_Access,
                     M => Aperiodic_Work'Unchecked_Access,
                     P => Thread_Policies (S),
                     Name => 'A' & "1");
                  --  find hyperperiod
                  Trace (2, "test_five: --4--");
                  declare
                     Periods : Positive_Vector (Periodic_Range);
                  begin
                     for P in Periods'Range loop
                        Periods (P) :=
                          Integer (Sizes.Periodic_Parameters (R)(P).Period);
                     end loop;
                     Hyperperiod := Time (LCM (Periods));
                  end;
                  --  set up periodic task set
                  Trace (2, "test_five: --5--");
                  for P in Periodic_Range loop
                     Periodic_Works (P).Bind_Parms
                       (Periodic_Parameters (R)(P));
                     Tasks.Bind (Periodic_Tasks (P)'Unchecked_Access,
                                 M => Periodic_Works (P)'Unchecked_Access,
                                 P => Periodic_Policies (P)'Unchecked_Access,
                                 Name => 'P' & Trim (Integer'Image (P), Left));
                  end loop;
                  --  (re) initialize task models with newly bound values
                  Trace (2, "test_five: --6--");
                  Tasks.Initialize;
                  --  run simulation
                  Trace (2, "test_five: --7--");
--                  Simulator.Run (Normal_Stop_Time => Hyperperiod * 3,
--                                 Latest_Stop_Time => Hyperperiod * 4);
                  Simulator.Run (Normal_Stop_Time => 100000,
                                 Latest_Stop_Time => 200000);
                  --  collect results
                  declare
                     D : Stats.Data;
                  begin
                     for P in Periodic_Range loop
                        D := Tasks.Get_Stats (Periodic_Tasks (P));
                        Report.Collect_Periodic (R, L, S, D);
                     end loop;
                     D := Tasks.Get_Stats (Aperiodic_Task);
                     Report.Collect_Server (R, L, S, D);
                     Report.Validate (R, L, S);
                  end;
                  Trace (2, "test_five: --8--");
                  --  alternative output, to compare with report
                  declare
                     S : Stats.Data := Tasks.Get_Stats (Aperiodic_Task);
                  begin
                     Put (Tasks.Name (Aperiodic_Task)
                            & "Max_Resp_Time         = ");
                     Put (Integer (S.Max_Resp_Time));
                     New_Line;
                     Put (Tasks.Name (Aperiodic_Task)
                            & "Total_Resp_Time       = ");
                     Put (S.Total_Resp_Time, 9, 0, 0);
                     New_Line;
                     Put (Tasks.Name (Aperiodic_Task)
                            & "Total_Sq_Resp_Time    =");
                     Put (S.Total_Sq_Resp_Time);
                     New_Line;
                     Put (Tasks.Name (Aperiodic_Task)
                            & "Total_Exec_Time       = ");
                     Put (Integer (S.Total_Exec_Time));
                     New_Line;
                     Put (Tasks.Name (Aperiodic_Task)
                            & "Job_Count             = ");
                     Put (S.Job_Count);
                     New_Line;
                     Put (Tasks.Name (Aperiodic_Task)
                            & "Average response time = ");
                     Put (Float (S.Total_Resp_Time) / Float (S.Job_Count), 9, 0, 0);
                     New_Line;
                     Put (Tasks.Name (Aperiodic_Task)
                         & "Missed_Deadlines         = ");
                     Put (Integer (S.Missed_Deadlines));
                     New_Line;
                  end;
                  New_Line;
                  for K in Periodic_Tasks'Range loop
                     declare
                        S : Stats.Data := Tasks.Get_Stats (Periodic_Tasks (K));
                     begin
                        Put (Tasks.Name(Periodic_Tasks (K))
                               & "Max_Resp_Time           = ");
                        Put (Integer (S.Max_Resp_Time));
                        New_Line;
                        Put (Tasks.Name (Periodic_Tasks (K))
                               & "Total_Resp_Time         = ");
                        Put (S.Total_Resp_Time, 9, 0, 0);
                        New_Line;
                        Put (Tasks.Name (Periodic_Tasks (K))
                               & "Total_Sq_Resp_Time      =");
                        Put (S.Total_Sq_Resp_Time);
                        New_Line;
                        Put (Tasks.Name (Periodic_Tasks (K))
                               & "Total_Exec_Time         = ");
                        Put (Integer (S.Total_Exec_Time));
                        New_Line;
                        Put (Tasks.Name (Periodic_Tasks (K))
                               & "Job_Count               = ");
                        Put (S.Job_Count);
                        New_Line;
                        Put (Tasks.Name (Periodic_Tasks (K))
                               & "Average response time   = ");
                        Put (Float (S.Total_Resp_Time) / Float (S.Job_Count), 9, 0, 0);
                        New_Line;
                        Put (Tasks.Name (Periodic_Tasks (K))
                             & "Missed_Deadlines        = ");
                        Put (Integer (S.Missed_Deadlines));
                        New_Line;
                     end;
                     New_Line;
                  end loop;
               end if;
            end loop; --  Servers
            Report.Put_Plot (I, R, L);
            Report.Put_Tex (I, R, L);
         end loop; --  Loads
         Report.Break_Tex;
         Report.Finish_Plot;
      end loop; --  Runs
      Report.Finish_Tex (I);
   end loop; --  IATS

   Error_Log.Close;

exception
   when E : others =>
      Put (Log, "exception " & Ada.Exceptions.Exception_Name (E));
      Put (Log, " in " & Test_Name & " at ");
      Put_Line (Log, Ada.Exceptions.Exception_Message (E));
      Error_Log.Close;
      raise;
end Test_Five;
