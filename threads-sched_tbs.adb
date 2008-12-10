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
      P.T.Is_In_Ready_Queue := False;
      Change_Priority(P.T, Time'Last);
   end Init;

-- Job arrives to empty server, set new deadline
-- Job completes and new job is selected (new job is J), set new deadline
   procedure New_Job(P : in out Object; J : in Jobs.Job) is
	Max : Time;
	Priority_New : Time;
   begin

	if P.T.Priority = Time'Last then
		Max := J.Arrival_time;
	elsif J.Arrival_time >= P.T.Priority then
		Max := J.Arrival_time;
	else
		Max := P.T.Priority;
	end if;

--        pragma Debug(Trace_Priority (1, P, "New_Job"));	
	Priority_New := Max + Time (Float'Floor ( Float ( Float (J.Execution_Time) / P.Server_Utilization) ) );
	Change_Priority(P.T, Priority_New);

        pragma Debug(Trace_Priority (1, P, "New_Job"));
   end New_Job;
end Threads.Sched_TBS;
