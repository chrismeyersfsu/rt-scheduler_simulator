---  $Id: threads-sched_pls.adb,v 1.2 2008/11/24 15:53:04 baker Exp baker $

--  polling server scheduler

with Simulator;
with Error_Log; use Error_Log;
with Ada.Text_IO; use Ada.Text_IO;
package body Threads.Sched_TBS is

   type Policy_Ref is access all Object;

   ------------------
   --  Bind_Parms  --
   ------------------

   procedure Bind_Parms
     (P : in out Object;
      Parms : Aperiodic_Server_Parameters.Parameters) is
   begin
      P.Parms := Parms;
      P.Server_Utilization := Float (Float (P.Parms.Budget) / Float (P.Parms.Budget_Interval));
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

	Priority_New := Max + Time (Float'Ceiling ( Float ( Float (J.Execution_Time) / P.Server_Utilization) ) );

	Change_Priority(P.T, Priority_New);

   end New_Job;
end Threads.Sched_TBS;


