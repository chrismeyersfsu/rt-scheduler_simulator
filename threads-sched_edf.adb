--  $Id: threads-sched_edf.adb,v 1.2 2008/11/23 01:54:28 baker Exp $

--  EDF scheduler

with Simulator;
with Error_Log; use Error_Log;
with Ada.Text_IO; use Ada.Text_IO;
package body Threads.Sched_EDF is

   type Policy_Ref is access all Object;

   ------------
   --  Init  --
   ------------

   procedure Init
     (P : in out Object) is
   begin
      P.T.Priority := Time'Last;
      P.T.Is_In_Ready_Queue := False;
   end Init;

   ---------------
   --  New_Job  --
   ---------------

   --  This is called when a server starts working on a new job
   --  from its queue.  We don't need this for most (all?)
   --  aperiodic server policies, but we may need this to provide
   --  the deadline for periodic tasks that are scheduled
   --  according to individual job deadlines.

   procedure New_Job
     (P : in out Object;
      J : in Jobs.Job) is
   begin
      P.Current_Job := J;
      P.T.Priority := J.Absolute_Deadline;
   end New_Job;

end Threads.Sched_EDF;
