--  $Id: threads-sched_bgs.adb,v 1.1 2008/11/24 02:05:39 baker Exp $

--  background scheduling policy plug-in

package body Threads.Sched_BGS is

   type Policy_Ref is access all Object;

   ------------------
   --  Bind_Parms  --
   ------------------

   procedure Bind_Parms
     (P : in out Object;
      Parms : Aperiodic_Server_Parameters.Parameters) is
   begin
      null;      
   end Bind_Parms;

   ------------
   --  Init  --
   ------------

   procedure Init
     (P : in out Object) is
   begin
      P.T.Priority :=  Time'Last - 1;
   end Init;

end Threads.Sched_BGS;
