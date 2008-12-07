--  $Id: replenishments.adb,v 1.5 2008/11/24 02:05:39 baker Exp $

with Error_Log; use Error_Log;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
package body Replenishments is
   use Queues;

   function ">" (Left, Right: R_Info) return Boolean is
   begin
      return Left.R_Time >= Right.R_Time;
   end ">";

   procedure Merge_R_Queue
      (Q : in out Queues.Object;
       Now : in Time) is
      T, R : R_Info;
   begin
      pragma Assert (not Is_Empty (Q));
      --  combine all currently available replenishments
      R := Q.Front_Of;
      T.R_Time := Now;
      T.R_Amount := 0;
      while not Q.Is_Empty and then
        Q.Front_Of.R_Time <= Now loop
        T.R_Amount := T.R_Amount + Q.Front_Of.R_Amount;
        Q.Pop;
      end loop;
      Q.Add (T);
   end Merge_R_Queue;

   procedure Check_R_Sum
      (Q : in out Queues.Object;
       Budget : in Time;
       Msg: String) is
      Total : Time := 0;
      procedure Check_One (R : R_Info) is
      begin
         Total := Total + R.R_Amount;
      end Check_One;
      procedure Check_All is new
        Queues.For_All (Check_One);
   begin
      pragma Assert (not Q.Is_Empty);
      Check_All (Q);
      if Total /= Budget then
         Put_Line (Log, "Check_R_Sum failure: total =" &
                     Time'Image (Total) & " budget = " &
                     Time'Image (Budget) & " at " & Msg);
         Show_R_Queue (Q);
         New_Line (Log);
         pragma Assert (False);
      end if;
   end Check_R_Sum;

   procedure Show_R_Queue
      (Q : in Queues.Object) is
      Total : Time := 0;
      procedure Show_One (R : R_Info) is
      begin
         Put (Log, '{' & Trim (Time'Image (R.R_Time), Left)
                &  Time'Image (R.R_Amount) & '}');
      end Show_One;
      procedure Show_All is new
        Replenishments.Queues.For_All (Show_One);
   begin
      Show_All (Q);
   end Show_R_Queue;

end Replenishments;

