--  $Id: replenishments.ads,v 1.4 2008/11/23 01:54:28 baker Exp $

with Virtual_Times; use Virtual_Times;
with Generic_Queues;
package Replenishments is

   type R_Info is record
      R_Time : Time;
      R_Amount : Time;
   end record;

   function ">" (Left, Right: R_Info) return Boolean;

   package Queues is new Generic_Queues (R_Info);

   --  Merge_R_Queue coalesces all the replenishments
   --  in Q that have time <= Now, and sets their
   --  replenishment times to Now.

   procedure Merge_R_Queue
      (Q : in out Queues.Object;
       Now : in Time);

   --  Check_R_Sum verifies that the sum of the R_Amounts in Q is
   --  equal to the original Budget.  This is a useful assertion
   --  for catching errors in the replenishment logic.

   procedure Check_R_Sum
      (Q : in out Queues.Object;
       Budget : in Time;
       Msg: String);

   --  Show_R_Queue prints to the log file a list of the elements
   --  of Q.  This can be useful for debugging.

   procedure Show_R_Queue
      (Q : in Queues.Object);

end Replenishments;
