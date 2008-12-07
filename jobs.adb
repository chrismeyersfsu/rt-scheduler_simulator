--  $Id: jobs.adb,v 1.2 2008/11/20 18:49:43 baker Exp $

package body Jobs is
   function ">" (Left, Right: Job) return Boolean is
   begin
      return Left.Arrival_Time >= Right.Arrival_Time;
   end ">";
end Jobs;
