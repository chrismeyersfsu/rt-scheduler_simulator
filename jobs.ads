--  $Id: jobs.ads,v 1.2 2008/11/20 18:49:43 baker Exp $

with Virtual_Times; use Virtual_Times;
package Jobs is
   type Job is record
      Arrival_Time,
      Execution_Time,
      Absolute_Deadline : Time;
   end record;
   function ">" (Left, Right: Job) return Boolean;
end Jobs;
