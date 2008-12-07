-- $Id: stats.ads,v 1.3 2008/11/23 21:33:20 baker Exp $

with Virtual_Times; use Virtual_Times;
package Stats is

   type Data is record
      Max_Resp_Time : Time;
      Total_Resp_Time : Float;
      Total_Sq_Resp_Time : Float;
      Job_Count : Integer;
      Total_Exec_Time : Time;
      Missed_Deadlines : Integer;
   end record;

   procedure End_Of_Job_Update
     (D : in out Data;
      RT : Time;  -- response time
      Deadline : Time;
      Exec_Time : Time;
      Now : Time);

end Stats;
