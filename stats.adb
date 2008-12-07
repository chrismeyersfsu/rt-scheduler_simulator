--  $Id: stats.adb,v 1.4 2008/11/24 02:05:39 baker Exp $

package body Stats is

   procedure End_Of_Job_Update
     (D : in out Data;
      RT : Time; -- response time
      Deadline : Time;
      Exec_Time : Time;
      Now : Time) is
   begin
      if RT > D.Max_Resp_Time then
         D.Max_Resp_Time := RT;
      end if;
      D.Total_Exec_Time := D.Total_Exec_Time + Exec_Time;
      D.Total_Resp_Time := D.Total_Resp_Time + Float (RT);
      D.Total_Sq_Resp_Time := D.Total_Sq_Resp_Time + Float (RT) ** 2;
      D.Job_Count := D.Job_Count + 1;
      if Now > Deadline then
         D.Missed_Deadlines := D.Missed_Deadlines + 1;
      end if;
   end End_Of_Job_Update;

end Stats;
