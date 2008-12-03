with Events;
with Virtual_Times; use Virtual_Times; -- for type Time
with Ada.Text_IO; use Ada.Text_IO;
package Simulator is

   procedure Initialize;

   procedure Run (Latest_Stop_Time : Time);
   procedure Run (Normal_Stop_Time, Latest_Stop_Time : Time);
   --  Call this after queuing up initial events, like task starts.

   procedure Schedule_Event (E : in out Events.Object'Class);
   --  inserts E into the event queue
   --  at the associated time

   procedure Cancel_Event (E : in out Events.Object'Class);
   --  removes E from the event queue;
   --  You want to do this whenever you change the associated time

   procedure Set_Every_Clock_Event (E : in out Events.Object'Class);
   --  can arrange for one event to be triggered every time
   --  that the simulated time advances

   Simulation_Done : exception;
   --  Raise this to terminate the simulation immediately.

   procedure Stop;
   --  will stop the simulation at the next call to Check_time.

   procedure Check_Time;
   --  will stop the simulation if the current time > Latest_Stop Time.

   function Current_Time return Time;

   procedure Show_Event_Queue (F : File_Type; Long : Boolean := False);
   --  prints out event queue contents

end Simulator;
