with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Virtual_Times; use Virtual_Times; -- for type Time
with Events;
with Error_Log; use Error_Log;  -- for file Log
with System.Assertions;
package body Simulator is

   Now : Time :=0;
   Stopping_Time : Time;
   Latest_Stopping_Time : Time;
   Event_Queue : Events.Queues.Object;

   Every_Clock_Event : Events.Class_Ref;
   --  a clumsy hook on which to hang an event that
   --  need to be done every time the clock advances

   function Current_Time return Time is
   begin
      return Now;
   end Current_Time;

   procedure Check_Time is
   begin
      if Now > Stopping_Time then
         Put_Line (Log, "stopping simulation");
         raise Simulation_Done;
      end if;
   end Check_Time;

   procedure Check_Schedule (E : in out Events.Object'Class) is
   begin
      if E.Event_Time < Now then
         Put_Line (Log, "* attempt to schedule an event in the past: " & E.Name);
         pragma Assert (False);
      elsif E.Event_Time = Time'Last then
         Put_Line (Log, "* attempt to schedule an event at Time'Last: " & E.Name);
         pragma Assert (False);
      elsif E.Enqueued then
         Put_Line (Log, "* attempt to schedule an already-queued event: " & E.Name);
         pragma Assert (False);
      end if;
   end Check_Schedule;

   procedure Check_Cancel (E : in out Events.Object'Class) is
   begin
      if not E.Enqueued then
         Put_Line (Log, "* attempt to cancel an unqueued event: " & E.Name);
         pragma Assert (False);
      end if;
   end Check_Cancel;

   procedure Schedule_Event (E : in out Events.Object'Class) is
   begin
      pragma Debug (Check_Schedule (E));
      Event_Queue.Add (E'Unchecked_Access);
      E.Enqueued := True;
   end Schedule_Event;

   procedure Set_Every_Clock_Event (E : in out Events.Object'Class) is
      use Events;
   begin
      pragma Assert (Every_Clock_Event = null);
      Every_Clock_Event := E'Unchecked_Access;
   end Set_Every_Clock_Event;

   procedure Cancel_Event (E : in out Events.Object'Class) is
   begin
      pragma Debug (Check_Cancel (E));
      Event_Queue.Delete (E'Unchecked_Access);
      E.Event_Time := Time'Last;
      E.Enqueued := False;
   end Cancel_Event;

   --  Debugging support

   procedure Show_Event_Queue (F : File_Type; Long : Boolean := False) is
      procedure Show_Event (E : Events.Class_Ref) is
      begin
         Put (F, " ["); Put (F, E.Name); Put (F, "]");
         if Long then New_Line (F);
         end if;
      end Show_Event;
      procedure Show_All is new Events.Queues.For_All (Show_Event);
   begin
      if Long then
         Put_Line (F, "event_queue: ");
      end if;
      Show_All (Event_Queue);
      if not Long then New_Line (F);
      end if;
   end Show_Event_Queue;

   procedure Trace_Event_Queue (Threshold : Integer;
                               Message : String) is
   begin
      if Debug_Level >= Threshold then
         Put_Line (Log, Message);
         Show_Event_Queue (Log, Long => False);
      end if;
   end Trace_Event_Queue;

   procedure Trace_Event
     (Threshold : Integer;
      E : Events.Class_Ref) is
   begin
      if Debug_Level >= Threshold then
         Put_Line (Log, "   __________"
                     & Trim (Time'Image (Now), Left)
                     & "__________");
         Put (Log, E.all.Name);
         if Debug_Level >= Threshold  then
            Show_Event_Queue (Log, Long => False);
         else
            New_Line (Log);
         end if;
      end if;
   end Trace_Event;

   procedure Stop is
   begin
      Stopping_Time := Now;
   end Stop;

   procedure Run (Latest_Stop_Time : Time) is
   begin
      Run (Latest_Stop_Time, Latest_Stop_Time);
   end Run;

   procedure Initialize is
   begin
      Now := 0;
      --  Do not use Events.Queues.Clear on this queue,
      --  since it does not reset the .Is_Queued flag
      --  of events it removes from the queue.
      Events.Clear_Queue (Event_Queue);
      Every_Clock_Event := null;
   end Initialize;

   procedure Run (Normal_Stop_Time,
                  Latest_Stop_Time : Time) is
      E : Events.Class_Ref;
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
      use Events.Queues;
      use Events;
   begin
      Trace (3, "simulator.run starting");
      Stopping_Time := Normal_Stop_Time;
      Latest_Stopping_Time := Latest_Stop_Time;
      -- Queue simulation
      loop
         --  process events that occur at the same virtual time
         while not Event_Queue.Is_Empty and then
           Front_Of (Event_Queue).Event_Time = Now loop
            --  remove next event from queue
            E := Event_Queue.Front_Of;
            Event_Queue.Pop;
            E.Enqueued := False;
            pragma Debug (Trace_Event (2, E));
            E.Handler;
         end loop;
         if Every_Clock_Event /= null then
            Every_Clock_Event.Handler;
         end if;
         if Event_Queue.Is_Empty then
            pragma Debug (Trace (0, "simulator: empty event queue"));
            exit;
         end if;
         Now := Event_Queue.Front_Of.Event_Time;
         if Now > Latest_Stopping_Time then
            pragma Debug (Trace (0, "simulator: past latest stop time"));
            exit;
         end if;
      end loop;
      Trace_Event_Queue (0,  "simulator: abnormal completion (see log file for details)");
      Put_Line ("simulator : abnormal completion (probable overload)");
   exception
      when Simulation_Done =>
         Trace_Event_Queue (0,  "simulator: normal completion");
      when X : others =>
         Put_Line ("simulator : exceptional completion (see log file for details)");
         Trace_Event_Queue (0,  "simulator: exceptional completion");
         Put (Log, "exception " & Ada.Exceptions.Exception_Name (X));
         Put (Log, " in Simulator.Run at ");
         Put_Line (Log, Ada.Exceptions.Exception_Message (X));
         Put (Log, "last event: ");
         Put (Log, Name (E.all)); New_Line (Log);
         raise;
   end Run;

end Simulator;
