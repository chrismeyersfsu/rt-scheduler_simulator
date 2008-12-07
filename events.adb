--  $Id: events.adb,v 1.4 2008/11/23 01:54:28 baker Exp $

with Error_Log; use Error_Log;
package body Events is

   function Name (E : Object) return String is
   begin
      return "@" & Time'Image (E.Event_Time);
   end Name;

   function ">" (L, R : Class_Ref) return Boolean is
   begin
      if L.Event_Time > R.Event_Time then
         return True;
      elsif L.Event_Time < R.Event_Time then
         return False;
      else
         return L.Ordinal > R.Ordinal;
      end if;
   end ">";

   procedure Clear_Queue (Q : in out Queues.Object) is
      E : Events.Class_Ref;
   begin
      while not Q.Is_Empty loop
         E := Q.Front_Of;
         E.Enqueued := False;
         pragma Debug (Trace (3, Name (E.all) & " reset"));
         Q.Pop;
      end loop;
   end Clear_Queue;

end Events;
