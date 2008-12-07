--  $Id: events.ads,v 1.5 2008/11/23 01:54:28 baker Exp $

--  root type for Events class

----------------------
--  Event Ordering  --
----------------------

--  The field Events.Object.Ordinal serves to define an order for
--  events that appear to occur at the same time.

--  Of course, this may amount to a bit of a "cheat" since a true
--  operating system would have some degree of indeterminacy in
--  event ordering.  For example, the timer indicating a
--  replenishment might go off just after the corresponding task
--  has been preempted due to budget exhaustion, or a task
--  might be preempted just before it completes.

--  However, since we are dealing with rather coarse-grained
--  integer time values, the simulation is cleaner if we enforce
--  a consistent ordering on events that the simulation says
--  should occur at the same integer time value.

--  In particular, we can set the ordinal values to arrange
--  that job completions are processed before job arrivals,
--  replenishment events are processed before budget exhaustion
--  timer events, and all replenishment-related events are
--  processed after arrival and completion events.

with Virtual_Times; use Virtual_Times;
with Generic_Queues;
package Events is

   type Object is abstract tagged record

      Event_Time : Time := Time'Last;

      Enqueued : Boolean := False;

      --  Enqueued is intended for debugging,
      --  to catch events being re-queued before they
      --  have occurred, or from being cancelled when
      --  they have not been scheduled.

      Ordinal : Integer := Integer'Last;

      --  Ordinal is to logically order certain kinds
      --  of simultaneous events; use it with care.

   end record;

   type Class_Ref is access all Object'Class;

   procedure Handler (E : in out Object) is abstract;

   --  Handler is called by the simulator to implement
   --  the action associated with the event.


   function Name (E : Object) return String;

   --  Name is used for event-tracing output.

   function ">" (L, R : Class_Ref) return Boolean;

   --  This relation is useful for ordering
   --  events in the following generic queue type.

   package Queues is new Generic_Queues (Class_Ref);

   --  Clear_Queue should be used instead of Q.Clear,
   --  in order to reset E.Enqueued.

   procedure Clear_Queue (Q : in out Queues.Object);


end Events;

