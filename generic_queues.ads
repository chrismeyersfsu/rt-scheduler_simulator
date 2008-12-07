-- $Id: generic_queues.ads,v 1.3 2008/11/23 01:54:28 baker Exp $

--  priority queue (ordered list)
--  with smallest first (order of increasing size) according to
--  the generic function parameter "<"

--  Ordering is FIFO within equal priorities
--  Beware that this means removing an item and re-inserting it
--  will kick it to the back within its priority group.

--  Queues start out initialized and empty.

--  You can safely clear an empty queue, and safely
--  delete an item that is not in a queue.

--  However, adding an item twice will put two copies into the
--  queue, and then you will need to delete twice.

generic
  type Item_Type is private;
  with function ">" (Left, Right: Item_Type) return Boolean is <>;

package Generic_Queues is

  type Object is tagged private;

  procedure Clear (Queue: in out Object);
  procedure Add (Queue: in out Object; Item: in Item_Type);
  procedure Delete (Queue: in out Object; Item: in Item_Type);
  procedure Pop (Queue: in out Object);

  function Is_Empty (Queue: in Object) return Boolean;
  function Front_Of (Queue: in Object) return Item_Type;
  function Back_Of (Queue: in Object) return Item_Type;

  generic
    with procedure P (Item: in Item_Type);
  procedure For_All (Queue: in Object);

private

  type Cell;
  type Link is access Cell;
  type Object is tagged record
      Front: Link;
      Back : Link;
    end record;

end Generic_Queues;
