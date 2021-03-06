-- $Id: generic_queues.adb,v 1.3 2008/11/20 18:49:43 baker Exp $

--  priority queue (ordered list)
--  with smallest first (order of increasing size) according to
--  the generic function parameter "<"

--  Ordering is FIFO within equal priorities
--  Beware that this means removing an item and re-inserting it
--  will kick it to the back within its priority group.

package body Generic_Queues is

   type Cell is
     record
       Item: Item_Type;
       Next: Link;
     end record;

   Avail: Link:= null;

   function Is_Empty (Queue: in Object) return Boolean is
   begin
     return Queue.Front = null;
   end Is_Empty;

   procedure Clear (Queue: in out Object) is
   begin
     if not Is_Empty (Queue) then
       Queue.Back.Next := Avail;
       Avail := Queue.Front;
     end if;
     Queue := Object'(Front => null, Back  => null);
   end Clear;

   procedure Add (Queue: in out Object; Item: in Item_Type) is
     Previous : Link := null;
     Index    : Link := Queue.Front;
   begin
     if Is_Empty (Queue) then
       if Avail = null then Queue.Front:= new Cell;
       else Queue.Front:= Avail; Avail:= Avail.Next; end if;
       Queue.Front.Item:= Item;
       Queue.Front.Next:= Null;
       Queue.Back:= Queue.Front;
     else
        while (Index /= null) and then
          ((Item > Index.Item) or else (Item = Index.Item)) loop
         Previous:= Index;
         Index:= Index.Next;
       end loop;
       if Previous = null then -- add at the front
         if Avail = null then Queue.Front:= new Cell;
         else Queue.Front:= Avail; Avail:= Avail.Next; end if;
         Queue.Front.Item:= Item;
         Queue.Front.Next:= Index;
       elsif Index = null then -- add at the back
         if Avail = null then Queue.Back.Next:= new Cell;
         else Queue.Back.Next:= Avail; Avail:= Avail.Next; end if;
         Queue.Back.Next.Item:= Item;
         Queue.Back.Next.Next:= null;
         Queue.Back:= Queue.Back.Next;
       else -- add immediately after last item <= one to be added
         if Avail = null then
             Previous.Next:= New Cell;
         else
            Previous.Next:= Avail; Avail:= Avail.Next;
         end if;
         Previous.Next.Item:= Item;
         Previous.Next.Next:= Index;
       end if;
     end if;
   end Add;

   procedure Delete(Queue: in out Object; Item: in Item_Type) is
     Previous: Link;
     Index: Link:= Queue.Front;
   begin
     while (Index /= null) and then (Item /= Index.Item) loop
       Previous:= Index;
       Index := Index.Next;
     end loop;
     if Index = null then return;
     elsif Previous = null then Queue.Front:= Index.Next;
     else Previous.Next:= Index.Next; end if;
     if Queue.Back = Index then Queue.Back:= Previous; end if;
     if Index /= null then Index.Next:= Avail; Avail:= Index; end if;
   end Delete;

   procedure Pop (Queue: in out Object) is
     Temp: Link:= Queue.Front;
   begin
     if not Is_Empty(Queue) then
       Queue.Front:= Queue.Front.Next;
       if Queue.Front = null then Queue.Back:= null; end if;
       Temp.Next:= Avail; Avail:= Temp;
     end if;
   end Pop;

   function Front_Of (Queue: in Object) return Item_Type is
   begin
      if Is_Empty (Queue) then
         raise Program_Error;
      end if;
      return Queue.Front.Item;
   end Front_Of;

   function Back_Of(Queue: in Object) return Item_Type is
   begin
     if Is_Empty(Queue) then raise Program_Error; end if;
     return Queue.Back.Item;
   end Back_Of;

   procedure For_All (Queue: in Object) is
     Index: Link := Queue.Front;
   begin
     while (Index /= null) loop
       P (Index.Item);
       Index:= Index.Next;
     end loop;
   end For_All;

end Generic_Queues;
