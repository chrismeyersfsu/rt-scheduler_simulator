--  $Id

with Ada.Text_IO;
package body Error_Log is

   Opened : Boolean := False;

   procedure Open (Prefix : String := "default") is
   begin
      if Opened then return;
      end if;
      Create (Log, Out_File, Prefix & ".log");
   exception when others=>
      Open (Log, Out_File, Prefix & ".log");
   end Open;

   procedure Close is
   begin
      Close (Log);
   end Close;

   procedure Debug
     (Threshold : Integer;
      Msg : String) is
   begin
      if Debug_Level >= Threshold then
         Put_Line (Log, Msg);
      end if;
   end Debug;

end Error_Log;
