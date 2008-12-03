--  $Id

with Ada.Text_IO; use Ada.Text_IO;
package Error_Log is
   Log : File_Type;
   procedure Open (Prefix : String := "default");
   procedure Close;
   --  idempotent, opens or creates logfile Prefix & ".errors"
   Debug_Level : Integer := 0; --  default is no debugging output
   procedure Debug
     (Threshold : Integer;
      Msg : String);
   --  Phase out use of pragma Debug (Debug...);
   --  in favor of pragma Debug (Trace...);
   --  The latter reads better.
   procedure Trace
     (Threshold : Integer;
      Msg : String) renames Debug;
end Error_Log;
