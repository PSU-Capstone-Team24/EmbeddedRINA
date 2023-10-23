--  Temp disabling
pragma Style_Checks (Off);

with Ada.Text_IO;

package body Debug is

   procedure Print (Msg : String) is
      DEBUG : constant Boolean := true;
   begin
      if DEBUG then
         Ada.Text_IO.Put_Line (Msg);
      end if;
   end Print;

end Debug;