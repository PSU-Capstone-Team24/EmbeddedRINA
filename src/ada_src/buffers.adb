--  Temp disabling
pragma Style_Checks (Off);

with Debug;
with Ada.Text_IO;

package body Buffers is

   procedure Put_Bytes (Input : Byte_Buffer) is
      function ToHex(Val : Byte) return String is
         Str : String := "0x00";
         First_Digit : Byte := ((Val / 16) mod 16) + 48;
         Second_Digit : Byte := (Val mod 16) + 48;
      begin
         --  ASCII offset so we go from 0-9, then jump to A-F
         if First_Digit > 57 then
            First_Digit := First_Digit + 7;
         end if;

         --  ASCII offset so we go from 0-9, then jump to A-F
         if Second_Digit > 57 then
            Second_Digit := Second_Digit + 7;
         end if;

         -- Replace chars in hex string
         Str(3) := Character'Val(First_Digit);
         Str(4) := Character'Val(Second_Digit);

         return Str;
      end ToHex;
   begin
      Ada.Text_IO.Put ("Byte Stream: ");

      for Byte of Input loop   
         Ada.Text_IO.Put (ToHex (Byte) & " ");
      end loop;

      Ada.Text_IO.New_Line;
   end Put_Bytes;

   procedure Push_Bytes (Self : Byte_Buffer; To_Push : in T) is
   begin
      Debug.Print("Push_Bytes", "Push", Debug.Info);
   end Push_Bytes;

end Buffers;