--  Temp disabling
pragma Style_Checks (Off);

with Debug;
with System;
with Ada.Strings;
with Ada.Strings.Bounded;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

package body Bindings.Rlite.Kernel_Msg is
   procedure Print_Bytes (V : T) is
      Bytes : Byte_Buffer (1 .. V'Size / 8)
         with Address => V'Address, Import, Volatile;
      
      function ToHex(Val : Unsigned_8) return String is
         Str : String := "0x00";
         First_Digit : Unsigned_8 := ((Val / 16) mod 16) + 48;
         Second_Digit : Unsigned_8 := (Val mod 16) + 48;
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

      for Byte of Bytes loop   
         Ada.Text_IO.Put (ToHex (Byte) & " ");
      end loop;

      Ada.Text_IO.New_Line;
   end Print_Bytes;

   function Serialize (V : T; print : Boolean) return Byte_Buffer is
      Bytes : Byte_Buffer (0 .. V'Size / 8)
         with Address => V'Address, Import, Volatile;

      type Message_Buffer is new Byte_Buffer(1 .. V'Size / 8 - 7);
      procedure Put_Bytes is new Print_Bytes (T => Message_Buffer);
   begin
      if print then
         Put_Bytes (Message_Buffer (Bytes(8 .. V'Size / 8)));
      end if;
      
      return Bytes(8 .. V'Size / 8);
   end Serialize;

end Bindings.Rlite.Kernel_Msg;