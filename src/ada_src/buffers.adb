--  Temp disabling
pragma Style_Checks (Off);

with Debug;
with Ada.Text_IO;
with Exceptions;

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
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Byte Stream: ");

      for Byte of Input loop   
         Ada.Text_IO.Put (ToHex (Byte) & " ");
      end loop;

      Ada.Text_IO.New_Line;
   end Put_Bytes;

   procedure Push_Bytes (Self : Byte_Buffer; To_Push : in T) is
   begin
      Debug.Print("Push_Bytes", "Push", Debug.Info);
      raise Exceptions.Not_Implemented_Exception;
   end Push_Bytes;

   function Buffer_To_String (Buffer : Byte_Buffer) return String is
      --  +1 to account for ASCII.NUL
      Result : String(1 .. Buffer'Length) := (others => ASCII.NUL);
   begin
      for I in Buffer'Range loop
         Result(I - Buffer'First + 1) := Character'Val(Buffer(I));

         --  We've hit a null terminator, stop reading
         exit when Character'Val(Buffer(I)) = ASCII.NUL;
      end loop;

      --  Append null terminator
      Result(Result'Last) := ASCII.NUL;

      return Result;
   end Buffer_To_String;

   function Buffer_Reverse (Buffer : in Byte_Buffer) return Byte_Buffer is
      Return_Buffer : Byte_Buffer(Buffer'First .. Buffer'Last) := (others => 0);
   begin
      for I in reverse Buffer'First .. Buffer'Last loop
         Return_Buffer(I) := Buffer(Buffer'Last - I + Buffer'First);
      end loop;

      return Return_Buffer;
   end Buffer_Reverse;

end Buffers;