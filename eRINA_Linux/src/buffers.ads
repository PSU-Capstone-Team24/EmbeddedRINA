--  Temp disabling
pragma Style_Checks (Off);

with Interfaces; use Interfaces;

package Buffers is
   type Byte is new Unsigned_8;
   type Byte_Buffer is array (Natural range <>) of Byte;

   procedure Put_Bytes (Input : Byte_Buffer);

   generic
      type T is private;
   procedure Push_Bytes (Self : Byte_Buffer; To_Push : in T);

   function Buffer_To_String (Buffer : Byte_Buffer) return String;
   function Buffer_Reverse (Buffer : in Byte_Buffer) return Byte_Buffer;
end Buffers;
