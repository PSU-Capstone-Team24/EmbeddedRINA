with System;
with Interfaces; use Interfaces;
with Ada.Containers.Vectors;
with System.Storage_Elements;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Buffers is
   pragma Preelaborate;

   --  unsigned subtypes copied from ada_enet
   subtype Uint8 is Unsigned_8;
   subtype Uint16 is Unsigned_16;
   subtype Uint32 is Unsigned_32;
   subtype Uint64 is Unsigned_64;

   --  signed subtypes
   subtype Int8 is Short_Short_Integer;
   subtype Int16 is Short_Integer;
   subtype Int32 is Integer;
   subtype Int64 is Long_Long_Integer;

   type Byte is new System.Storage_Elements.Storage_Element;
   type Byte_Buffer is array (Natural range <>) of Byte;
   type Byte_Buffer_Access is access all Byte_Buffer;

   package Byte_Vectors is new Ada.Containers.Vectors (Positive, Byte);
   subtype Byte_Vector is Byte_Vectors.Vector;
   subtype Byte_Cursor is Byte_Vectors.Cursor;

   type Bit is range 0 .. 1 with
     Size => 1;
   type Bit_Array is array (Positive range <>) of Bit;
   pragma Pack (Bit_Array);

   --  Pushes/appends the contents of a byte buffer into an existing byte vector
   procedure Append_Byte_Buffer_To_Vector (Buffer : in Byte_Buffer; Vector : in out Byte_Vector);

   --  Converts a byte buffer to a string consisting of that stream of bytes
   function Buffer_To_Byte_String (Buffer : Byte_Buffer) return String;
  
   --  Converts a byte buffer to a string corresponding to each byte as ASCII characters
   function Buffer_To_String (Buffer : Byte_Buffer) return String;

   --  Converts an unbounded string to a Byte_Vector
   function String_To_Byte_Vector (Input : Unbounded_String) return Byte_Vector;

   --  Converts a byte vector into a byte buffer (byte array)
   function Byte_Vector_To_Buffer (Vector : Byte_Vector) return Byte_Buffer;

   --  Converts a byte buffer (array) into a byte vector
   function Byte_Buffer_To_Vector (Buffer : in Byte_Buffer) return Byte_Vector;

   --  Reverses a byte buffer (for endianness conversion)
   function Buffer_Reverse (Buffer : in Byte_Buffer) return Byte_Buffer;
end Buffers;
