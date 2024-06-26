--  This package exists to support Protobuf encoded
--  messages such as the CDAP ones coming from rLite
--  https://protobuf.dev/programming-guides/encoding/

with Buffers; use Buffers;

package Protobuf is

   --  When a message is encoded, each key-value pair is turned into a record
   --  consisting of the field number, a wire type and a payload.
   --  The wire type tells the parser how big the payload after it is.
   --  This allows old parsers to skip over new fields they don’t understand.
   --  This type of scheme is sometimes called Tag-Length-Value, or TLV.

   --  ID  Name      Used For
   --  -------------------------------------------------------------------------
   --  0   VARINT    int32, int64, uint32, uint64, sint32, sint64, bool, enum
   --  1   I64       fixed64, sfixed64, double
   --  2   LEN       string, bytes, embedded messages, packed repeated fields
   --  3   SGROUP    group start (deprecated)
   --  4   EGROUP    group end (deprecated)
   --  5   I32       fixed32, sfixed32, float

   type Wire is (VARINT, I64, LEN, SGROUP, EGROUP, I32);

   function Tag_To_Wire_Type (Input : Byte) return Wire;
   function Has_MSB (Input : Byte) return Boolean;

   --  Variable-width integers, or varints, are at the core of the wire format.
   --  They allow encoding unsigned 64-bit ints using anywhere between 1 and 10 bytes
   --  Where smaller values use fewer bytes
   function VARINT_To_Uint64 (V : in Byte_Vector) return Uint64;
   function VARINT_To_Uint32 (V : in Byte_Vector) return Uint32;

   function VARINT_To_Int32 (V : in Byte_Vector) return Int32;
   function VARINT_To_Int64 (V : in Byte_Vector) return Int64;

   --  Different signed types, sint32 and sint64 vs int32 or int64, encode negative integers differently
   --  The implementation of these may be wrong for now...
   function VARINT_To_SInt32 (V : in Byte_Vector) return Int32;
   function VARINT_To_SInt64 (V : in Byte_Vector) return Int64;
end Protobuf;
