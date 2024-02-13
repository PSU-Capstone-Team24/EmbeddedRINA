--  This package exists to support Protobuf encoded
--  messages such as the CDAP ones coming from rLite
--  https://protobuf.dev/programming-guides/encoding/
with Ada.Unchecked_Conversion;
with Ada.Containers;

package body Protobuf is
   use type Ada.Containers.Count_Type;

   function Bit_Array_To_Uint64 is new Ada.Unchecked_Conversion
     (Source => Bit_Array, Target => Uint64);

   function Has_MSB (Input : Byte) return Boolean is
      Result : constant Byte := Input and 2#1000_0000#;
   begin
      return (Result = 128);
   end Has_MSB;

   function Get_Bit_At (Input : Byte; Pos : Natural) return Bit is
      Shifted : constant Byte := Shift_Right (Input, Pos) and 2#0000_0001#;
   begin
      return Bit (Shifted);
   end Get_Bit_At;

   function Tag_To_Wire_Type (Input : Byte) return Wire is
      Wire_Type_Num : constant Byte := Input and 2#0000_0111#;
   begin
      case Wire_Type_Num is
         when 0 =>
            return VARINT;
         when 1 =>
            return I64;
         when 2 =>
            return LEN;
         when 3 =>
            return SGROUP;
         when 4 =>
            return EGROUP;
         when 5 =>
            return I32;
         when others =>
            raise Constraint_Error with "Invalid wire type";
      end case;
   end Tag_To_Wire_Type;

   function VARINT_To_Uint64 (V : in Byte_Vector) return Uint64 is
      Working_Bit_Array   : Bit_Array (1 .. 64) := (others => 0);
   begin
      --  MT: TODO: dropping this requirement for now
      --  Note we are checking vector capacity (max elements), not length (total elements)
      --  if Capacity (V) /= 10 then
      --     raise Constraint_Error with "Invalid VARINT vector capacity";
      --  end if;

      --  This is a good indicator something went horribly wrong or the packet is malformed
      if Byte_Vectors.Length (V) > 10 or Byte_Vectors.Length (V) < 1 then
         raise Constraint_Error with "Invalid VARINT vector length";
      end if;

      --  Flip endianness and drop the MSB of each byte
      --  The MSB is just there to tell us whether we've reached the end of the number
      --  V.Reverse_Iterate (Process => Remove_MSB'Access);

      --  For each byte
      for I in V.First_Index .. V.Last_Index loop
         --  For each bit in this byte, note 1 .. 7 and not 1 .. 8, we ignore the MSB
         for J in 1 .. 7 loop
            Working_Bit_Array ((I - V.First_Index) * 7 + J) :=
              Get_Bit_At (V (I), J - 1);
         end loop;
      end loop;

      --  MT: TODO: May need some idiot proofing
      return Bit_Array_To_Uint64 (Working_Bit_Array);
   end VARINT_To_Uint64;

   function VARINT_To_Uint32 (V : in Byte_Vector) return Uint32 is
      function To_Uint32 is new Ada.Unchecked_Conversion(Uint64, Uint32);
   begin
      return To_Uint32 (VARINT_To_Uint64(V));
   end VARINT_To_Uint32;

   function VARINT_To_Int32 (V : in Byte_Vector) return Int32 is
      function To_Int32 is new Ada.Unchecked_Conversion(Uint64, Int32);
   begin
      return To_Int32 (VARINT_To_Uint64(V));
   end VARINT_To_Int32;

   function VARINT_To_Int64 (V : in Byte_Vector) return Int64 is
      function To_Int64 is new Ada.Unchecked_Conversion(Uint64, Int64);
   begin
      return To_Int64 (VARINT_To_Uint64(V));
   end VARINT_To_Int64;

   --  Different signed types, sint32 and sint64 vs int32 or int64, encode negative integers differently
   --  The implementation of these may be wrong for now...
   function VARINT_To_SInt32 (V : in Byte_Vector) return Int32 is
      function To_Int32 is new Ada.Unchecked_Conversion(Uint64, Int32);
   begin
      return To_Int32 (VARINT_To_Uint64(V));
   end VARINT_To_SInt32;

   function VARINT_To_SInt64 (V : in Byte_Vector) return Int64 is
      function To_Int64 is new Ada.Unchecked_Conversion(Uint64, Int64);
   begin
      return To_Int64 (VARINT_To_Uint64(V));
   end VARINT_To_SInt64;
end Protobuf;
