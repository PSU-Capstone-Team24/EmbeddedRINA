--  This package exists to support Protobuf encoded
--  messages such as the CDAP ones coming from rLite
--  https://protobuf.dev/programming-guides/encoding/

package body Protobuf is
   use type Ada.Containers.Count_Type;

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

   function Tag_To_Field_Number (Input : Byte) return Byte is
   begin
      --  Drop most significant bit and shift right 3 times
      return Shift_Right (Input and 2#0111_1111#, 3);
   end Tag_To_Field_Number;

   function To_VARINT (V : in Vector) return Uint64 is
      Working_Vector : Vector;

      procedure Remove_MSB (C : Cursor) is
         MSB_Removed : constant Byte := V (C) and 2#0111_1111#;
      begin
         Working_Vector.Append (MSB_Removed);
      end Remove_MSB;

   begin
      --  Note we are checking vector capacity (max elements), not length (total elements)
      if Capacity (V) /= 10 then
         raise Constraint_Error with "Invalid VARINT vector capacity";
      end if;

      if Length (V) > 10 or Length (V) < 1 then
         raise Constraint_Error with "Invalid VARINT vector length";
      end if;

      --  Flip endianness and drop the MSB of each byte
      --  The MSB is just there to tell us whether we’ve reached the end of the number
      V.Reverse_Iterate (Process => Remove_MSB'Access);

      --  TODO: MT Concatenate bytes and unchecked cast to uint64
      return 0;
   end To_VARINT;

end Protobuf;
