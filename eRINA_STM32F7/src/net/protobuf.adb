--  This package exists to support Protobuf encoded
--  messages such as the CDAP ones coming from rLite
--  https://protobuf.dev/programming-guides/encoding/

with Buffers; use Buffers;

package body Protobuf is

   function Tag_To_Wire_Type (Input : Byte) return Wire is
      Wire_Type_Num : Byte := Input and 2#111#;
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
      return Shift_Right (Input and 2#01111111#, 3);
   end Tag_To_Field_Number;

end Protobuf;