--  This package exists to support Protobuf encoded
--  messages such as the CDAP ones coming from rLite
--  https://protobuf.dev/programming-guides/encoding/

package body Protobuf is
   use type Ada.Containers.Count_Type;
   
   function Has_MSB (Input : Byte) return Boolean is
      Result : constant Byte := input and 2#1000_0000#;
   begin
      return (Result = 1);
   end Has_MSB;

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

   function Tag_To_Field_Number (Input : Byte) return CDAP_Field is
      --  Drop most significant bit and shift right 3 times
      Value : constant Byte := Shift_Right (Input and 2#0111_1111#, 3);
   begin
      --  MT: TODO: Need to handle weird case when resulting value does not match an enum in CDAP_Field
      return CDAP_Field'Val (Value);
   end Tag_To_Field_Number;

   function To_VARINT (V : in Byte_Vector) return Uint64 is
      Working_Vector : Byte_Vector;

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

   function To_CDAP(V : in Vector) return CDAPMessage is
      Result_Msg : CDAPMessage;
      Wire_Type : Wire;
      Field_Id : CDAP_Field;
      Is_Tag_Field : Boolean := True;

      --  Using a cursor here instead of an iterative loop so we can skip elements
      C : Cursor := V.First;
   begin
      while C /= No_Element loop
         if Is_Tag_Field then
            Wire_Type := Tag_To_Wire_Type (V(C));
            Field_Id := Tag_To_Field_Number (V(C));
            Is_Tag_Field := False;
         else
            if Wire_Type = VARINT then

               declare
                  VARINT_Vector : Byte_Vector;
               begin
                  --  Keep reading bytes until we no longer have a MSB of 1
                  while C /= No_Element loop
                     VARINT_Vector.Append (V(C));
                     exit when (not Has_MSB (V(C)));
                     C := Next (C);
                  end loop;
               end;

            end if;

            --  Next field will be a tag field
            Is_Tag_Field := True;
         end if;

         C := Next (C);
      end loop;

      return Result_Msg;
   end To_CDAP;
end Protobuf;
