with Ada.Text_IO; use Ada.Text_IO;
with Protobuf;    use Protobuf;

package body CDAP is

   --  For setting VARINT fields
   procedure Set_Field
     (Self : in out CDAPMessage; Field : CDAP_Field; Val : Uint64)
   is
      Val_As_Uint32 : constant Uint32 := Uint32 (Val);
   begin
      case Field is
         when Abs_Syntax =>
            Self.Abs_Syntax := Val_As_Uint32;
         when Invoke_Id =>
            Self.Invoke_Id := Val_As_Uint32;
         when Result =>
            Self.Result := Val_As_Uint32;
         when Scope =>
            Self.Scope := Val_As_Uint32;
         when Filter =>
            Self.Filter := Val_As_Uint32;
         when Version =>
            Self.Version := Val;
         when others =>
            --  MT: TODO: Should we make this throw an exception?
            null;
      end case;
   end Set_Field;

   procedure Set_Field
     (Self : in out CDAPMessage; Field : CDAP_Field; Val : Byte_Buffer)
   is
      Final_String           : constant String := Buffer_To_String (Val);
      Final_String_Unbounded : constant Unbounded_String :=
        To_Unbounded_String (Final_String);
   begin
      case Field is
         when Obj_Class =>
            Self.Obj_Class := Final_String_Unbounded;
         when Obj_Name =>
            Self.Obj_Name := Final_String_Unbounded;
         when Auth_Mech =>
            Self.Auth_Mech := Final_String_Unbounded;
         when Dest_Ae_Inst =>
            Self.Dest_Ae_Inst := Final_String_Unbounded;
         when Dest_Ae_Name =>
            Self.Dest_Ae_Name := Final_String_Unbounded;
         when Dest_Ap_Inst =>
            Self.Dest_Ap_Inst := Final_String_Unbounded;
         when Dest_Ap_Name =>
            Self.Dest_Ap_Name := Final_String_Unbounded;
         when Src_Ae_Inst =>
            Self.Src_Ae_Inst := Final_String_Unbounded;
         when Src_Ae_Name =>
            Self.Src_Ae_Name := Final_String_Unbounded;
         when Src_Ap_Inst =>
            Self.Src_Ap_Inst := Final_String_Unbounded;
         when Src_Ap_Name =>
            Self.Src_Ap_Name := Final_String_Unbounded;
         when Result_Reason =>
            Self.Result_Reason := Final_String_Unbounded;
         when others =>
            --  MT: TODO: Should we make this throw an exception?
            null;
      end case;
   end Set_Field;

   procedure Set_Field
     (Self : in out CDAPMessage; Field : CDAP_Field; Val : Op_Code)
   is
   begin
      null;
   end Set_Field;

   procedure Set_Field
     (Self : in out CDAPMessage; Field : CDAP_Field; Val : CDAPFlags)
   is
   begin
      null;
   end Set_Field;

   procedure Set_Field
     (Self : in out CDAPMessage; Field : CDAP_Field; Val : Obj_Value)
   is
   begin
      null;
   end Set_Field;

   procedure Set_Field
     (Self : in out CDAPMessage; Field : CDAP_Field; Val : Auth_Value)
   is
   begin
      null;
   end Set_Field;

   function Tag_To_Field_Number (Input : Byte) return CDAP_Field is
      --  Drop most significant bit and shift right 3 times
      Value : constant Byte := Shift_Right (Input and 2#0111_1111#, 3);
   begin
      --  MT: TODO: Debug only! Remove Me!
      Put_Line
        ("Decoded tagged field :: " & CDAP_Field'Enum_Val (Value)'Image);

      --  MT: TODO: Need to handle weird case when resulting value does not match an enum in CDAP_Field
      return CDAP_Field'Enum_Val (Value);
   end Tag_To_Field_Number;

   function To_CDAP (V : in Byte_Vector) return CDAPMessage is
      Result_Msg   : CDAPMessage;
      Wire_Type    : Wire;
      Field_Id     : CDAP_Field;
      Is_Tag_Field : Boolean := True;

      --  Using a cursor here instead of an iterative loop so we can skip elements
      C : Byte_Vectors.Cursor := V.First;
      use type Byte_Vectors.Cursor;
   begin
      while C /= Byte_Vectors.No_Element loop
         if Is_Tag_Field then
            Wire_Type    := Tag_To_Wire_Type (V (C));
            Field_Id     := Tag_To_Field_Number (V (C));
            Is_Tag_Field := False;
         else
            if Wire_Type = VARINT then
               declare
                  VARINT_Vector : Byte_Vector;
               begin
                  --  Keep reading bytes until we no longer have a MSB of 1
                  while C /= Byte_Vectors.No_Element loop
                     VARINT_Vector.Append (V (C));
                     exit when (not Has_MSB (V (C)));
                     C := Byte_Vectors.Next (C);
                  end loop;

                  --  Decode and update message
                  Result_Msg.Set_Field (Field_Id, To_VARINT (VARINT_Vector));
               end;
            end if;

            --  LEN can be a string, embedded messages, packed repeated fields, or even just an array of bytes
            --  For our purposes we always return a vector of bytes, then let the known tag field dictate what this
            --  should turn into...
            if Wire_Type = LEN then
               declare
                  LEN_Vector   : Byte_Vector;
                  Data_Vector  : Byte_Vector;
                  LEN_Length   : Natural := 0;
                  LEN_Iterator : Natural := 0;
               begin
                  while C /= Byte_Vectors.No_Element loop
                     LEN_Vector.Append (V (C));
                     C := Byte_Vectors.Next (C);
                     exit when (not Has_MSB (V (C)));
                  end loop;

                  --  The VARINT storing the length is an int32
                  LEN_Length := Natural (To_VARINT (LEN_Vector));

                  while C /= Byte_Vectors.No_Element loop
                     Data_Vector.Append (V (C));
                     LEN_Iterator := LEN_Iterator + 1;
                     exit when (LEN_Iterator = LEN_Length);
                     C := Byte_Vectors.Next (C);
                  end loop;

                  Put_Line
                    ("Decoded LEN of size (" & LEN_Length'Image &
                     ") to be :: " &
                     Buffer_To_String (Byte_Vector_To_Buffer (Data_Vector)));
                  Result_Msg.Set_Field
                    (Field_Id, Byte_Vector_To_Buffer (Data_Vector));
               end;
            end if;

            --  Next field will be a tag field
            Is_Tag_Field := True;
         end if;

         C := Byte_Vectors.Next (C);
      end loop;

      return Result_Msg;
   end To_CDAP;

end CDAP;
