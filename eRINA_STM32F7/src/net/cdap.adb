with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Protobuf;    use Protobuf;
with Interfaces;  use Interfaces;
with Debug;

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
     (Self : in out Obj_Value; Field : Obj_Value_Field; Val : Uint64) is
   begin
      case Field is
         when Intval =>
            Self.Intval := Uint32 (Val);
         when Sintval =>
            Self.Sintval := Int32 (Val);
         when Int_64val =>
            Self.Int_64val := Val;
         when Sint_64val =>
            Self.Sint_64val := Int64 (Val);
         when Floatval =>
            Self.Floatval := Short_Float (Val);
         when Doubleval =>
            Self.Doubleval := Long_Long_Float (Val);
         when others =>
            --  MT: TODO: Should we make this throw an exception?
            null;
      end case;
   end Set_Field;

   procedure Set_Field
     (Self : in out Obj_Value; Field : Obj_Value_Field; Val : Byte_Vector)
   is
      Final_String           : constant String := Buffer_To_String (Byte_Vector_To_Buffer (Val));
      Final_String_Unbounded : constant Unbounded_String :=
        To_Unbounded_String (Final_String);
   begin
      
      if Field = Strval then
         Self.Strval := Final_String_Unbounded;
      elsif Field = Byteval then
         Self.Byteval := Val;
      end if;

   end Set_Field;

   procedure Set_Field
     (Self : in out CDAPMessage; Field : CDAP_Field; Val : Byte_Vector)
   is
      Final_String           : constant String := Buffer_To_String (Byte_Vector_To_Buffer (Val));
      Final_String_Unbounded : constant Unbounded_String :=
        To_Unbounded_String (Final_String);
   begin
      
      if Field = ObjValue then
         Self.ObjValue := To_OBJ_Value (Val);
      end if;

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
     (Self : in out CDAPMessage; Field : CDAP_Field; Val : Auth_Value)
   is
   begin
      null;
   end Set_Field;

   procedure Set_Field
     (Self : in out Obj_Value; Field : Obj_Value_Field; Val : Unbounded_String) is
   begin
      null;
   end Set_Field;

   procedure Set_Field
     (Self : in out Obj_Value; Field : Obj_Value_Field; Val : Boolean) is
   begin
      null;
   end Set_Field;

   --  MT: TODO: I really don't like the code duplication here, optimize me later
   function To_OBJ_Value (V : in Byte_Vector) return Obj_Value is
      ObjValue     : Obj_Value;
      Wire_Type    : Wire;
      Field_Id     : Obj_Value_Field;
      Is_Tag_Field : Boolean := True;

      C : Byte_Vectors.Cursor := V.First;
      use type Byte_Vectors.Cursor;
   begin
      while C /= Byte_Vectors.No_Element loop
         if Is_Tag_Field then
            declare
               Tag_Vector : Byte_Vector;
            begin
               while C /= Byte_Vectors.No_Element loop
                  Tag_Vector.Append (V(C));
                  exit when (not Has_MSB (V(C)));
                  C := Byte_Vectors.Next (C);
               end loop;

               Field_Id  := Tag_To_OBJ_Value_Field (Tag_Vector);
               Wire_Type := Tag_To_Wire_Type (Tag_Vector.First_Element);
            end;

            Is_Tag_Field      := False;
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
                  ObjValue.Set_Field (Field_Id, VARINT_To_Uint64 (VARINT_Vector));
               end;
            end if;

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
                  LEN_Length := Natural (VARINT_To_Uint64 (LEN_Vector));

                  if LEN_Length > 0 then
                     while C /= Byte_Vectors.No_Element loop
                        Data_Vector.Append (V (C));
                        LEN_Iterator := LEN_Iterator + 1;
                        exit when (LEN_Iterator = LEN_Length);
                        C := Byte_Vectors.Next (C);
                     end loop;

                     ObjValue.Set_Field (Field_Id, Data_Vector);
                  else
                     C := Byte_Vectors.Next (C);
                  end if;
               end;
            end if;
         end if;
         
         C := Byte_Vectors.Next (C);
      end loop;

      return ObjValue;
   end To_OBJ_Value;

   function Tag_To_OBJ_Value_Field (Input : Byte_Vector) return Obj_Value_Field is
      Value : constant Uint64 := VARINT_To_Uint64(Input) / 2 ** 3;
   begin
      Debug.Print(Debug.Info, "Obj_Value_Field: " & VARINT_To_Uint64(Input)'Image);
      --  MT: TODO: Need to handle weird case when resulting value does not match an enum in Obj_Value
      return Obj_Value_Field'Enum_Val (Value);
   end Tag_To_OBJ_Value_Field;

   function Tag_To_CDAP_Field (Input : Byte_Vector) return CDAP_Field is
      Value : constant Uint64 := VARINT_To_Uint64(Input) / 2 ** 3;
   begin
      Debug.Print(Debug.Info, Input.First_Element'Image);
      Debug.Print(Debug.Info, "Tag_To_CDAP_Field: " & VARINT_To_Uint64(Input)'Image);
      --  MT: TODO: Need to handle weird case when resulting value does not match an enum in CDAP_Field
      return CDAP_Field'Enum_Val (Value);
   end Tag_To_CDAP_Field;

   procedure Put (Self : CDAPMessage) is begin
      Put_Line ("Message Contents:");
      
      --  Abs_Syntax
      Put (Head ("Abs_Syntax:", 15, ' '));
      Put_Line (Self.Abs_Syntax'Image);

      --  OpCode
      Put (Head ("OpCode:", 16, ' '));
      Put_Line (Self.OpCode'Image);

      --  Invoke_Id
      Put (Head ("Invoke_Id:", 15, ' '));
      Put_Line (Self.Invoke_Id'Image);

      --  Flags
      Put (Head ("Flags:", 16, ' '));
      Put_Line (Self.Flags'Image);

      --  Obj_Class
      Put (Head ("Obj_Class:", 16, ' '));
      Put_Line ("'" & To_String(Self.Obj_Class) & "'");

      --  Obj_Name
      Put (Head ("Obj_Name:", 16, ' '));
      Put_Line ("'" & To_String(Self.Obj_Name) & "'");

      --  Obj_Inst
      Put (Head ("Obj_Inst:", 15, ' '));
      Put_Line (Self.Obj_Inst'Image);

      --  Obj_Value
      Put_Line ("Obj_Value:");

      --  Obj_Value.IntVal
      Put (Head("  IntVal:", 15, ' '));
      Put_Line (Self.ObjValue.Intval'Image);

      --  Obj_Value.IntVal
      Put (Head("  Sintval:", 15, ' '));
      Put_Line (Self.ObjValue.Sintval'Image);

      --  Obj_Value.Int_64val
      Put (Head("  Int_64val:", 15, ' '));
      Put_Line (Self.ObjValue.Int_64val'Image);

      --  Obj_Value.Sint_64val
      Put (Head("  Sint_64val:", 15, ' '));
      Put_Line (Self.ObjValue.Sint_64val'Image);

      --  Obj_Value.Strval
      Put (Head("  Strval:", 16, ' '));
      Put_Line ("'" & To_String(Self.ObjValue.Strval) & "'");

      --  Obj_Value.Byteval
      Put (Head("  Byteval:", 16, ' '));

      case Self.ObjValue.Byteval.Length is
         when 0 =>
            Put_Line("N/A");
         when others => null;
            --  Put_Bytes(Byte_Vector_To_Buffer (Self.ObjValue.Byteval));
      end case;

      --  Obj_Value.Floatval
      Put (Head("  Floatval:", 15, ' '));
      Put_Line (Self.ObjValue.Floatval'Image);

      --  Obj_Value.Doubleval
      Put (Head("  Doubleval:", 15, ' '));
      Put_Line (Self.ObjValue.Doubleval'Image);

      --  Obj_Value.Boolval
      Put (Head("  Boolval:", 16, ' '));
      Put_Line (Self.ObjValue.Boolval'Image);

      --  Result
      Put (Head ("Result:", 15, ' '));
      Put_Line (Self.Result'Image);

      --  Scope
      Put (Head ("Scope:", 15, ' '));
      Put_Line (Self.Scope'Image);

      --  Filter
      Put (Head ("Filter:", 15, ' '));
      Put_Line (Self.Filter'Image);

      --  Auth_Mech
      Put (Head ("Auth_Mech:", 16, ' '));
      Put_Line ("'" & To_String(Self.Auth_Mech) & "'");

      --  Dest_Ae_Inst
      Put (Head ("Dest_Ae_Inst:", 16, ' '));
      Put_Line ("'" & To_String(Self.Dest_Ae_Inst) & "'");

      --  Dest_Ae_Name
      Put (Head ("Dest_Ae_Name:", 16, ' '));
      Put_Line ("'" & To_String(Self.Dest_Ae_Name & "'"));

      --  Dest_Ap_Inst
      Put (Head ("Dest_Ap_Inst:", 16, ' '));
      Put_Line ("'" & To_String(Self.Dest_Ap_Inst & "'"));

      --  Dest_Ap_Name
      Put (Head ("Dest_Ap_Name:", 16, ' '));
      Put_Line ("'" & To_String(Self.Dest_Ap_Name & "'"));

      --  Src_Ae_Inst
      Put (Head ("Src_Ae_Inst:", 16, ' '));
      Put_Line ("'" & To_String(Self.Src_Ae_Inst & "'"));

      --  Src_Ae_Name
      Put (Head ("Src_Ae_Name:", 16, ' '));
      Put_Line ("'" & To_String(Self.Src_Ae_Name & "'"));

      --  Src_Ap_Inst
      Put (Head ("Src_Ap_Inst:", 16, ' '));
      Put_Line ("'" & To_String(Self.Src_Ap_Inst & "'"));

      --  Src_Ap_Name
      Put (Head ("Src_Ap_Name:", 16, ' '));
      Put_Line ("'" & To_String(Self.Src_Ap_Name & "'"));

      --  Result_Reason
      Put (Head ("Result_Reason:", 16, ' '));
      Put_Line ("'" & To_String(Self.Result_Reason & "'"));

      --  Result_Reason
      Put (Head ("Result_Reason:", 15, ' '));
      Put_Line (Self.Version'Image);

   end Put;

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
            declare
               Tag_Vector : Byte_Vector;
            begin
               while C /= Byte_Vectors.No_Element loop
                  Tag_Vector.Append (V(C));
                  exit when (not Has_MSB (V(C)));
                  C := Byte_Vectors.Next (C);
               end loop;

               Field_Id  := Tag_To_CDAP_Field (Tag_Vector);
               Wire_Type := Tag_To_Wire_Type (Tag_Vector.First_Element);

               Debug.Print(Debug.Info, "Field_Id " & Field_Id'Image & " Wire_Type " & Wire_Type'Image);
            end;

            Is_Tag_Field      := False;
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
                  Result_Msg.Set_Field (Field_Id, VARINT_To_Uint64 (VARINT_Vector));
                  Debug.Print(Debug.Info, "VARINT Done!");
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
                  LEN_Length := Natural (VARINT_To_Uint64 (LEN_Vector));
                  
                  if LEN_Length > 0 then
                     while C /= Byte_Vectors.No_Element loop
                        Data_Vector.Append (V (C));
                        LEN_Iterator := LEN_Iterator + 1;
                        exit when (LEN_Iterator = LEN_Length);
                        C := Byte_Vectors.Next (C);
                     end loop;

                     Result_Msg.Set_Field (Field_Id, Data_Vector);
                     Debug.Print(Debug.Info, "LEN Done!");
                  else
                     C := Byte_Vectors.Next (C);
                  end if;
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
