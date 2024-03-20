with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces;        use Interfaces;
with Debug;

package body CDAP is

   function Tag_To_OBJ_Value_Field (Input : Byte_Vector) return Obj_Value_Field
   is
      Value : constant Uint64 := VARINT_To_Uint64 (Input) / 2**3;
   begin
      --  MT: TODO: Need to handle weird case when resulting value does not match an enum in Obj_Value
      return Obj_Value_Field'Enum_Val (Value);
   end Tag_To_OBJ_Value_Field;

   function Tag_To_CDAP_Field (Input : Byte_Vector) return CDAP_Field is
      Value : constant Uint64 := VARINT_To_Uint64 (Input) / 2**3;
   begin
      --  MT: TODO: Need to handle weird case when resulting value does not match an enum in CDAP_Field
      return CDAP_Field'Enum_Val (Value);
   end Tag_To_CDAP_Field;

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
                  Tag_Vector.Append (V (C));
                  exit when (not Has_MSB (V (C)));
                  C := Byte_Vectors.Next (C);
               end loop;

               Field_Id  := Tag_To_OBJ_Value_Field (Tag_Vector);
               Wire_Type := Tag_To_Wire_Type (Tag_Vector.First_Element);
            end;

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
                  ObjValue.Set_Field
                    (Field_Id, VARINT_To_Uint64 (VARINT_Vector));
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
                  end if;
               end;
            end if;
         end if;

         C := Byte_Vectors.Next (C);
      end loop;

      return ObjValue;
   end To_OBJ_Value;

   --  For setting VARINT fields
   procedure Set_Field
     (Self : in out CDAPMessage; Field : CDAP_Field; Val : Uint64)
   is
      Val_As_Uint32 : constant Uint32 := Uint32 (Val);
   begin
      Self.Set_Fields.Append (Field);

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

   procedure Clear_Fields (Self : in out Obj_Value) is
   begin
      Self.Set_Fields.Clear;
   end Clear_Fields;

   procedure Set_Field
     (Self : in out Obj_Value; Field : Obj_Value_Field; Val : Uint64)
   is
   begin
      Self.Set_Fields.Append (Field);

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
   begin
      Self.Set_Fields.Append (Field);

      if Field = Strval then
         declare
            Final_String : constant String :=
              Buffer_To_String (Byte_Vector_To_Buffer (Val));
            Final_String_Unbounded : constant Unbounded_String :=
              To_Unbounded_String (Final_String);
         begin
            Self.Strval := Final_String_Unbounded;
         end;
      elsif Field = Byteval then
         Self.Byteval := Val;
      end if;

   end Set_Field;

   procedure Clear_Fields (Self : in out CDAPMessage) is
   begin
      Self.Set_Fields.Clear;
   end Clear_Fields;

   procedure Set_Field
     (Self : in out CDAPMessage; Field : CDAP_Field; Val : Byte_Vector)
   is
      Final_String : constant String :=
        Buffer_To_String (Byte_Vector_To_Buffer (Val));
      Final_String_Unbounded : constant Unbounded_String :=
        To_Unbounded_String (Final_String);
   begin
      Self.Set_Fields.Append (Field);

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
      Self.Set_Fields.Append (Field);
      Self.OpCode := Val;
   end Set_Field;

   --  MT: TODO: Implement these as required
   procedure Set_Field
     (Self : in out CDAPMessage; Field : CDAP_Field; Val : CDAPFlags)
   is
   begin
      Self.Set_Fields.Append (Field);
      null;
   end Set_Field;

   procedure Set_Field
     (Self : in out CDAPMessage; Field : CDAP_Field; Val : Auth_Value)
   is
   begin
      Self.Set_Fields.Append (Field);
      null;
   end Set_Field;

   procedure Set_Field
     (Self : in out Obj_Value; Field : Obj_Value_Field; Val : String)
   is
   begin
      Self.Set_Fields.Append (Field);
      Self.Strval := To_Unbounded_String (Val);
   end Set_Field;

   procedure Set_Field
     (Self : in out Obj_Value; Field : Obj_Value_Field; Val : Boolean)
   is
   begin
      Self.Set_Fields.Append (Field);
      null;
   end Set_Field;

   procedure Put (Self : CDAPMessage) is
   begin
      Debug.Print (Debug.Info, "Message Contents:");

      --  Abs_Syntax
      if Self.Set_Fields.Contains (Abs_Syntax) then
         Debug.Print
           (Debug.Info, Head ("Abs_Syntax:", 15, ' ') & Self.Abs_Syntax'Image);
      end if;

      --  OpCode
      if Self.Set_Fields.Contains (OpCode) then
         Debug.Print
           (Debug.Info, Head ("OpCode:", 16, ' ') & Self.OpCode'Image);
      end if;

      --  Invoke_Id
      if Self.Set_Fields.Contains (Invoke_Id) then
         Debug.Print
           (Debug.Info, Head ("Invoke_Id:", 15, ' ') & Self.Invoke_Id'Image);
      end if;

      --  Flags
      if Self.Set_Fields.Contains (Flags) then
         Debug.Print (Debug.Info, Head ("Flags:", 16, ' ') & Self.Flags'Image);
      end if;

      --  Obj_Class
      if Self.Set_Fields.Contains (Obj_Class) then
         Debug.Print
           (Debug.Info,
            Head ("Obj_Class:", 16, ' ') & "'" & To_String (Self.Obj_Class) &
            "'");
      end if;

      --  Obj_Name
      if Self.Set_Fields.Contains (Obj_Name) then
         Debug.Print
           (Debug.Info,
            Head ("Obj_Name:", 16, ' ') & "'" & To_String (Self.Obj_Name) &
            "'");
      end if;

      --  Obj_Inst
      if Self.Set_Fields.Contains (Obj_Inst) then
         Debug.Print
           (Debug.Info, Head ("Obj_Inst:", 15, ' ') & Self.Obj_Inst'Image);
      end if;

      --  Obj_Value
      if False then
         Debug.Print (Debug.Info, "Obj_Value:");

         --  Obj_Value.IntVal
         Debug.Print
           (Debug.Info,
            Head ("  IntVal:", 15, ' ') & Self.ObjValue.Intval'Image);

         --  Obj_Value.IntVal
         Debug.Print
           (Debug.Info,
            Head ("  Sintval:", 15, ' ') & Self.ObjValue.Sintval'Image);

         --  Obj_Value.Int_64val
         Debug.Print
           (Debug.Info,
            Head ("  Int_64val:", 15, ' ') & Self.ObjValue.Int_64val'Image);

         --  Obj_Value.Sint_64val
         Debug.Print
           (Debug.Info,
            Head ("  Sint_64val:", 15, ' ') & Self.ObjValue.Sint_64val'Image);

         --  Obj_Value.Strval
         Debug.Print
           (Debug.Info,
            Head ("  Strval:", 16, ' ') & "'" &
            To_String (Self.ObjValue.Strval) & "'");

         --  Obj_Value.Byteval
         Debug.Print (Debug.Info, Head ("  Byteval:", 16, ' '));

         case Self.ObjValue.Byteval.Length is
            when 0 =>
               Debug.Print (Debug.Info, "N/A");
            when others =>
               Debug.Print
                 (Debug.Info,
                  Buffer_To_Byte_String
                    (Byte_Vector_To_Buffer (Self.ObjValue.Byteval)));
         end case;

         --  Obj_Value.Floatval
         Debug.Print
           (Debug.Info,
            Head ("  Floatval:", 15, ' ') & Self.ObjValue.Floatval'Image);

         --  Obj_Value.Doubleval
         Debug.Print
           (Debug.Info,
            Head ("  Doubleval:", 15, ' ') & Self.ObjValue.Doubleval'Image);

         --  Obj_Value.Boolval
         Debug.Print
           (Debug.Info,
            Head ("  Boolval:", 16, ' ') & Self.ObjValue.Boolval'Image);
      end if;

      --  Result
      if Self.Set_Fields.Contains (Result) then
         Debug.Print
           (Debug.Info, Head ("Result:", 15, ' ') & Self.Result'Image);
      end if;

      --  Scope
      if Self.Set_Fields.Contains (Scope) then
         Debug.Print (Debug.Info, Head ("Scope:", 15, ' ') & Self.Scope'Image);
      end if;

      --  Filter
      if Self.Set_Fields.Contains (Filter) then
         Debug.Print
           (Debug.Info, Head ("Filter:", 15, ' ') & Self.Filter'Image);
      end if;

      --  Auth_Mech
      if Self.Set_Fields.Contains (Auth_Mech) then
         Debug.Print
           (Debug.Info,
            Head ("Auth_Mech:", 16, ' ') & "'" & To_String (Self.Auth_Mech) &
            "'");
      end if;

      --  Dest_Ae_Inst
      if Self.Set_Fields.Contains (Dest_Ae_Inst) then
         Debug.Print
           (Debug.Info,
            Head ("Dest_Ae_Inst:", 16, ' ') & "'" &
            To_String (Self.Dest_Ae_Inst) & "'");
      end if;

      --  Dest_Ae_Name
      if Self.Set_Fields.Contains (Dest_Ae_Name) then
         Debug.Print
           (Debug.Info,
            Head ("Dest_Ae_Name:", 16, ' ') & "'" &
            To_String (Self.Dest_Ae_Name) & "'");
      end if;

      --  Dest_Ap_Inst
      if Self.Set_Fields.Contains (Dest_Ap_Inst) then
         Debug.Print
           (Debug.Info,
            Head ("Dest_Ap_Inst:", 16, ' ') & "'" &
            To_String (Self.Dest_Ap_Inst) & "'");
      end if;

      --  Dest_Ap_Name
      if Self.Set_Fields.Contains (Dest_Ap_Name) then
         Debug.Print
           (Debug.Info,
            Head ("Dest_Ap_Name:", 16, ' ') & "'" &
            To_String (Self.Dest_Ap_Name) & "'");
      end if;

      --  Src_Ae_Inst
      if Self.Set_Fields.Contains (Src_Ae_Inst) then
         Debug.Print
           (Debug.Info,
            Head ("Src_Ae_Inst:", 16, ' ') & "'" &
            To_String (Self.Src_Ae_Inst) & "'");
      end if;

      --  Src_Ae_Name
      if Self.Set_Fields.Contains (Src_Ae_Name) then
         Debug.Print
           (Debug.Info,
            Head ("Src_Ae_Name:", 16, ' ') & "'" &
            To_String (Self.Src_Ae_Name) & "'");
      end if;

      --  Src_Ap_Inst
      if Self.Set_Fields.Contains (Src_Ap_Inst) then
         Debug.Print
           (Debug.Info,
            Head ("Src_Ap_Inst:", 16, ' ') & "'" &
            To_String (Self.Src_Ap_Inst) & "'");
      end if;

      --  Src_Ap_Name
      if Self.Set_Fields.Contains (Src_Ap_Name) then
         Debug.Print
           (Debug.Info,
            Head ("Src_Ap_Name:", 16, ' ') & "'" &
            To_String (Self.Src_Ap_Name) & "'");
      end if;

      --  Result_Reason
      if Self.Set_Fields.Contains (Result_Reason) then
         Debug.Print
           (Debug.Info,
            Head ("Result_Reason:", 16, ' ') & "'" &
            To_String (Self.Result_Reason) & "'");
      end if;

      --  Version
      if Self.Set_Fields.Contains (Version) then
         Debug.Print
           (Debug.Info, Head ("Version:", 15, ' ') & Self.Version'Image);
      end if;

   end Put;

   procedure To_CDAP (M : in out CDAPMessage; V : in Byte_Vector) is
      Wire_Type    : Wire;
      Field_Id     : CDAP_Field;
      Is_Tag_Field : Boolean := True;

      --  Using a cursor here instead of an iterative loop so we can skip elements
      C : Byte_Vectors.Cursor := V.First;
      use type Byte_Vectors.Cursor;
   begin
      M.Set_Fields.Clear;

      while C /= Byte_Vectors.No_Element loop
         if Is_Tag_Field then
            declare
               Tag_Vector : Byte_Vector;
            begin
               while C /= Byte_Vectors.No_Element loop
                  Tag_Vector.Append (V (C));
                  exit when (not Has_MSB (V (C)));
                  C := Byte_Vectors.Next (C);
               end loop;

               Field_Id  := Tag_To_CDAP_Field (Tag_Vector);
               Wire_Type := Tag_To_Wire_Type (Tag_Vector.First_Element);
            end;

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
                  if Field_Id = OpCode then
                     M.Set_Field
                       (Field_Id,
                        Op_Code'Val (VARINT_To_Uint64 (VARINT_Vector)));
                  else
                     M.Set_Field (Field_Id, VARINT_To_Uint64 (VARINT_Vector));
                  end if;
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
                     --  Debug.Print(Debug.Info, "Appending to LEN_Vec " & Img(V.Last_Element) & " Has_MSB: " & Has_MSB (V.Last_Element)'Image);
                     exit when (not Has_MSB (V (C)));
                     C := Byte_Vectors.Next (C);
                  end loop;

                  C := Byte_Vectors.Next (C);

                  --  The VARINT storing the length is an int32
                  LEN_Length := Natural (VARINT_To_Uint64 (LEN_Vector));

                  if LEN_Length > 0 then
                     while C /= Byte_Vectors.No_Element loop
                        Data_Vector.Append (V (C));
                        LEN_Iterator := LEN_Iterator + 1;
                        exit when (LEN_Iterator = LEN_Length);
                        C := Byte_Vectors.Next (C);
                     end loop;

                     M.Set_Field (Field_Id, Data_Vector);
                  else
                     C := Byte_Vectors.Previous (C);
                  end if;
               end;
            end if;

            --  Next field will be a tag field
            Is_Tag_Field := True;
         end if;

         C := Byte_Vectors.Next (C);
      end loop;
   end To_CDAP;

   function To_Tag (Field : Integer; Wire_Type : Wire) return Byte_Vector is
      Vec       : Byte_Vector;
      Wire_Num  : constant Integer := Wire'Enum_Rep (Wire_Type);
      Key_Value : Natural          := (Field * (2**3)) + Wire_Num;
   begin
      --  First we need to append the record "tag" field
      loop
         declare
            Byte_To_Add : constant Byte := Byte (Key_Value mod (2**7));
         begin
            if Key_Value >= 128 then
               Vec.Append (Byte_To_Add + 128);
            else
               Vec.Append (Byte_To_Add);
               exit;
            end if;
         end;

         Key_Value := Key_Value / (2**7);
      end loop;

      return Vec;
   end To_Tag;

   function To_Tag (Field : CDAP_Field; Wire_Type : Wire) return Byte_Vector is
   begin
      return To_Tag (CDAP_Field'Enum_Rep (Field), Wire_Type);
   end To_Tag;

   function To_Tag (Field : DTC_Field; Wire_Type : Wire) return Byte_Vector is
   begin
      return To_Tag (DTC_Field'Enum_Rep (Field), Wire_Type);
   end To_Tag;

   function Field_To_Wire_Type (Input : CDAP_Field) return Wire is
   begin
      case Input is
         when Abs_Syntax =>
            return VARINT;
         when OpCode =>
            return VARINT;
         when Invoke_Id =>
            return VARINT;
         when Flags =>
            return VARINT;
         when Obj_Class =>
            return LEN;
         when Obj_Name =>
            return LEN;
         when Obj_Inst =>
            return LEN;
         when Result =>
            return VARINT;
         when Scope =>
            return VARINT;
         when Dest_Ae_Inst =>
            return LEN;
         when Dest_Ae_Name =>
            return LEN;
         when Dest_Ap_Inst =>
            return LEN;
         when Dest_Ap_Name =>
            return LEN;
         when Src_Ae_Inst =>
            return LEN;
         when Src_Ae_Name =>
            return LEN;
         when Src_Ap_Inst =>
            return LEN;
         when Src_Ap_Name =>
            return LEN;
         when ObjValue =>
            return LEN;
         when others =>
            return VARINT;
      end case;
   end Field_To_Wire_Type;

   function Field_To_Wire_Type (Input : DTC_Field) return Wire is
   begin
      --  All DTC fields are VARINT encoded
      return VARINT;
   end Field_To_Wire_Type;

   --  The VARINT wire type can encode: int32, int64, uint32, uint64, sint32, sint64, bool, enum
   --  We use Uint64 as the value here since it is large enough to contain all of the possible types
   function To_VARINT (Value : Uint64) return Byte_Vector is
      Vec  : Byte_Vector;
      Temp : Uint64 := Value;
   begin
      --  Append actual field value
      loop
         declare
            -- Take 7 bits of temp
            Byte_To_Add : constant Byte := Byte (Temp mod (2**7));
         begin
            if Temp >= (2**7) then
               --  If this is not the last byte, set the msb
               Vec.Append (Byte_To_Add + (2**7));
            else
               --  If this is the last byte, add it without setting msb
               Vec.Append (Byte_To_Add);
               exit;
            end if;
         end;

         --  Shift right by 7 bits
         Temp := Temp / (2**7);
      end loop;

      return Vec;
   end To_VARINT;

   function To_VARINT (Value : Uint32) return Byte_Vector is
   begin
      return To_VARINT (Uint64 (Value));
   end To_VARINT;

   function To_LEN (Value : Unbounded_String) return Byte_Vector is
      Vec : Byte_Vector;
      Str : String := To_String (Value);
   begin
      --  Append length of string
      Vec.Append (To_VARINT (Uint64 (Str'Length)));

      for I in 1 .. Str'Length loop
         Vec.Append (Character'Pos (Str (I)));
      end loop;

      return Vec;
   end To_LEN;

   function To_LEN (Value : Byte_Vector) return Byte_Vector is
      Vec : Byte_Vector;
   begin
      --  Append length of string
      Vec.Append (To_VARINT (Uint64 (Value.Length)));
      Vec.Append (Value);
      return Vec;
   end To_LEN;

   function To_ObjValue (Value : Obj_Value) return Byte_Vector is
      Vec : Byte_Vector;
   begin
      begin
         if Value.Set_Fields.Contains (Intval) then
            Vec.Append (To_Tag (1, VARINT));
            Vec.Append (To_VARINT (Value.Intval));
         end if;

         if Value.Set_Fields.Contains (Sintval) then
            Vec.Append (To_Tag (2, VARINT));
            Vec.Append (To_VARINT (Uint32 (Value.Sintval)));
         end if;

         if Value.Set_Fields.Contains (Int_64val) then
            Vec.Append (To_Tag (3, VARINT));
            Vec.Append (To_VARINT (Uint64 (Value.Int_64val)));
         end if;

         if Value.Set_Fields.Contains (Sint_64val) then
            Vec.Append (To_Tag (4, VARINT));
            Vec.Append (To_VARINT (Uint64 (Value.Sint_64val)));
         end if;

         if Value.Set_Fields.Contains (Strval) then
            Vec.Append (To_Tag (5, LEN));
            Vec.Append (To_LEN (Value.Strval));
         end if;

         if Value.Set_Fields.Contains (Byteval) then
            Vec.Append (To_Tag (6, LEN));
            Vec.Append (To_LEN (Value.Byteval));
         end if;

         Vec.Prepend (Byte (Vec.Length));
         Vec.Prepend (To_Tag (8, LEN));
      exception
         when E : others =>
            Debug.Print (Debug.Error, "Failed during To_ObjValue!");
      end;

      return Vec;
   end To_ObjValue;

   function Encode
     (Self : CDAPMessage; Fields : Field_Variadic) return Byte_Buffer
   is
      Ret : Byte_Vector;
   begin
      for I in Fields'Range loop

         if Fields (I) /= ObjValue then
            Ret.Append (To_Tag (Fields (I), Field_To_Wire_Type (Fields (I))));
         end if;

         case Fields (I) is
            when Abs_Syntax =>
               Ret.Append (To_VARINT (Self.Abs_Syntax));
            when OpCode =>
               Ret.Append
                 (To_VARINT (Uint32 (Op_Code'Enum_Rep (Self.OpCode))));
            when Invoke_Id =>
               Ret.Append (To_VARINT (Self.Invoke_Id));
            when Flags =>
               Ret.Append
                 (To_VARINT (Uint32 (CDAPFlags'Enum_Rep (Self.Flags))));
            when Obj_Class =>
               Ret.Append (To_LEN (Self.Obj_Class));
            when Obj_Name =>
               Ret.Append (To_LEN (Self.Obj_Name));
            when Obj_Inst =>
               Ret.Append (To_VARINT (Self.Obj_Inst));
            when ObjValue =>
               Ret.Append (To_ObjValue (Self.ObjValue));
            when Result =>
               Ret.Append (To_VARINT (Self.Result));
            when Dest_Ae_Inst =>
               Ret.Append (To_LEN (Self.Dest_Ae_Inst));
            when Dest_Ae_Name =>
               Ret.Append (To_LEN (Self.Dest_Ae_Name));
            when Dest_Ap_Inst =>
               Ret.Append (To_LEN (Self.Dest_Ap_Inst));
            when Dest_Ap_Name =>
               Ret.Append (To_LEN (Self.Dest_Ap_Name));
            when Src_Ae_Inst =>
               Ret.Append (To_LEN (Self.Src_Ae_Inst));
            when Src_Ae_Name =>
               Ret.Append (To_LEN (Self.Src_Ae_Name));
            when Src_Ap_Inst =>
               Ret.Append (To_LEN (Self.Src_Ap_Inst));
            when Src_Ap_Name =>
               Ret.Append (To_LEN (Self.Src_Ap_Name));
            when others =>
               null;
         end case;
      end loop;

      --  Ends with 01
      Ret.Append ((16#01#));

      return Byte_Vector_To_Buffer (Ret);
   end Encode;

   function Encode
     (Self : Data_Transfer_Constants; Fields : DTC_Field_Variadic)
      return Byte_Buffer
   is
      Ret : Byte_Vector;
   begin
      for I in Fields'Range loop
         Ret.Append (To_Tag (Fields (I), Field_To_Wire_Type (Fields (I))));

         case Fields (I) is
            when Max_PDU_Size =>
               Ret.Append (To_VARINT (Self.Max_Pdu_Size));
            when Address_Width =>
               Ret.Append (To_VARINT (Self.Address_Width));
            when Port_Id_Width =>
               Ret.Append (To_VARINT (Self.Port_Id_Width));
            when Cep_Id_Width =>
               Ret.Append (To_VARINT (Self.Cep_Id_Width));
            when Qos_Id_Width =>
               Ret.Append (To_VARINT (Self.Qos_Id_Width));
            when Seq_Num_Width =>
               Ret.Append (To_VARINT (Self.Seq_Num_Width));
            when Length_Width =>
               Ret.Append (To_VARINT (Self.Length_Width));
            when Seq_Rollover_Thresh =>
               Ret.Append (To_VARINT (Self.Seq_Rollover_Thresh));
            when Max_Pdu_Lifetime =>
               Ret.Append (To_VARINT (Self.Max_Pdu_Lifetime));
            when Concatenation_Enabled =>
               Ret.Append
                 (To_VARINT
                    (Uint32 (Boolean'Pos (Self.Concatenation_Enabled))));
            when Fragmentation_Enabled =>
               Ret.Append
                 (To_VARINT
                    (Uint32 (Boolean'Pos (Self.Fragmentation_Enabled))));
            when Integrity_Enabled =>
               Ret.Append
                 (To_VARINT (Uint32 (Boolean'Pos (Self.Integrity_Enabled))));
            when Max_Rtx_Time =>
               Ret.Append (To_VARINT (Self.Max_Rtx_Time));
            when Max_Ack_Delay =>
               Ret.Append (To_VARINT (Self.Max_Ack_Delay));
            when Rate_Width =>
               Ret.Append (To_VARINT (Self.Rate_Width));
            when Frame_Width =>
               Ret.Append (To_VARINT (Self.Frame_Width));
            when Ctrl_Seq_Num_Width =>
               Ret.Append (To_VARINT (Self.Ctrl_Seq_Num_Width));
               --  when others =>
               --     raise Program_Error with "Unknown DTC field encountered during encoding";
         end case;
      end loop;

      Ret.Prepend (To_VARINT (Uint64 (Ret.Length)));
      Ret.Prepend (To_Tag (4, LEN));

      return Byte_Vector_To_Buffer (Ret);
   end Encode;

   function Encode (Self : Data_Transfer_Constants) return Byte_Buffer is
   begin
      return
        Encode
          (Self,
           (Max_PDU_Size, Address_Width, Port_Id_Width, Cep_Id_Width,
            Qos_Id_Width, Seq_Num_Width, Length_Width, Seq_Rollover_Thresh,
            Max_Pdu_Lifetime, Concatenation_Enabled, Fragmentation_Enabled,
            Integrity_Enabled, Max_Rtx_Time, Max_Ack_Delay,
            Ctrl_Seq_Num_Width));
   end Encode;

   function Encode
     (Self : Enrollment_Info; Fields : EInfo_Field_Variadic) return Byte_Buffer
   is
      Ret : Byte_Vector;
   begin
      for I in Fields'Range loop
         case Fields (I) is
            when Address =>
               Ret.Append (To_Tag (1, VARINT));
               Ret.Append (To_VARINT (Self.Address));
            when Lower_Difs =>
               Ret.Append (To_Tag (2, LEN));
               Ret.Append (To_LEN (Self.Lower_Difs));
            when Start_Early =>
               Ret.Append (To_Tag (3, VARINT));
               Ret.Append
                 (To_VARINT (Uint32 (Boolean'Pos (Self.Start_Early))));
            when Dt_Constants =>
               Ret.Append (Byte_Buffer_To_Vector (Self.Dt_Constants.Encode));
         end case;
      end loop;

      return Byte_Vector_To_Buffer (Ret);
   end Encode;

   --  No specified fields argument means we encode all fields
   function Encode (Self : Enrollment_Info) return Byte_Buffer is
   begin
      return Encode (Self, (Address, Lower_Difs, Start_Early, Dt_Constants));
   end Encode;

end CDAP;
