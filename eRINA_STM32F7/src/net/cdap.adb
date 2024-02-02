package body CDAP is

   --  For setting VARINT fields
   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : Uint64) is
      Val_As_Uint32 : Uint32 := Uint32 (Val);
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

   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : Byte_Buffer) is
      Final_String : String := Buffer_To_String (Val);
      Final_String_Unbounded : Unbounded_String := To_Unbounded_String (Final_String);
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
            null;
      end case;
   end Set_Field;

   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : Op_Code)
   is
   begin
      null;
   end Set_Field;

   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : CDAPFlags)
   is
   begin
      null;
   end Set_Field;

   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : Obj_Value)
   is
   begin
      null;
   end Set_Field;

   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : Auth_Value)
   is
   begin
      null;
   end Set_Field;

   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : Unbounded_String)
   is
   begin
      null;
   end Set_Field;

end CDAP;
