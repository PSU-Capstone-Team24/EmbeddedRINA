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
