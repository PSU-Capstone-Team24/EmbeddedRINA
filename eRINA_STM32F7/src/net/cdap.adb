package body CDAP is

   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : Uint32) is
   begin
      case Field is
         when Abs_Syntax =>
            Self.Abs_Syntax := Val;
         when Invoke_Id => 
            Self.Invoke_Id := Val;
         when Result => 
            Self.Result := Val;
         when Scope =>
            Self.Scope := Val;
         when Filter => 
            Self.Filter := Val;
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

   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : Uint64)
   is
   begin
      null;
   end Set_Field;
end CDAP;
