with System;
with Interfaces;

with Buffers; use Buffers;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package CDAP is

   type Op_Code is
     (M_CONNECT, M_CONNECT_R, M_RELEASE, M_RELEASE_R, M_CREATE, M_CREATE_R,
      M_DELETE, M_DELETE_R, M_READ, M_READ_R, M_CANCELREAD, M_CANCELREAD_R,
      M_WRITE, M_WRITE_R, M_START, M_START_R, M_STOP, M_STOP_R);

   type CDAPFlags is (F_NO_FLAGS, F_SYNC, F_RD_INCOMPLETE);

   type Auth_Type is (AUTH_NONE, AUTH_PASSWD, AUTH_SSHRSA, AUTH_SSHDSA);

   type Auth_Value is record
      Auth_Name     : Unbounded_String;
      Auth_Password : Unbounded_String;
      Auth_Other    : Unbounded_String;
   end record;

   type Obj_Value is record
      Intval     : Uint32;
      Sintval    : Uint32;
      Int_64val  : Uint64;
      Sint_64val : Uint64;
      Strval     : Unbounded_String;
      Byteval    : Uint8;
      Floatval   : Uint32;
      Doubleval  : Uint64;
      Boolval    : Boolean;
   end record;

   type CDAP_Field is
     (Abs_Syntax, OpCode, Invoke_Id, Flags, Obj_Class, Obj_Name, Obj_Inst,
     ObjValue, Result, Scope, Filter, Auth_Mech, AuthValue, Dest_Ae_Inst,
     Dest_Ae_Name, Dest_Ap_Inst, Dest_Ap_Name, Src_Ae_Inst, Src_Ae_Name,
     Src_Ap_Inst, Src_Ap_Name, Result_Reason, Version);

   for CDAP_Field use
     (Abs_Syntax   => 1,
      OpCode       => 2,
      Invoke_Id    => 3,
      Flags        => 4,
      Obj_Class    => 5,
      Obj_Name     => 6,
      Obj_Inst     => 7,
      ObjValue     => 8,
      Result       => 9,
      Scope        => 10,
      Filter       => 11,
      Auth_Mech    => 17,
      AuthValue    => 18,
      Dest_AE_Inst => 19,
      Dest_AE_Name => 20,
      Dest_AP_Inst => 21,
      Dest_AP_Name => 22,
      Src_AE_Inst  => 23,
      Src_AE_Name  => 24,
      Src_AP_Inst  => 25,
      Src_AP_Name  => 26,
      Result_Reason => 27,
      Version      => 28);

   type CDAPMessage is tagged record
      Abs_Syntax    : Uint32;
      OpCode        : Op_Code := M_CONNECT;
      Invoke_Id     : Uint32;
      Flags         : CDAPFlags;
      Obj_Class     : Unbounded_String;
      Obj_Name      : Unbounded_String;
      Obj_Inst      : Uint64;
      ObjValue      : Obj_Value;
      Result        : Uint32;
      Scope         : Uint32;
      Filter        : Uint32;
      Auth_Mech     : Unbounded_String;
      AuthValue     : Auth_Value;
      Dest_Ae_Inst  : Unbounded_String;
      Dest_Ae_Name  : Unbounded_String;
      Dest_Ap_Inst  : Unbounded_String;
      Dest_Ap_Name  : Unbounded_String;
      Src_Ae_Inst   : Unbounded_String;
      Src_Ae_Name   : Unbounded_String;
      Src_Ap_Inst   : Unbounded_String;
      Src_Ap_Name   : Unbounded_String;
      Result_Reason : Unbounded_String;
      Version       : Uint64;
   end record;

   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : Op_Code);
   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : CDAPFlags);
   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : Obj_Value);
   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : Auth_Value);
   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : Unbounded_String);
   procedure Set_Field (Self : in out CDAPMessage; Field : CDAP_Field; Val : Uint64);
end CDAP;
