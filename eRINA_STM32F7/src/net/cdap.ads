with System;
with Interfaces;

with Buffers;
  use Buffers;

with Ada.Strings.Unbounded;
  use Ada.Strings.Unbounded;

with Protobuf;

package CDAP is

   type Op_Code is
     (M_CONNECT, M_CONNECT_R, M_RELEASE, M_RELEASE_R, M_CREATE, M_CREATE_R,
      M_DELETE, M_DELETE_R, M_READ, M_READ_R, M_CANCELREAD, M_CANCELREAD_R,
      M_WRITE, M_WRITE_R, M_START, M_START_R, M_STOP, M_STOP_R);

   type CDAPFlags is
     (F_NO_FLAGS, F_SYNC, F_RD_INCOMPLETE);
   
   type Auth_Type is
     (AUTH_NONE, AUTH_PASSWD, AUTH_SSHRSA, AUTH_SSHDSA);

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

   type CDAPMessage is record
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

end CDAP;