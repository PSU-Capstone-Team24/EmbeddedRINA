with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Buffers;               use Buffers;
with Protobuf;              use Protobuf;
with Ada.Containers.Vectors;
with Ada.Real_Time;         use Ada.Real_Time;

package CDAP is

   Startup_Time : Time := Clock;

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

   type Obj_Value_Field is
     (Intval, Sintval, Int_64val, Sint_64val, Strval, Byteval, Floatval,
      Doubleval, Boolval);

   for Obj_Value_Field use
     (Intval  => 1, Sintval => 2, Int_64val => 3, Sint_64val => 4, Strval => 5,
      Byteval => 6, Floatval => 7, Doubleval => 8, Boolval => 9);

   package OBJ_Value_Fields is new Ada.Containers.Vectors
     (Natural, Obj_Value_Field);
   subtype OBJ_Value_Field_Vector is OBJ_Value_Fields.Vector;

   type Obj_Value is tagged record
      Set_Fields : OBJ_Value_Field_Vector;
      Intval     : Uint32           := 0;
      Sintval    : Int32            := 0;
      Int_64val  : Uint64           := 0;
      Sint_64val : Int64            := 0;
      Strval     : Unbounded_String := To_Unbounded_String ("");
      Byteval    : Byte_Vector;
      Floatval   : Short_Float      := 0.0;
      Doubleval  : Long_Long_Float  := 0.0;
      Boolval    : Boolean          := False;
   end record;

   procedure Clear_Fields (Self : in out Obj_Value);
   procedure Set_Field
     (Self : in out Obj_Value; Field : Obj_Value_Field; Val : Uint64);
   procedure Set_Field
     (Self : in out Obj_Value; Field : Obj_Value_Field; Val : String);
   procedure Set_Field
     (Self : in out Obj_Value; Field : Obj_Value_Field; Val : Boolean);
   procedure Set_Field
     (Self : in out Obj_Value; Field : Obj_Value_Field; Val : Byte_Vector);

   type CDAP_Field is
     (Abs_Syntax, OpCode, Invoke_Id, Flags, Obj_Class, Obj_Name, Obj_Inst,
      ObjValue, Result, Scope, Filter, Auth_Mech, AuthValue, Dest_Ae_Inst,
      Dest_Ae_Name, Dest_Ap_Inst, Dest_Ap_Name, Src_Ae_Inst, Src_Ae_Name,
      Src_Ap_Inst, Src_Ap_Name, Result_Reason, Version);

   for CDAP_Field use
     (Abs_Syntax => 1, OpCode => 2, Invoke_Id => 3, Flags => 4, Obj_Class => 5,
      Obj_Name => 6, Obj_Inst => 7, ObjValue => 8, Result => 9, Scope => 10,
      Filter       => 11, Auth_Mech => 17, AuthValue => 18, Dest_AE_Inst => 19,
      Dest_AE_Name => 20, Dest_AP_Inst => 21, Dest_AP_Name => 22,
      Src_AE_Inst  => 23, Src_AE_Name => 24, Src_AP_Inst => 25,
      Src_AP_Name  => 26, Result_Reason => 27, Version => 28);

   package Field_Vectors is new Ada.Containers.Vectors (Natural, CDAP_Field);
   subtype Field_Vector is Field_Vectors.Vector;

   type DTC_Field is
     (Max_PDU_Size, Address_Width, Port_Id_Width, Cep_Id_Width, Qos_Id_Width,
      Seq_Num_Width, Length_Width, Seq_Rollover_Thresh, Max_Pdu_Lifetime,
      Concatenation_Enabled, Fragmentation_Enabled, Integrity_Enabled,
      Max_Rtx_Time, Max_Ack_Delay, Rate_Width, Frame_Width,
      Ctrl_Seq_Num_Width);

   for DTC_Field use
     (Max_PDU_Size          => 1, Address_Width => 2, Port_Id_Width => 3,
      Cep_Id_Width          => 4, Qos_Id_Width => 5, Seq_Num_Width => 6,
      Length_Width => 7, Seq_Rollover_Thresh => 8, Max_Pdu_Lifetime => 9,
      Concatenation_Enabled => 10, Fragmentation_Enabled => 11,
      Integrity_Enabled     => 12, Max_Rtx_Time => 13, Max_Ack_Delay => 14,
      Rate_Width => 15, Frame_Width => 16, Ctrl_Seq_Num_Width => 17);

   type Data_Transfer_Constants is tagged record
      Max_Pdu_Size          : Uint32;
      Address_Width         : Uint32;
      Port_Id_Width         : Uint32;
      Cep_Id_Width          : Uint32;
      Qos_Id_Width          : Uint32;
      Seq_Num_Width         : Uint32;
      Length_Width          : Uint32;
      Seq_Rollover_Thresh   : Uint64;
      Max_Pdu_Lifetime      : Uint32;
      Concatenation_Enabled : Boolean;
      Fragmentation_Enabled : Boolean;
      Integrity_Enabled     : Boolean;
      Max_Rtx_Time          : Uint32;
      Max_Ack_Delay         : Uint32;
      Rate_Width            : Uint32;
      Frame_Width           : Uint32;
      Ctrl_Seq_Num_Width    : Uint32;
   end record;

   type DTC_Field_Variadic is array (Positive range <>) of DTC_Field;
   function Encode
     (Self : Data_Transfer_Constants; Fields : DTC_Field_Variadic)
      return Byte_Buffer;

   --  No specified fields argument means we encode all fields
   function Encode (Self : Data_Transfer_Constants) return Byte_Buffer;

   type Enrollment_Info is tagged record
      Address      : Uint64;
      Lower_Difs   : Unbounded_String;
      Start_Early  : Boolean;
      Dt_Constants : Data_Transfer_Constants;
   end record;

   type EInfo_Field is (Address, Lower_Difs, Start_Early, Dt_Constants);
   for EInfo_Field use
     (Address => 1, Lower_Difs => 2, Start_Early => 3, Dt_Constants => 4);
   type EInfo_Field_Variadic is array (Positive range <>) of EInfo_Field;

   function Encode
     (Self : Enrollment_Info; Fields : EInfo_Field_Variadic)
      return Byte_Buffer;

   --  No specified fields argument means we encode all fields
   function Encode (Self : Enrollment_Info) return Byte_Buffer;

   type CDAPMessage is tagged record
      Set_Fields    : Field_Vector;
      Abs_Syntax    : Uint32           := 0;
      OpCode        : Op_Code          := M_CONNECT;
      Invoke_Id     : Uint32           := 0;
      Flags         : CDAPFlags        := F_NO_FLAGS;
      Obj_Class     : Unbounded_String := To_Unbounded_String ("");
      Obj_Name      : Unbounded_String := To_Unbounded_String ("");
      Obj_Inst      : Uint64           := 0;
      ObjValue      : Obj_Value;
      Result        : Uint32           := 0;
      Scope         : Uint32           := 0;
      Filter        : Uint32           := 0;
      Auth_Mech     : Unbounded_String := To_Unbounded_String ("");
      AuthValue     : Auth_Value;
      Dest_Ae_Inst  : Unbounded_String := To_Unbounded_String ("");
      Dest_Ae_Name  : Unbounded_String := To_Unbounded_String ("");
      Dest_Ap_Inst  : Unbounded_String := To_Unbounded_String ("");
      Dest_Ap_Name  : Unbounded_String := To_Unbounded_String ("");
      Src_Ae_Inst   : Unbounded_String := To_Unbounded_String ("");
      Src_Ae_Name   : Unbounded_String := To_Unbounded_String ("");
      Src_Ap_Inst   : Unbounded_String := To_Unbounded_String ("");
      Src_Ap_Name   : Unbounded_String := To_Unbounded_String ("");
      Result_Reason : Unbounded_String := To_Unbounded_String ("");
      Version       : Uint64           := 0;
   end record;

   --  Print message contents for debugging purposes
   procedure Put (Self : CDAPMessage);

   type Field_Variadic is array (Positive range <>) of CDAP_Field;
   function Encode
     (Self : CDAPMessage; Fields : Field_Variadic) return Byte_Buffer;

   procedure Clear_Fields (Self : in out CDAPMessage);

   --  Takes in a vector of bytes and returns a CDAP message record
   procedure To_CDAP (M : in out CDAPMessage; V : in Byte_Vector);

   function To_Tag (Field : Integer; Wire_Type : Wire) return Byte_Vector;
   function To_Tag (Field : CDAP_Field; Wire_Type : Wire) return Byte_Vector;
   function To_Tag (Field : DTC_Field; Wire_Type : Wire) return Byte_Vector;

   --  Takes a CDAP message field and returns a VARINT encoded byte buffer
   function To_VARINT (Value : Uint32) return Byte_Vector;

   function Field_To_Wire_Type (Input : CDAP_Field) return Wire;
end CDAP;
