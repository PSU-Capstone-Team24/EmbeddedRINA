--  Temp disabling
pragma Style_Checks (Off);

with Interfaces;
  use Interfaces;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
  use Ada.Containers;

with Bindings.Rlite.API;
with Bindings.Rlite.Common;
with Bindings.Rlite.List;
   use Bindings.Rlite.List;

with System;
with GNAT.OS_Lib;
   use GNAT.OS_Lib;

with Buffers;
  use Buffers;

with Names;
  use Names.Name_String;

with Bindings.Rlite.Msg.IPCP;
with Bindings.Rlite.Msg.Flow;
  use Bindings.Rlite.Msg;

package Bindings.Rlite.Ctrl is
   --  OS Library Package
   package OS renames GNAT.OS_Lib;

   --  RLite Binding Packages
   package API renames Bindings.Rlite.API;
   package Common renames Bindings.Rlite.Common;
   package List renames Bindings.Rlite.List;

   type Sa_Pending_Item_Base is record
      Handle : OS.File_Descriptor;
      Req    : Flow.Request_Arrived;
   end record;

   package Sig_Action_List_Base is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Sa_Pending_Item_Base);

   type Sa_Pending_Item is record
      Sa_Pending : Sa_Pending_Item_Base;
      Node : Sig_Action_List_Base.List;
   end record;

   package Sig_Action_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Sa_Pending_Item);

   package IPCP_Id_Map is new Ada.Containers.Hashed_Maps (
      Key_Type => Bounded_String,
      Element_Type => Rl_Ipcp_Id_T,
      Hash => Names.Hash,
      Equivalent_Keys => "="
   );

   use IPCP_Id_Map;

   IPCP_Map : Map;
   function Search_Map_By_Value (Map_Var : in out Map; Value : Rl_Ipcp_Id_T) return Cursor;

   function RINA_Register_Common (Fd : OS.File_Descriptor;
   Dif_Name : Bounded_String;
   Local_Appl : Bounded_String;
   Flags : Integer;
   Reg : Unsigned_8
   ) return OS.File_Descriptor;

   function RINA_Create_IPCP (
      Fd : OS.File_Descriptor;
      Name : Bounded_String;
      DIF_Type : Bindings.Rlite.Msg.IPCP.DIF_Types;
      DIF_Name : Bounded_String
   ) return Rl_IPCP_Id_T;

   procedure RINA_Destroy_IPCP (
      Fd : OS.File_Descriptor;
      Id : Rl_Ipcp_Id_T);

   -- struct rl_msg_base *rl_read_next_msg(int rfd, int quiet)
   function Rl_Read_Msg (
      Rfd : OS.File_Descriptor;
      Quiet : Integer
   ) return Byte_Buffer;

   --  int rl_write_msg(int rfd, const struct rl_msg_base *msg, int quiet);
   procedure Rl_Write_Msg
     (Rfd : OS.File_Descriptor;
      Msg : Byte_Buffer;
      Quiet : Integer);
     
   --  void rl_msg_free(struct rl_msg_layout *numtables, size_t num_entries,
   --                   struct rl_msg_base *msg);
   procedure Rl_Msg_Free
      (numtables : System.Address;
       num_entries : Unsigned_32;
       msg : System.Address);
   pragma Import (C, Rl_Msg_Free, "rl_msg_free");

   function RINA_Flow_Accept(
      fd          : OS.File_Descriptor;
      remote_appl : in out Bounded_String;
      spec        : Flow.RINA_Flow_Spec;
      flags       : Integer
   ) return OS.File_Descriptor;

   function RINA_Flow_Alloc(
      dif_name       : Bounded_String;
      local_appl     : Bounded_String;
      remote_appl    : Bounded_String;
      flowspec       : Flow.RINA_Flow_Spec;
      flags          : Unsigned_32;
      upper_ipcp_id  : Rl_Ipcp_Id_T
   ) return OS.File_Descriptor;

   function RINA_Flow_Alloc_Wait(
      wfd            : OS.File_Descriptor;
      port_id        : Unsigned_16
   )return OS.File_Descriptor;

   function RINA_Flow_Respond(
      fd : OS.File_Descriptor;
      handle : OS.File_Descriptor;
      response : Integer
   ) return OS.File_Descriptor;

   function Open_Port_Common(
      port_id : Rl_Port_T;
      mode    : Unsigned_8;
      ipcp_id : Rl_Ipcp_Id_T
   ) return OS.File_Descriptor;

   function Rl_Open_Appl_Port(
      port_id : Rl_Port_T
   ) return OS.File_Descriptor;

   --  #define RLITE_IOCTL_FLOW_BIND _IOW(0xAF, 0x00, struct rl_ioctl_info)
   RLITE_IOCTL_FLOW_BIND : constant Unsigned_32 := 1074310912;
   
   --  Bind the flow identified by port_id to this rl_io device.
   --  #define RLITE_IO_MODE_APPL_BIND 86
   RLITE_IO_MODE_APPL_BIND : constant Unsigned_8 := 86;

   private
      Sa_Pending : Sig_Action_List.List;
      Sa_Handle : Integer := 0;
      Sa_Pending_Len : Natural := 0;
end Bindings.Rlite.Ctrl;