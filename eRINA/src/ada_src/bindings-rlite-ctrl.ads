--  Temp disabling
pragma Style_Checks (Off);

with Interfaces; use Interfaces;
with Ada.Containers.Doubly_Linked_Lists;

with Bindings.Rlite.API;
with Bindings.Rlite.Common;
with Bindings.Rlite.Utils;
with Bindings.Rlite.List;
   use Bindings.Rlite.List;
with System;
with GNAT.OS_Lib;
   use GNAT.OS_Lib;

with Buffers;
  use Buffers;
  
with Names; use Names.Name_String;

with Bindings.Rlite.Msg.Flow;
  use Bindings.Rlite.Msg;

package Bindings.Rlite.Ctrl is
   --  OS Library Package
   package OS renames GNAT.OS_Lib;

   --  RLite Binding Packages
   package API renames Bindings.Rlite.API;
   package Common renames Bindings.Rlite.Common;
   package Utils renames Bindings.Rlite.Utils;
   package List renames Bindings.Rlite.List;

   type Sa_Pending_Item is record
      Handle : Integer;
      Req    : Rl_Msg_T := RLITE_KER_FA_REQ_ARRIVED;
   end record;

   package Sig_Action_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Sa_Pending_Item);

   function RINA_Register_Common (Fd : OS.File_Descriptor;
   Dif_Name : Bounded_String;
   Local_Appl : Bounded_String;
   Flags : Integer;
   Reg : Unsigned_8) return OS.File_Descriptor;

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
   ) return Boolean;

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
      handle : Integer;
      response : Integer
   ) return OS.File_Descriptor;

   private
      Sa_Pending : Sig_Action_List.List;
      
end Bindings.Rlite.Ctrl;