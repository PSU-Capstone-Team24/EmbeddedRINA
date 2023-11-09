--  Temp disabling
pragma Style_Checks (Off);

with Interfaces; use Interfaces;

with Bindings.Rlite.Msg;
  use Bindings.Rlite.Msg;

with Bindings.Rlite.Msg.Flow;

with Bindings.Rlite.Kernel_Msg;
with Bindings.Rlite.API;
with Bindings.Rlite.Common;
with Bindings.Rlite.Utils;
with Bindings.Rlite.List;
with System;
with GNAT.OS_Lib;

with Buffers;
  use Buffers;
  
with Names; use Names.Name_String;

package Bindings.Rlite.Ctrl is
   --  OS Library Package
   package OS renames GNAT.OS_Lib;

   --  RLite Binding Packages
   package Kernel_Msg renames Bindings.Rlite.Kernel_Msg;
   package API renames Bindings.Rlite.API;
   package Common renames Bindings.Rlite.Common;
   package Utils renames Bindings.Rlite.Utils;
   package List renames Bindings.Rlite.List;

   type Sa_Pending_Item(Local_Appl_Size : Natural; Remote_Appl_Size : Natural; Dif_Name_Size : Natural) is record
      handle : Integer;
      req : Flow.Request_Arrived(Local_Appl_Size, Remote_Appl_Size, Dif_Name_Size);
      node : List.List_Head;
   end record;

   function RINA_Register_Common (fd : OS.File_Descriptor;
   dif_name : Bounded_String;
   local_appl : Bounded_String;
   flags : Integer;
   reg : Unsigned_8) return OS.File_Descriptor;


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
      remote_appl : Bounded_String;
      spec        : Flow.RINA_Flow_Spec;
      flags       : Integer
   ) return Os.File_Descriptor;

   function RINA_Flow_Alloc(
      dif_name       : Bounded_String;
      local_appl     : Bounded_String;
      remote_appl    : Bounded_String;
      flowspec       : Flow.RINA_Flow_Spec;
      flags          : Unsigned_32;
      upper_ipcp_id  : Unsigned_16
   ) return OS.File_Descriptor;

   function RINA_Flow_Alloc_Wait(
      wfd            : OS.File_Descriptor;
      port_id        : Unsigned_16
   )return OS.File_Descriptor;


end Bindings.Rlite.Ctrl;