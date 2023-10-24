--  Temp disabling
pragma Style_Checks (Off);

with Interfaces; use Interfaces;

with Bindings.Rlite.Kernel_Msg;
with Bindings.Rlite.API;
with Bindings.Rlite.Common;
with Bindings.Rlite.Utils;
with Bindings.Rlite.List;
with System;
with GNAT.OS_Lib;

package Bindings.Rlite.Ctrl is
   package List renames Bindings.Rlite.List;


   type Sa_Pending_Item is record
      handle : Integer;
      req : Rlite.Kernel_Msg.Rl_Kmsg_Fa_Req_Arrived;
      node : List.List_Head;
   end record;

   --  OS Library Package
   package OS renames GNAT.OS_Lib;

   --  RLite Binding Packages
   package Kernel_Msg renames Bindings.Rlite.Kernel_Msg;
   package API renames Bindings.Rlite.API;
   package Common renames Bindings.Rlite.Common;
   package Utils renames Bindings.Rlite.Utils;
   
   function RINA_Register_Common (fd : OS.File_Descriptor;
   dif_name : String;
   local_appl : String;
   flags : Integer;
   reg : Unsigned_8) return OS.File_Descriptor;

   --  unsigned int rl_msg_serlen(size_t num_entries,
   --                          const struct rl_msg_base *msg);
   function Rl_Msg_Serlen (Msg : Kernel_Msg.Rl_Msg_Base) return Natural;

   --  int rl_write_msg(int rfd, const struct rl_msg_base *msg, int quiet);
   function Rl_Write_Msg
     (Rfd : OS.File_Descriptor;
      Msg : Kernel_Msg.Rl_Msg_Base;
      Quiet : Integer) return OS.File_Descriptor;

   --  void rl_msg_free(struct rl_msg_layout *numtables, size_t num_entries,
   --                   struct rl_msg_base *msg);
   procedure Rl_Msg_Free
      (numtables : System.Address;
       num_entries : Unsigned_32;
       msg : System.Address);
   pragma Import (C, Rl_Msg_Free, "rl_msg_free");

   function RINA_Flow_Accept(
      fd          : OS.File_Descriptor;
      remote_appl : String;
      spec        : Bindings.Rlite.API.RINA_FLOW_SPEC;
      flags       : Integer
   ) return Os.File_Descriptor;
end Bindings.Rlite.Ctrl;