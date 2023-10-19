--  Temp disabling
pragma Style_Checks (Off);

with Interfaces; use Interfaces;
with Interfaces.C.Strings;
with Bindings.Rlite.Kernel_Msg;

with System;
with GNAT.OS_Lib;

package Bindings.Rlite.Ctrl is
   package OS renames GNAT.OS_Lib;
   
   function RINA_Register_Common (fd : OS.File_Descriptor;
   dif_name : String;
   local_appl : String;
   flags : Integer;
   reg : Unsigned_8) return OS.File_Descriptor;

   --  unsigned int rl_msg_serlen(struct rl_msg_layout *numtables, size_t num_entries,
   --                          const struct rl_msg_base *msg);
   function Rl_Msg_Serlen
     (numtables : System.Address;
      num_entries : Natural;
      msg : System.Address) return Natural;
   pragma Import (C, Rl_Msg_Serlen, "rl_msg_serlen");
   
   --  int rl_write_msg(int rfd, const struct rl_msg_base *msg, int quiet);
   function Rl_Write_Msg
     (rfd : OS.File_Descriptor;
      msg : System.Address;
      quiet : Integer) return OS.File_Descriptor;

   --  void rl_msg_free(struct rl_msg_layout *numtables, size_t num_entries,
   --                   struct rl_msg_base *msg);
   procedure Rl_Msg_Free
      (numtables : System.Address;
       num_entries : Unsigned_32;
       msg : System.Address);
   pragma Import (C, Rl_Msg_Free, "rl_msg_free");

   function Rl_Register_Req_Fill
     (req : Bindings.Rlite.Kernel_Msg.Rl_Kmsg_Appl_Register;
      event_id : Unsigned_32;
      dif_name : Interfaces.C.Strings.chars_ptr;
      reg : Unsigned_8;
      appl_name : Interfaces.C.Strings.chars_ptr) return OS.File_Descriptor;
   pragma Import (C, Rl_Register_Req_Fill, "rl_register_req_fill");

end Bindings.Rlite.Ctrl;