--  Temp disabling
pragma Style_Checks (Off);

with Bindings.Rlite.Kernel_Msg;
with Bindings.Rlite.Common;

with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;

with GNAT.OS_Lib;

package Bindings.Rlite.Ctrl is
   package OS renames GNAT.OS_Lib;

   function RINA_Register_Common (fd : OS.File_Descriptor;
   dif_name : String;
   local_appl : String;
   flags : Integer;
   reg : Integer) return OS.File_Descriptor;

   function Rl_Register_Req_Fill (req : Bindings.Rlite.Kernel_Msg.Rl_Kmsg_Appl_Register;
   event_id : Unsigned_32;
   dif_name : String;
   reg : Integer;
   appl_name: String) return Integer;

   function Rl_Write_Msg (rfd : Integer;
   msg : Bindings.Rlite.Common.Rl_Msg_Base;
   quiet : Integer) return Integer;

end Bindings.Rlite.Ctrl;