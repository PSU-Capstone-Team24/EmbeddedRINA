--  Temp disabling
pragma Style_Checks (Off);

with Interfaces; use Interfaces;

with GNAT.OS_Lib;

package Bindings.Rlite.Ctrl is
   package OS renames GNAT.OS_Lib;

   function RINA_Register_Common (fd : OS.File_Descriptor;
   dif_name : String;
   local_appl : String;
   flags : Integer;
   reg : Unsigned_8) return OS.File_Descriptor;

   --  function Rl_Register_Req_Fill (req : in out Bindings.Rlite.Kernel_Msg.Rl_Kmsg_Appl_Register;
   --  event_id : Unsigned_32;
   --  dif_name : String;
   --  reg : Unsigned_8;
   --  appl_name: String) return Integer;

end Bindings.Rlite.Ctrl;