--  Temp disabling
pragma Style_Checks (Off);

--  GNAT
with GNAT.OS_Lib;

--  Interfaces
with Interfaces; use Interfaces;

package Bindings.Rlite.API is
   package OS renames GNAT.OS_Lib;
   
   RINA_F_NOWAIT : constant Integer := 1;
   RINA_F_NORESP : constant Integer := 2;

   --  Casual value used for assert (0x7a6b)
   RINA_REG_EVENT_ID : constant Unsigned_32 := 16#7a6b#; 

   function RINA_Open return OS.File_Descriptor;

   function RINA_Register (fd : OS.File_Descriptor;
      dif_name : String;
      local_appl : String;
      flags : Integer) return OS.File_Descriptor;

end Bindings.Rlite.API;