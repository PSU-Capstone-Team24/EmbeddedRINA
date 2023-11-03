--  Temp disabling
pragma Style_Checks (Off);

--  GNAT
with GNAT.OS_Lib;

-- Ada
with Ada.Strings.Unbounded;
   use Ada.Strings.Unbounded;

--  Interfaces
with Interfaces;
   use Interfaces;

package Bindings.Rlite.API is
   package OS renames GNAT.OS_Lib;
   
   RINA_F_NOWAIT : constant Integer := 1;
   RINA_F_NORESP : constant Integer := 2;

   --  Casual value used for assert (0x7a6b)
   RINA_REG_EVENT_ID : constant Unsigned_32 := 16#7a6b#;

   type RINA_FLOW_SPEC is record
      RINA_FLOW_SPEC_VERSION :  Interfaces.Unsigned_32 := 2;
      RINA_FLOW_SPEC_LOSS_MAX : Interfaces.Unsigned_16 := 10000;
      Version           : Interfaces.Unsigned_32; -- in microseconds
      Max_Delay         : Interfaces.Unsigned_32; -- in SDUs
      Max_Sdu_Gap       : Interfaces.Unsigned_64; -- in bits per second
      Avg_Bandwidth     : Interfaces.Unsigned_64; -- from 0 (0%) to 10000 (100%)
      Max_Loss          : Interfaces.Unsigned_16;
      In_Order_Delivery : Boolean;
      Msg_Boundaries    : Boolean;
      Max_Jitter        : Interfaces.Unsigned_32; -- in microseconds
   end record;

   function RINA_Open return OS.File_Descriptor;
   
   procedure RINA_Close(fd : OS.File_Descriptor);

   function RINA_Register (fd : OS.File_Descriptor;
      dif_name : Unbounded_String;
      local_appl : Unbounded_String;
      flags : Integer) return OS.File_Descriptor;

   function RINA_Unregister (
      fd : OS.File_Descriptor;
      dif_name : Unbounded_String;
      local_appl : Unbounded_String;
      flags : Integer
   ) return OS.File_Descriptor;

   -- int rina_flow_accept(int fd, char **remote_appl, struct rina_flow_spec *spec, unsigned int flags);
   function RINA_Flow_Accept (
      fd           : OS.File_Descriptor;
      remote_appl  : Unbounded_String;
      spec         : RINA_FLOW_SPEC;
      flags        : Integer
      ) return Os.File_Descriptor;
end Bindings.Rlite.API;