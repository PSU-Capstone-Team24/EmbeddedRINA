--  Temp disabling
pragma Style_Checks (Off);

--  GNAT
with GNAT.OS_Lib;

--  Interfaces
with Interfaces;
   use Interfaces;

with Names;
  use Names.Name_String;

with Bindings.Rlite.Msg.Flow;
   use Bindings.Rlite.Msg;

package Bindings.Rlite.API is
   package OS renames GNAT.OS_Lib;
   
   RINA_F_NOWAIT : constant Integer := 1;
   RINA_F_NORESP : constant Integer := 2;
      
   function RINA_Open return OS.File_Descriptor;
   
   procedure RINA_Close(fd : OS.File_Descriptor);

   function RINA_Register (fd : OS.File_Descriptor;
      dif_name : Bounded_String;
      local_appl : Bounded_String;
      flags : Integer) return OS.File_Descriptor;

   function RINA_Unregister (
      fd : OS.File_Descriptor;
      dif_name : Bounded_String;
      local_appl : Bounded_String;
      flags : Integer
   ) return OS.File_Descriptor;

   -- int rina_flow_accept(int fd, char **remote_appl, struct rina_flow_spec *spec, unsigned int flags);
   function RINA_Flow_Accept (
      fd           : OS.File_Descriptor;
      remote_appl  : Bounded_String;
      spec         : Msg.Flow.RINA_Flow_Spec;
      flags        : Integer
      ) return Os.File_Descriptor;

   function RINA_Flow_Alloc(
      dif_name       : Bounded_String;
      local_appl     : Bounded_String;
      remote_appl    : Bounded_String;
      flowspec       : Flow.RINA_Flow_Spec;
      flags          : Unsigned_32
   )  return OS.File_Descriptor;

   function RINA_Flow_Alloc_Wait(
      wfd            : OS.File_Descriptor 
   )  return OS.File_Descriptor;


end Bindings.Rlite.API;