--  Temp disabling
pragma Style_Checks (Off);

--  Bindings
with Bindings.Rlite.Ctrl;

package body Bindings.Rlite.API is

   --  Renames
   package Ctrl renames Bindings.Rlite.Ctrl;

   --  int rina_open (void)
   --  Open a file descriptor that can be used to register/unregister names,
   --  and to manage incoming flow allocation requests. On success, it
   --  returns a file descriptor that can be later provided to rina_register(),
   --  rina_unregister(), rina_flow_accept(), and rina_flow_respond().
   --  On error -1 is returned.
   function RINA_Open return OS.File_Descriptor is
   begin
      return OS.Open_Read_Write ("/dev/rlite", OS.Binary);
   end RINA_Open;

   --  int rina_close (void)
   --  Closes a RINA file descriptor
   -- This exists just for sematic purposes, is redundant with OS.Close
   procedure RINA_Close(fd : OS.File_Descriptor) is
   begin
      OS.Close (fd);
   end RINA_Close;
   
   --  int rina_register(int fd,
   --                    const char *dif_name,
   --                    const char *local_appl
   --                    int flags)
  --  Register the application name local_appl to a DIF in the system.
  --  After a successful registration, flow allocation requests can be received
  --  on fd.
   function RINA_Register (fd : OS.File_Descriptor;
      dif_name : Bounded_String;
      local_appl : Bounded_String;
      flags : Integer) return OS.File_Descriptor is
   begin
      return Ctrl.RINA_Register_Common (fd, dif_name, local_appl, flags, 1);
   end RINA_Register;

   function RINA_Unregister (fd : OS.File_Descriptor;
      dif_name : Bounded_String;
      local_appl : Bounded_String;
      flags : Integer
   ) return OS.File_Descriptor is
   begin
      return Ctrl.RINA_Register_Common (fd, dif_name, local_appl, flags, 0);
   end RINA_Unregister;

   function RINA_Flow_Accept (
      fd           : OS.File_Descriptor;
      remote_appl : Bounded_String;
      spec         : Flow.RINA_Flow_Spec;
      flags        : Integer
   ) return Os.File_Descriptor is
   begin
      return Ctrl.RINA_Flow_Accept(fd, remote_appl, spec, flags);
   end RINA_Flow_Accept;

   function RINA_Flow_Alloc(
      dif_name       : Bounded_String;
      local_appl     : Bounded_String;
      remote_appl    : Bounded_String;
      flowspec       : Flow.RINA_Flow_Spec;
      flags          : Unsigned_32
   )  return OS.File_Descriptor is
   begin
      return Ctrl.RINA_Flow_Alloc(dif_name, local_appl, remote_appl,
      flowspec, flags, 16#FFFF#);
   end RINA_Flow_Alloc;

   function RINA_Flow_Alloc_Wait(
      wfd            : OS.File_Descriptor 
   )  return OS.File_Descriptor is
   begin
      return Ctrl.RINA_Flow_Alloc_Wait(wfd, 0);
   end RINA_Flow_Alloc_Wait;




end Bindings.Rlite.API;