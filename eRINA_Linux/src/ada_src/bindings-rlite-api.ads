--  Temp disabling
pragma Style_Checks (Off);

--  GNAT
with GNAT.OS_Lib;

--  Interfaces
with Interfaces;
   use Interfaces;

with Names;
  use Names.Name_String;

with Bindings.Rlite.Msg.IPCP;
   use Bindings.Rlite.Msg.IPCP;

with Bindings.Rlite.Msg.Flow;
   use Bindings.Rlite.Msg;

package Bindings.Rlite.API is
   package OS renames GNAT.OS_Lib;
   
   --  Constants
   RINA_F_NOWAIT : constant Integer := 1;
   RINA_F_NORESP : constant Integer := 2;

   --  Open a file descriptor that can be used to register/unregister names,
   --  and to manage incoming flow allocation requests. On success, it
   --  returns a file descriptor that can be later provided to rina_register(),
   --  rina_unregister(), rina_flow_accept(), and rina_flow_respond().
   --  On error -1 is returned.
   --  int rina_open (void)
   function RINA_Open return OS.File_Descriptor;
   
   --  Closes a RINA file descriptor
   --  This exists just for sematic purposes, is redundant with OS.Close
   --  int rina_close (void)
   procedure RINA_Close (fd : OS.File_Descriptor);

   --  Creates a new IPCP and creates a DIF of type DIF_Type with name DIF_Name
   function RINA_Create_IPCP (
      Fd : OS.File_Descriptor;
      Name : String;
      DIF_Type : DIF_Types;
      DIF_Name : String
   ) return Rl_IPCP_Id_T;

   -- Removes/destroys an existing IPCP by name
   procedure RINA_Destroy_IPCP (
      Fd : OS.File_Descriptor;
      Name : String
   );

   -- Removes/destroys an existing IPCP by its IPCP_Id
   -- (This can be looked up via the IPCP hashmap in Ctrl logic)
   procedure RINA_Destroy_IPCP (
      Fd : OS.File_Descriptor;
      Id : Rl_Ipcp_Id_T
   );

   --  Register the application name local_appl to a DIF in the system.
   --  After a successful registration, flow allocation requests can be received
   --  on fd.
   --  int rina_register(int fd,
   --                    const char *dif_name,
   --                    const char *local_appl
   --                    int flags)
   function RINA_Register (fd : OS.File_Descriptor;
      dif_name : String;
      local_appl : String;
      flags : Integer
   ) return OS.File_Descriptor;

   --  Unregister the application name @local_appl from the DIF where it was registered
   --  The @dif_name argument must match the one passed to rina_register().
   --  After a successful unregistration, flow allocation requests can no longer be received on @fd
   --  int rina_unregister(int fd, const char *dif_name, const char *local_appl,
   --                      int flags);
   function RINA_Unregister (
      Fd : OS.File_Descriptor;
      Dif_Name : String;
      Local_Appl : String;
      Flags : Integer
   ) return OS.File_Descriptor;

   --  Accept an incoming flow request arrived on @fd. If @flags does not contain
   --  RINA_F_NORESP, it also sends a positive response to the requesting application;
   --  otherwise, the response (positive or negative) can be sent by a subsequent call
   --  to the rina_flow_respond().
   --  int rina_flow_accept(int fd, char **remote_appl, struct rina_flow_spec *spec,
   --                       unsigned int flags);
   function RINA_Flow_Accept (
      Fd           : OS.File_Descriptor;
      Remote_Appl  : in out Bounded_String;
      Spec         : Msg.Flow.RINA_Flow_Spec;
      Flags        : Integer
      ) return OS.File_Descriptor;


   --  Issue a flow allocation request towards the destination application called
   --  @remote_appl, using @local_appl as a source application name. If @flowspec
   --  is not NULL, it specifies the QoS parameters to be used for the flow, if the
   --  flow allocation request is successful.
   --  int rina_flow_alloc(const char *dif_name, const char *local_appl,
   --                      const char *remote_appl,
   --                      const struct rina_flow_spec *flowspec, unsigned int flags);
   function RINA_Flow_Alloc(
      Dif_Name       : String;
      Local_Appl     : String;
      Remote_Appl    : String;
      Flowspec       : Flow.RINA_Flow_Spec;
      Flags          : Unsigned_32
   )  return OS.File_Descriptor;

   --  Wait for the completion of a flow allocation procedure previously initiated
   --  with a call to rina_flow_alloc() with the RINA_F_NOWAIT flag set. The @wfd
   --  file descriptor must match the one returned by rina_flow_alloc().
   --  int rina_flow_alloc_wait(int wfd);
   function RINA_Flow_Alloc_Wait(
      Wfd            : OS.File_Descriptor 
   )  return OS.File_Descriptor;

   function RINA_Flow_Respond(
      Fd       : OS.File_Descriptor;
      Handle   : OS.File_Descriptor;
      Response : Integer
   ) return OS.File_Descriptor;


end Bindings.Rlite.API;