--  Temp disabling
pragma Style_Checks (Off);

--  Debug
with Debug;

--  Ada
with Ada.Text_IO;
  use Ada.Text_IO;

with Names;
  use Names;

with Buffers;
  use Buffers;

with Bindings.Rlite.Msg.Flow;

with Bindings.Rlite.Msg.Register;
  use Bindings.Rlite.Msg;

with Bindings.Rlite.API;

package body Bindings.Rlite.Ctrl is
   
   procedure Rl_Write_Msg
     (Rfd : OS.File_Descriptor;
      Msg : Byte_Buffer;
      Quiet : Integer) is

      Ser_Len : Natural := Msg'Size / 8;

      --  Result can be less than N if a disk full condition was detected
      --  https://sites.radford.edu/~nokie/classes/320/std_lib_html/gnat-os_lib.html#204
      Ret : Integer := 0;
   begin
      Ret := OS.Write (Rfd, Msg'Address, Ser_Len);
      Debug.Print("RINA_Register_Common", "Bytes Written " & Integer'Image(Ret), Debug.Info);

      --  Check for failure conditions
      if Ret < 0 then
         Debug.Print ("Rl_Write_Msg", "Disk full condition? Ret < 0", Debug.Error);
      end if;
   end Rl_Write_Msg;

   function RINA_Register_Common (fd : OS.File_Descriptor;
      dif_name : Bounded_String;
      local_appl : Bounded_String;
      flags : Integer;
      reg : Unsigned_8) return OS.File_Descriptor is

      ret : OS.File_Descriptor := OS.Invalid_FD;
      wfd : OS.File_Descriptor := OS.Invalid_FD;
      res : constant OS.File_Descriptor := OS.Invalid_FD;

      Bits_Other_Than_NoWait : constant Unsigned_32 := Unsigned_32 (flags) and not Unsigned_32 (Bindings.Rlite.API.RINA_F_NOWAIT);
      
      req : Register.Request;
   begin

      if Bits_Other_Than_NoWait /= 0 then
         --  Flag has bits other than RINA_F_NOWAIT set, return invalid file descriptor
         Debug.Print ("RINA_Register_Common", "Flag has bits other than RINA_F_NOWAIT", Debug.Error);
         return res;
      end if;

      --  Open dedicated file descriptor to perform the operation and wait for response
      wfd := Bindings.Rlite.API.RINA_Open;

      if Integer (wfd) < 0 then
         Debug.Print ("RINA_Register_Common", "RINA_Register_Common WFD < 0", Debug.Error);

         --  Invalid control device file descriptor
         return wfd;
      end if;

      --  Setup request message
      req.Hdr.Msg_Type  := RLITE_KER_APPL_REGISTER;
      req.Reg           := reg;
      req.Appl_Name     := local_appl;
      req.Dif_Name      := dif_name;

      declare
         Buffer : Byte_Buffer := Bindings.Rlite.Msg.Register.Serialize (req);
      begin
         Rl_Write_Msg (fd, Buffer, 0);
      end; 
      
      Debug.Print ("RINA_Register_Common", "Message Type: " & Rl_Msg_T'Image (req.Hdr.Msg_Type), Debug.Info);

      --  MT: TODO: Rl_Msg_Free implementation, check flags again and return rina_register_wait
      --  instead of a file descriptor to the result
      return ret;
   end RINA_Register_Common;

   function RINA_Flow_Accept(
      fd          : OS.File_Descriptor;
      remote_appl : Bounded_String;
      spec        : Flow.RINA_Flow_Spec;
      flags       : Integer
   ) return Os.File_Descriptor is
      req : Flow.Request_Arrived(0, Used_Size (remote_appl), 0);
      spi : Sa_Pending_Item(0, Used_Size (remote_appl), 0);
      resp : Flow.Response;
   begin
      return 1;
   end RINA_Flow_Accept;

   function RINA_Flow_Alloc(
      dif_name       : Bounded_String;
      local_appl     : Bounded_String;
      remote_appl    : Bounded_String;
      flowspec       : Flow.RINA_Flow_Spec;
      flags          : Unsigned_32;
      upper_ipcp_id  : Unsigned_16
   ) return OS.File_Descriptor is
      req      : Msg.Flow.Request;
      wfd, ret : OS.File_Descriptor;
      function Get_Pid return Unsigned_32
         with Import, Convention => C, External_Name => "getpid";
      Bits_Other_Than_NoWait : constant Unsigned_32 := 
         Unsigned_32 (flags) and not Unsigned_32 
         (Bindings.Rlite.API.RINA_F_NOWAIT);
   begin
      if Bits_Other_Than_NoWait /= 0 then
         --  Flag has bits other than RINA_Flow_Alloc set, return invalid file descriptor
         Debug.Print ("RINA_Flow_Alloc", "Flag has bits other than RINA_Flow_Alloc", Debug.Error);
         return OS.Invalid_FD;
      end if;

      if flowspec.Version /= Msg.Flow.RINA_FLOW_SPEC_VERSION then
      --  Flag has bits other than RINA_Flow_Spec_Version set, return invalid file descriptor
         Debug.Print ("RINA_Flow_Alloc", "flowspec version doesn't match", Debug.Error);
         return OS.Invalid_FD;
      end if;

      --  Setup rl_fa_req_fill - flow allocation
      req.Hdr.Msg_Type  := RLITE_KER_FA_REQ;
      req.Hdr.Event_ID  := RINA_REG_EVENT_ID;
      req.Dif_Name      := dif_name;
      req.Upper_Ipcp_Id := upper_ipcp_id;
      req.Local_Appl    := local_appl;
      req.Remote_Appl   := remote_appl;
      req.Cookie        := Get_Pid / 2;

      wfd := Bindings.Rlite.API.RINA_Open;
      
      --need to fix
      return wfd;

   end RINA_Flow_Alloc;



   function RINA_Flow_Alloc_Wait(
      wfd            : OS.File_Descriptor;
      port_id        : Unsigned_16
   )return OS.File_Descriptor is
   begin
      return wfd;
   end RINA_Flow_Alloc_Wait;



end Bindings.Rlite.Ctrl;