--  Temp disabling
pragma Style_Checks (Off);

--  Debug
with Debug;

with Names;
  use Names;

with Exceptions;

with Bindings.Rlite.Msg.Register;

package body Bindings.Rlite.Ctrl is
   procedure Rl_Write_Msg
     (Rfd : OS.File_Descriptor;
      Msg : Byte_Buffer;
      Quiet : Integer) is

      Ser_Len : constant Natural := Msg'Size / 8;

      --  Result can be less than N if a disk full condition was detected
      --  https://sites.radford.edu/~nokie/classes/320/std_lib_html/gnat-os_lib.html#204
      Ret : Integer := 0;
   begin
      Ret := OS.Write (Rfd, Msg'Address, Ser_Len);

      --  Verbose
      Put_Bytes (Msg);
      Debug.Print ("Rl_Write_Msg", "Bytes Written " & Integer'Image(Ret), Debug.Info);
      Debug.Print ("Rl_Write_Msg", "Message Type: " & Rl_Msg_T'Enum_Val (Msg (3))'Image, Debug.Info);

      --  Check for failure conditions
      if Ret < 0 then
         Debug.Print ("Rl_Write_Msg", "Disk full condition? Ret < 0", Debug.Error);
      end if;

      --  Check that we were actually able to write something
      if Ret = 0 then
         Debug.Print ("Rl_Write_Msg", "0 bytes written during OS.Write", Debug.Warning);
      end if;

   end Rl_Write_Msg;

      -- TODO: Needs to be implemented. Maybe first 16 bits are header where rest is body.
   function Rl_Read_Msg (
      Rfd : OS.File_Descriptor;
      Quiet : Integer
   ) return Byte_Buffer is
      Bytes_Read : Integer;
      Buffer : Byte_Buffer(1 .. 4096);
   begin
      --  Arbitrary 4096 bytes to follow along with rlite serbuf[4096]
      Bytes_Read := OS.Read (rfd, Buffer'Address, 4096);
      return Buffer;
   end Rl_Read_Msg;

   function RINA_Register_Wait (Fd : OS.File_Descriptor;
      Wfd : OS.File_Descriptor) return OS.File_Descriptor is
      Buffer : Byte_Buffer(1 .. 4096) := (others => 0);
      Bytes_Read : Integer := 0;
      Resp : Register.Response;
      Move : Register.Move;
   begin
      Register.Deserialize (Resp, Fd);
      
      --  Malformed or received wrong msg_type/event_id, throw exception? idk maybe
      --  Assert msg_type = RLITE_KER_APPL_REGISTER_RESP = 0x0005
      if Resp.Hdr.Msg_Type /= RLITE_KER_APPL_REGISTER_RESP then
         Debug.Print("RINA_Register_Wait", "Deserialized wrong message type?", Debug.Error);
         return OS.Invalid_FD;
      end if;

      --  Assert event_id = RINA_REG_EVENT_ID = 0x7A6B
      if Resp.Hdr.Event_Id /= RINA_REG_EVENT_ID then
         Debug.Print("RINA_Register_Wait", "Deserialized wrong event_id?", Debug.Error);
         return OS.Invalid_FD;
      end if;

      --  Response != 0, close and throw
      if Resp.Response > 0 then
         API.RINA_Close (wfd);
         raise Exceptions.DIF_Registration_Failure;
      end if;

      --  Registration was successful: associate the registered application
      --  with the file descriptor specified by the caller
      Move.Hdr.Msg_Type := RLITE_KER_APPL_MOVE;
      Move.Hdr.Event_Id := 1;
      Move.Ipcp_Id      := Resp.Ipcp_Id;
      Move.Fd           := Integer_32 (fd);

      declare
         Buffer : constant Byte_Buffer := Register.Serialize (Move);
      begin
         Rl_Write_Msg (wfd, Buffer, 1);
      end;

      --  Close our temp writing fd
      API.RINA_Close (wfd);

      return fd;
   end RINA_Register_Wait;

   function RINA_Register_Common (fd : OS.File_Descriptor;
      dif_name : Bounded_String;
      local_appl : Bounded_String;
      flags : Integer;
      reg : Unsigned_8) return OS.File_Descriptor is

      wfd : OS.File_Descriptor := OS.Invalid_FD;

      Bits_Other_Than_NoWait : constant Unsigned_32 := Unsigned_32 (flags) and not Unsigned_32 (API.RINA_F_NOWAIT);
      No_Wait : constant Unsigned_32 := Unsigned_32 (flags) and Unsigned_32 (API.RINA_F_NOWAIT);
      req : Register.Request;
   begin

      if Bits_Other_Than_NoWait /= 0 then
         --  Flag has bits other than RINA_F_NOWAIT set, return invalid file descriptor
         Debug.Print ("RINA_Register_Common", "Flag has bits other than RINA_F_NOWAIT", Debug.Error);
         return OS.Invalid_FD;
      end if;

      --  Open dedicated file descriptor to perform the operation and wait for response
      wfd := API.RINA_Open;

      if wfd = Invalid_FD then
         --  Invalid control device file descriptor
         Debug.Print ("RINA_Register_Common", "File descriptor to RINA control device < 0", Debug.Error);
         return OS.Invalid_FD;
      end if;

      --  Setup request message
      req.Hdr.Msg_Type  := RLITE_KER_APPL_REGISTER;
      req.Hdr.Event_Id  := RINA_REG_EVENT_ID;
      req.Reg           := reg;
      req.Appl_Name     := local_appl;
      req.Dif_Name      := dif_name;

      declare
         Buffer : constant Byte_Buffer := Register.Serialize (req);
      begin
         Rl_Write_Msg (fd, Buffer, 0);
      end;

      if No_Wait > 0 then
         --  Return the file descriptor to wait on
         return wfd;
      end if;

      --  Wait for the operation to complete right now
      return RINA_Register_Wait (fd, wfd);
   end RINA_Register_Common;

   function RINA_Flow_Accept(
      Fd          : OS.File_Descriptor;
      Remote_Appl : in out Bounded_String;
      Spec        : Flow.RINA_Flow_Spec;
      Flags       : Integer
   ) return Boolean is
      Resp : Flow.Response;
      Req  : Flow.Request_Arrived;
      Buffer : Byte_Buffer(1 .. 4096) := (others => 0);
      Bits_Other_Than_NoResp : constant Unsigned_32 := Unsigned_32 (flags) and not Unsigned_32 (Bindings.Rlite.API.RINA_F_NORESP);
      Bits_Same_As_NoResp : constant Unsigned_32 := Unsigned_32 (flags) and Unsigned_32 (Bindings.Rlite.Api.RINA_F_NORESP);
   begin
      if Spec.Version /= Flow.RINA_FLOW_SPEC_VERSION then
         Debug.Print("RINA_Flow_Accept", "FlowSpec version does not match constant RINA_FLOW_SPEC_VERSION", Debug.Error);
      end if;

      if Bits_Other_Than_NoResp /= 0 then
         Debug.Print("RINA_Flow_Accept", "Wrong flags", Debug.Error);
      end if;

      --  Wait for response message from the kernel
      Flow.Deserialize (Req, Fd);

      --  Message read in from FD was not a flow request, we can
      --  stop our processing logic here and throw this message out
      if Req.Hdr.Msg_Type /= RLITE_KER_FA_REQ_ARRIVED then
         return False;
      end if;

      --  Update Remote_Appl to what was received from flow allocation request msg
      Remote_Appl := Req.Remote_Appl;

      --  Build flow allocation request response message
      Resp.Hdr.Msg_Type := RLITE_KER_FA_RESP;
      Resp.Hdr.Event_Id := 1;
      Resp.Kevent_Id := Req.Kevent_Id;
      Resp.Ipcp_Id := Req.Ipcp_Id;
      Resp.Upper_Ipcp_Id := 16#FFFF#;
      Resp.Port_Id := Req.Port_Id;
      Resp.Response := 1;

      declare
         Buffer : constant Byte_Buffer := Flow.Serialize (Resp);
      begin
         Rl_Write_Msg (Fd, Buffer, 0);
      end;

      return True;
   end RINA_Flow_Accept;

   function RINA_Flow_Alloc(
      dif_name       : Bounded_String;
      local_appl     : Bounded_String;
      remote_appl    : Bounded_String;
      flowspec       : Flow.RINA_Flow_Spec;
      flags          : Unsigned_32;
      upper_ipcp_id  : Rl_Ipcp_Id_T
   ) return OS.File_Descriptor is
      req      : Flow.Request;
      wfd, ret : OS.File_Descriptor;
      function Get_Pid return Unsigned_32
         with Import, Convention => C, External_Name => "getpid";
      Bits_Other_Than_NoWait : constant Unsigned_32 := flags and not Unsigned_32 (Bindings.Rlite.API.RINA_F_NOWAIT);
   begin
      if Bits_Other_Than_NoWait /= 0 then
         --  Flag has bits other than RINA_Flow_Alloc set, return invalid file descriptor
         Debug.Print ("RINA_Flow_Alloc", "Flag has bits other than RINA_Flow_Alloc", Debug.Error);
         return OS.Invalid_FD;
      end if;

      if flowspec.Version /= Flow.RINA_FLOW_SPEC_VERSION then
      --  Flowspec version malformed or otherwise incorrect, return invalid file descriptor
         Debug.Print ("RINA_Flow_Alloc", "FlowSpec version does not match constant RINA_FLOW_SPEC_VERSION", Debug.Error);
         return OS.Invalid_FD;
      end if;

      --  Setup rl_fa_req_fill - flow allocation
      req.Hdr.Msg_Type  := RLITE_KER_FA_REQ;
      req.Hdr.Event_Id  := RINA_FA_EVENT_ID;
      req.Dif_Name      := dif_name;
      req.Upper_Ipcp_Id := upper_ipcp_id;
      req.Local_Appl    := local_appl;
      req.Remote_Appl   := remote_appl;

      --  Was originally bitshifted right 1x
      req.Cookie        := Get_Pid / 2;

      wfd := API.RINA_Open;
      
      if wfd = Invalid_FD then
         Debug.Print ("RINA_Flow_Alloc", "Error creating file descriptor to RINA control device", Debug.Error);
         return OS.Invalid_FD;
      end if;

      declare
         Buffer : Byte_Buffer := Flow.Serialize (req);
      begin
         Rl_Write_Msg (wfd, Buffer, 0);
      end;
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

function RINA_Flow_Respond(
   fd       : OS.File_Descriptor;
   handle   : Integer;
   response : Integer
) return OS.File_Descriptor is
   spi    : Sa_Pending_Item;
   cur    : Sa_Pending_Item;
   req    : Flow.Request;
   resp   : Flow.Response;
   ffd    : OS.File_Descriptor := -1;
   ret    : Integer;
   cursor : Sig_Action_List.Cursor := Sa_Pending.First;
begin
   while Sig_Action_List.Has_Element(cursor) loop
      if(Sig_Action_List.Element(cursor).Handle = handle) then
         spi := Sig_Action_List.Element(cursor);
         Sig_Action_List.Delete(Sa_Pending, cursor);
         cursor := Sig_Action_List.Next(cursor);
      end if;
   end loop;
   if spi.Handle /= -1 then --This logic should hold provided handle is not -1, unsure of potential values
      Debug.Print ("RINA_Flow_Respond", "Could not find sa_pending_item that matches handle value " & Integer'Image(handle), Debug.Error);
      return OS.Invalid_FD;
   end if;

   req := spi.Req;

   -- equivalent of rl_fa_resp_fill
   resp.Hdr.msg_type := RLITE_KER_FA_RESP;
   resp.Hdr.event_id := 1;
   resp.Kevent_Id := req.Kevent_Id;
   resp.Ipcp_Id := req.Ipcp_Id;
   resp.upper_ipcp_id := 16#FFFF#;

   return ffd;
end RINA_Flow_Respond;


end Bindings.Rlite.Ctrl;