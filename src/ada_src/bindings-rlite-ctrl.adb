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
      Resp : Msg.Register.Response;
      Move : Msg.Register.Move;
   begin
      --  Read 4096 bytes from our file descriptor
      Bytes_Read := OS.Read (fd, Buffer'Address, 4096);
      
      --  Make sure we've actually read something
      if Bytes_Read < 0 then
         Debug.Print ("RINA_Register_Wait", "Error reading from file descriptor", Debug.Error);
         return OS.Invalid_FD;
      end if;

      Resp := Msg.Register.Deserialize (Buffer);
      
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
         Buffer : constant Byte_Buffer := Msg.Register.Serialize (Move);
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
         Buffer : constant Byte_Buffer := Msg.Register.Serialize (req);
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
      fd          : OS.File_Descriptor;
      remote_appl : Bounded_String;
      spec        : Flow.RINA_Flow_Spec;
      flags       : Integer
   ) return Os.File_Descriptor is
      req : Msg.Flow.Request_Arrived;
      spi : Msg.Flow.Sa_Pending_Item;
      resp : Msg.Flow.Response;
      ffd : Integer := -1;
      ret : OS.File_Descriptor;
      Bits_Other_Than_NoResp : constant Unsigned_32 := Unsigned_32 (flags) and not Unsigned_32 (Bindings.Rlite.API.RINA_F_NORESP);
      Bits_Same_As_NoResp : constant Unsigned_32 := Unsigned_32 (flags) and Unsigned_32 (Bindings.Rlite.Api.RINA_F_NORESP);
   begin
      if spec.Version /= Msg.Flow.RINA_FLOW_SPEC_VERSION then
         Debug.Print("RINA_Flow_Accept", "FlowSpec version does not match constant RINA_FLOW_SPEC_VERSION", Debug.Error);
      end if;

      if Bits_Other_Than_NoResp /= 0 then
         Debug.Print("RINA_Flow_Accept", "No response flag was not set", Debug.Error);
      end if;

      --  req := Rl_Read_Msg(fd, 1); -- TODO: See RL_Read_Msg func

      if Bits_Same_As_NoResp /= 0 then
         Debug.Print("RINA_Flow_Accept", "No response flag was set", Debug.Error);
      end if;

      return OS.Invalid_FD;
   end RINA_Flow_Accept;

   function RINA_Flow_Alloc(
      dif_name       : Bounded_String;
      local_appl     : Bounded_String;
      remote_appl    : Bounded_String;
      flowspec       : Flow.RINA_Flow_Spec;
      flags          : Unsigned_32;
      upper_ipcp_id  : Rl_Ipcp_Id_T
   ) return OS.File_Descriptor is
      req      : Msg.Flow.Request;
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

      if flowspec.Version /= Msg.Flow.RINA_FLOW_SPEC_VERSION then
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
         Buffer : Byte_Buffer := Msg.Flow.Serialize (req);
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



end Bindings.Rlite.Ctrl;