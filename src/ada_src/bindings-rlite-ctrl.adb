--  Temp disabling
pragma Style_Checks (Off);

with Debug;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Bindings.Rlite.Ctrl is
   
   function Rl_Write_Msg
     (Rfd : OS.File_Descriptor;
      Msg : Kernel_Msg.Rl_Msg_Base;
      Quiet : Integer) return OS.File_Descriptor is

      -- Serbuf contains 4096 chars
      type Ser_Buffer is array (0 .. 4095) of Character;
      Ser_Len : Natural;
      Ret : Integer;
   begin

      --  Get serialized message length for what we are trying to send
      Debug.Print ("Rl_Write_Msg", "Msg_Type " & Kernel_Msg.Rl_Msg_T'Image(Msg.Hdr.Msg_Type), Debug.Warning);

      --  MT: WIP
      --  Ser_Len := Kernel_Msg.Rl_Msg_Serlen (Msg);

      --  Ada 'Size is in bits, account for this!
      --  if Ser_Len * 8 > Ser_Buffer'Size then
      --     Debug.Print ("Rl_Write_Msg", "Serialized message would be too long " & Integer'Image (Ser_Len * 8) & " > " & Integer'Image (Ser_Buffer'Size), Debug.Error);
      --     return OS.Invalid_FD;
      --  end if;

      --  Serialize the message
      --  Ser_Len := Serialize_Rlite_Msg (Utils.Rl_Ker_Numtables'Address, Integer (Kernel_Msg.RLITE_KER_MSG_MAX), Ser_Buffer'Address, Msg);

      return OS.Invalid_FD;
   end Rl_Write_Msg;

   function RINA_Register_Common (fd : OS.File_Descriptor;
      dif_name : String;
      local_appl : String;
      flags : Integer;
      reg : Unsigned_8) return OS.File_Descriptor is

      off : Natural := 1;
      req : Kernel_Msg.Rl_Kmsg_Appl_Register;
      ret : OS.File_Descriptor;
      wfd : OS.File_Descriptor;
      res : constant OS.File_Descriptor := OS.Invalid_FD;
      Bits_Other_Than_NoWait : constant Unsigned_32 := Unsigned_32 (flags) and not Unsigned_32 (Bindings.Rlite.API.RINA_F_NOWAIT);
      procedure Serialize is new Kernel_Msg.Display_Bytes (T => Kernel_Msg.Rl_Kmsg_Appl_Register);
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

      --  Setup request message, originally done in rl_register_req_fill
      req.Hdr.Version   := Kernel_Msg.RLITE_API_VERSION;
      req.Hdr.Msg_Type  := Kernel_Msg.RLITE_KER_APPL_REGISTER;
      req.Hdr.Event_Id  := Bindings.Rlite.API.RINA_REG_EVENT_ID;
      req.Dif_Name      := To_Unbounded_String (dif_name);
      req.Reg           := reg;
      req.Appl_Name     := To_Unbounded_String (local_appl);

      Serialize (req);
      Debug.Print ("RINA_Register_Common", "Message Type: " & Kernel_Msg.Rl_Msg_T'Image(req.Hdr.Msg_Type), Debug.Info);

      --  MT: TODO: Rl_Msg_Free implementation, check flags again and return rina_register_wait
      --  instead of a file descriptor to the result
      return ret;
   end RINA_Register_Common;

   function RINA_Flow_Accept(
      fd          : OS.File_Descriptor;
      remote_appl : String;
      spec        : Bindings.Rlite.API.RINA_FLOW_SPEC;
      flags       : Integer
   ) return Os.File_Descriptor is
      req : Kernel_Msg.Rl_Kmsg_Fa_Req_Arrived;
      spi : Sa_Pending_Item;
      resp : Kernel_Msg.rl_kmsg_fa_resp;
   begin
      return 1;
   end RINA_Flow_Accept;
end Bindings.Rlite.Ctrl;