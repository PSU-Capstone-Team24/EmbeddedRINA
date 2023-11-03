--  Temp disabling
pragma Style_Checks (Off);

--  Debug
with Debug;

--  Ada
with Ada.Strings;
   use Ada.Strings;

with Ada.Strings.Unbounded;
   use Ada.Strings.Unbounded;

with Ada.Text_IO;
   use Ada.Text_IO;

with Ada.Strings.Bounded;


package body Bindings.Rlite.Ctrl is
   
   function Rl_Write_Msg
     (Rfd : OS.File_Descriptor;
      Msg : Kernel_Msg.Rl_Msg_Base;
      Quiet : Integer) return OS.File_Descriptor is

      Ser_Len : Natural;
      Ret : Integer;
   begin

      --  Get serialized message length for what we are trying to send
      Debug.Print ("Rl_Write_Msg", "Msg_Type " & Kernel_Msg.Rl_Msg_T'Image (Msg.Hdr.Msg_Type), Debug.Warning);

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
      dif_name : Unbounded_String;
      local_appl : Unbounded_String;
      flags : Integer;
      reg : Unsigned_8) return OS.File_Descriptor is

      ret : OS.File_Descriptor := OS.Invalid_FD;
      wfd : OS.File_Descriptor := OS.Invalid_FD;
      res : constant OS.File_Descriptor := OS.Invalid_FD;

      req : Kernel_Msg.Rl_Kmsg_Appl_Register;

      Bits_Other_Than_NoWait : constant Unsigned_32 := Unsigned_32 (flags) and not Unsigned_32 (Bindings.Rlite.API.RINA_F_NOWAIT);
      
      function Serialize is new Kernel_Msg.Serialize (T => Kernel_Msg.Rl_Kmsg_Appl_Register);
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
      req.Reg           := reg;
      req.Pad1          := (others => 0);
      req.Appl_Name     := local_appl;
      req.Dif_Name      := dif_name;

      declare
         buf : Kernel_Msg.Byte_Buffer(1 .. req'Size / 8 - 7) := (others => 0);
      begin
         buf := Serialize (req, true);
      end;
      
      Debug.Print ("RINA_Register_Common", "Message Type: " & Kernel_Msg.Rl_Msg_T'Image (req.Hdr.Msg_Type), Debug.Info);

      --  MT: TODO: Rl_Msg_Free implementation, check flags again and return rina_register_wait
      --  instead of a file descriptor to the result
      return ret;
   end RINA_Register_Common;

   function RINA_Flow_Accept(
      fd          : OS.File_Descriptor;
      remote_appl : Unbounded_String;
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