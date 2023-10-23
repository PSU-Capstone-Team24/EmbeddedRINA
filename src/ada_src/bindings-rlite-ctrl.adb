--  Temp disabling
pragma Style_Checks (Off);

with Debug;
with Interfaces.C;
with Ada.Text_IO;

package body Bindings.Rlite.Ctrl is

   function Rl_Msg_Serlen
     (Numtables : Utils.Rl_Ker_Numtables_Array;
      Num_Entries : Natural;
      Msg : Common.Rl_Msg_Base) return Natural is

      Ret   : Natural;
      Name  : Common.Rina_Name;
      I     : Integer;
   begin

      if Natural(Msg.Hdr.Msg_Type) >= Num_Entries then
         Debug.Print ("Invalid numtables access [msg_type=" & Common.Rl_Msg_T'Image(Msg.Hdr.Msg_Type) & "]");
         return 0;
      end if;

      return 0;
   end Rl_Msg_Serlen;

   function Rl_Write_Msg
     (Rfd : OS.File_Descriptor;
      Msg : Common.Rl_Msg_Base;
      Quiet : Integer) return OS.File_Descriptor is

      -- Serbuf contains 4096 chars
      type Ser_Buffer is array (0 .. 4095) of Character;
      Ser_Len : Natural;
      Ret : Integer;
   begin

      --  Get serialized message length for what we are trying to send
      Ser_Len := Rl_Msg_Serlen (Utils.Rl_Ker_Numtables, Integer (Kernel_Msg.RLITE_KER_MSG_MAX), Msg);

      --  Ada 'Size is in bits, account for this!
      if Ser_Len * 8 > Ser_Buffer'Size then
         Debug.Print ("Serialized message would be too long " & Integer'Image (Ser_Len * 8) & " > " & Integer'Image (Ser_Buffer'Size));
         return OS.Invalid_FD;
      end if;

      --  Serialize the message
      --  Ser_Len := Serialize_Rlite_Msg (Utils.Rl_Ker_Numtables'Address, Integer (Kernel_Msg.RLITE_KER_MSG_MAX), Ser_Buffer'Address, Msg);
      ret := 0;

      return OS.File_Descriptor (ret);
   end Rl_Write_Msg;

   function RINA_Register_Common (fd : OS.File_Descriptor;
      dif_name : String;
      local_appl : String;
      flags : Integer;
      reg : Unsigned_8) return OS.File_Descriptor is

      req : Kernel_Msg.Rl_Kmsg_Appl_Register;
      ret : OS.File_Descriptor;
      wfd : OS.File_Descriptor;
      res : constant OS.File_Descriptor := OS.Invalid_FD;
      Bits_Other_Than_NoWait : constant Unsigned_32 := Unsigned_32 (flags) and not Unsigned_32 (Bindings.Rlite.API.RINA_F_NOWAIT);
   begin
      Ada.Text_IO.Put_Line("RINA_Register_Common Called");

      if Bits_Other_Than_NoWait /= 0 then
         --  Flag has bits other than RINA_F_NOWAIT set, return invalid file descriptor
         Ada.Text_IO.Put_Line ("Flag has bits other than RINA_F_NOWAIT");
         return res;
      end if;

      --  Open dedicated file descriptor to perform the operation and wait for response
      wfd := Bindings.Rlite.API.RINA_Open;

      if Integer (wfd) < 0 then
         Ada.Text_IO.Put_Line("RINA_Register_Common WFD < 0");

         --  Invalid control device file descriptor
         return wfd;
      end if;

      Ada.Text_IO.Put_Line("RINA_Register_Common Filling Request");

      ret := Rl_Register_Req_Fill (req, Bindings.Rlite.API.RINA_REG_EVENT_ID, Interfaces.C.Strings.New_String (dif_name), reg, Interfaces.C.Strings.New_String (local_appl));

      if Integer (ret) > 0 then
         --  Error here, close out FD
         Bindings.Rlite.API.RINA_Close (wfd);
         Ada.Text_IO.Put_Line ("Error filling registration request struct");
      end if;

      ret := Rl_Write_Msg (wfd, req, 1);

      --  MT: TODO: Rl_Msg_Free implementation, check flags again and return rina_register_wait
      --  instead of a file descriptor to the result
      return ret;
   end RINA_Register_Common;

end Bindings.Rlite.Ctrl;