--  Temp disabling
pragma Style_Checks (Off);

with Bindings.Rlite.API;
with Bindings.Rlite.Utils;

with Ada.Text_IO;

package body Bindings.Rlite.Ctrl is

   package Kernel_Msg renames Bindings.Rlite.Kernel_Msg;
   package Utils renames Bindings.Rlite.Utils;

   function Rl_Write_Msg
     (rfd : OS.File_Descriptor;
      msg : System.Address;
      quiet : Integer) return OS.File_Descriptor is

      type Ser_Buffer is array (0 .. 4096) of Character;
      Ser_Len : Natural;
      Ret : Integer;
   begin

      --  Serialize the message
      Ser_Len := Rl_Msg_Serlen (Utils.Rl_Ker_Numtables'Address, Integer (Kernel_Msg.RLITE_KER_MSG_MAX), msg);

      --  MT: TODO: Finish this implementation, use Rl_Write_Msg_Binded for now
      Ada.Text_IO.Put_Line ("Serlen: " & Natural'Image(Ser_Len));
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

      --  MT: TODO: Fix this, these should be printing the same output
      ret := Rl_Write_Msg (wfd, req'Address, 1);

      --  MT: TODO: Rl_Msg_Free implementation, check flags again and return rina_register_wait
      --  instead of a file descriptor to the result
      return ret;
   end RINA_Register_Common;

end Bindings.Rlite.Ctrl;