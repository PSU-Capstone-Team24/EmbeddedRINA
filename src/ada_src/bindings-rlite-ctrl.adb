--  Temp disabling
pragma Style_Checks (Off);

with Bindings.Rlite.Kernel_Msg;
with Interfaces; use Interfaces;

package body Bindings.Rlite.Ctrl is

   --  For writing messages to the RINA control device
   function Rl_Write_Msg (rfd : Integer;
      msg : Bindings.Rlite.Common.Rl_Msg_Base;
      quiet : Integer) return Integer is
   begin
      -- MT: TODO: Finish this
   end Rl_Write_Msg;

   function Rl_Register_Req_Fill (req : Bindings.Rlite.Kernel_Msg.Rl_Kmsg_Appl_Register;
   event_id : Unsigned_32;
   dif_name : String;
   reg : Integer;
   appl_name: String) return Integer is
   begin
      --  DIF is blank, set it to NULL
      if dif_name = "" then
         dif_name := NULL;
      end if;

      req.Hdr.Msg_Type  := RLITE_KER_APPL_REGISTER;
      req.Hdr.Event_Id  := event_id;
      req.Dif_Name      := dif_name;
      req.Reg           := reg;
      req.Appl_Name     := appl_name;

      return 0;
   end Rl_Register_Req_Fill;

   function RINA_Register_Common (fd : OS.File_Descriptor;
      dif_name : String;
      local_appl : String;
      flags : Integer;
      reg : Integer) return OS.File_Descriptor is

      req : Bindings.Rlite.Kernel_Msg.Rl_Kmsg_Appl_Register;
      ret : Integer;
      wfd : OS.File_Descriptor;
      res : OS.File_Descriptor := -1;
      Bits_Other_Than_NoWait : Unsigned_32 := Unsigned_32 (flags) and not Unsigned_32 (RINA_F_NOWAIT);
   begin

      if Bits_Other_Than_NoWait /= 0 then
         --  Flag has bits other than RINA_F_NOWAIT set, return invalid file descriptor
         Ada.Text_IO.Put_Line ("Flag has bits other than RINA_F_NOWAIT");
         return res;
      end if;

      --  Open dedicated file descriptor to perform the operation and wait for response
      wfd := RINA_Open;

      if wfd < 0 then
         --  Invalid control device file descriptor
         return wfd;
      end if;

      ret := Rl_Register_Req_Fill (req, RINA_REG_EVENT_ID, dif_name,
      reg,
      local_appl);

      --  In this case, ret can only be zero, so ignore any checks here we would normally do in C
      --  if (ret) {
      --     errno = ENOMEM;
      --     goto out;
      --  }

      return res;
   end RINA_Register_Common;
end Bindings.Rlite.Ctrl;