--  Temp disabling
pragma Style_Checks (Off);

with Interfaces.C.Strings;
with Bindings.Rlite.API;
with Bindings.Rlite.Kernel_Msg;

with Ada.Text_IO;

with System;

package body Bindings.Rlite.Ctrl is
   --  function Rl_Register_Req_Fill (req : in out Bindings.Rlite.Kernel_Msg.Rl_Kmsg_Appl_Register;
   --  event_id : Unsigned_32;
   --  dif_name : String;
   --  reg : Unsigned_8;
   --  appl_name: String) return Integer is
   --  begin
   --     req.Hdr.Msg_Type  := Bindings.Rlite.Common.Rl_Msg_T(Bindings.Rlite.Kernel_Msg.RLITE_KER_APPL_REGISTER);
   --     req.Hdr.Event_Id  := event_id;
   --     req.Dif_Name      := Interfaces.C.Strings.New_String (dif_name);
   --     req.Reg           := reg;
   --     req.Appl_Name     := Interfaces.C.Strings.New_String (appl_name);
   --
   --     return 0;
   --  end Rl_Register_Req_Fill;
   --
   function Rl_Register_Req_Fill
     (req : Bindings.Rlite.Kernel_Msg.Rl_Kmsg_Appl_Register;
      event_id : Unsigned_32;
      dif_name : Interfaces.C.Strings.chars_ptr;
      reg : Unsigned_8;
      appl_name : Interfaces.C.Strings.chars_ptr) return Integer;
   pragma Import (C, Rl_Register_Req_Fill, "rl_register_req_fill");

   function Rl_Write_Msg
     (rfd : Integer;
      msg : System.Address;
      quiet : Integer) return Integer;
   pragma Import (C, Rl_Write_Msg, "rl_write_msg");

   function RINA_Register_Common (fd : OS.File_Descriptor;
      dif_name : String;
      local_appl : String;
      flags : Integer;
      reg : Unsigned_8) return OS.File_Descriptor is

      req : Bindings.Rlite.Kernel_Msg.Rl_Kmsg_Appl_Register;
      ret : Integer;
      wfd : OS.File_Descriptor;
      res : constant OS.File_Descriptor := OS.File_Descriptor (-1);
      Bits_Other_Than_NoWait : constant Unsigned_32 := Unsigned_32 (flags) and not Unsigned_32 (Bindings.Rlite.API.RINA_F_NOWAIT);
      req_addr : System.Address;
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

      Ada.Text_IO.Put_Line ("After Rl_Register_Req_Fill, req is now:");
      Ada.Text_IO.Put_Line ("AppName: " & Interfaces.C.Strings.Value(req.Appl_Name));
      Ada.Text_IO.Put_Line ("DifName: " & Interfaces.C.Strings.Value(req.Dif_Name) );

      --  In this case, ret can only be zero, so ignore any checks here we would normally do in C
      --  if (ret) {
      --     errno = ENOMEM;
      --     goto out;
      --  }

      req_addr := req'Address;
      ret := Rl_Write_Msg (Integer(wfd), req_addr, 1);

      return res;
   end RINA_Register_Common;
end Bindings.Rlite.Ctrl;