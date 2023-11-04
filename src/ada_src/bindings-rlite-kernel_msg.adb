--  Temp disabling
pragma Style_Checks (Off);

with GNAT.OS_Lib;
with Names; use Names;

package body Bindings.Rlite.Kernel_Msg is

   procedure Send (Self : Rl_Msg_Base) is
   begin
      --  MT: TODO: Implement later
      return;
   end Send;

   -- MT: DEBUG ONLY
   procedure Rl_Write_Msg_From_Ada
     (Rfd : GNAT.OS_Lib.File_Descriptor;
     Msg : Byte_Buffer;
     Msg_Size : Positive;
     Quiet : Integer);
   pragma Import (C, Rl_Write_Msg_From_Ada, "rl_write_msg_from_ada");

   function Serialize (V : T) return Byte_Buffer is
      Bytes : Byte_Buffer (1 .. V'Size / 8)
         with Address => V'Address, Import, Volatile;
   begin
      Rl_Write_Msg_From_Ada (0, Bytes, Bytes'Size / 8, 0);
      return Bytes;
   end Serialize;

end Bindings.Rlite.Kernel_Msg;