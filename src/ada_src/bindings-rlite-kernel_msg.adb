--  Temp disabling
pragma Style_Checks (Off);

with Debug;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

package body Bindings.Rlite.Kernel_Msg is
   type Byte_Buffer is array (Natural range <>) of Unsigned_8;

   procedure Display_Bytes (V : T) is
      Bytes : Byte_Buffer (1 .. V'Size / 8)
         with Address => V'Address, Import, Volatile;
   begin
      for Byte of Bytes loop
         Ada.Text_IO.Put ("Byte:");
         Ada.Integer_Text_IO.Put (Integer (Byte), Width => 8, Base => 16);
         Ada.Text_IO.New_Line;
      end loop;
   end Display_Bytes;

end Bindings.Rlite.Kernel_Msg;