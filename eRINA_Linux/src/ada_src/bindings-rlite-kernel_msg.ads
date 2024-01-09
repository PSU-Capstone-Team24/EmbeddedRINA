--  Temp disabling
pragma Style_Checks (Off);

with Bindings.Rlite.Common;

with System;

with Interfaces;
  use Interfaces;

with Interfaces.C.Strings;

with Names;
  use Names.Name_String;

with Buffers;
  use Buffers;

with Bindings.Rlite.Msg;
  use Bindings.Rlite.Msg;

with Bindings.Rlite.Msg.Flow;

package Bindings.Rlite.Kernel_Msg is

   --  Renames
   package Common renames Bindings.Rlite.Common;
   
   type Rl_Msg_Layout is record
      copylen   : Unsigned_32;
      names     : Unsigned_32;
      strings   : Unsigned_32;
      buffers   : Unsigned_32;
      arrays    : Unsigned_32;
   end record;

   type Rl_Msg_Buf_Field is record
      Buf : System.Address;
      Len : Unsigned_32;
   end record;

end Bindings.Rlite.Kernel_Msg;