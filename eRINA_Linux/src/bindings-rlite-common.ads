--  Temp disabling
pragma Style_Checks (Off);

with Interfaces; use Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with Bindings.Rlite.Msg; 

package Bindings.Rlite.Common is
   package C renames Interfaces.C;

   type Unsigned_64 is mod 2 ** 64 with size => 64;

   --  Application naming information:
   --  Application Process Name
   --  Application Process Instance
   --  Application Entity Name
   --  Application Entity Instance
   type RINA_Name is record
      Apn : C.Strings.chars_ptr;
      Api : C.Strings.chars_ptr;
      Aen : C.Strings.chars_ptr;
      Aei : C.Strings.chars_ptr;
   end record;

   type Padding_3 is array (0 .. 2) of Unsigned_8;

   type Rl_Ioctl_Info is record
      mode    : Unsigned_8;
      pad1    : Padding_3;
      port_id : Bindings.Rlite.Msg.Rl_Port_T;
      ipcp_id : Bindings.Rlite.Msg.Rl_Ipcp_Id_T;
   end record;

   --  64-bit type defs
   -- type Rlm_Addr_T is new Unsigned_64;

end Bindings.Rlite.Common;
