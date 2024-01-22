--  Temp disabling
pragma Style_Checks (Off);

with Bindings.Rlite.Msg;
  use Bindings.Rlite.Msg;

package Bindings.Rlite.Msg.IPCP is

   type DIF_Types is (Normal, Ethernet);

   --  (Application --> Kernel) message to create a new IPC process.
   type Create is new Rl_Msg_Base with record
      Ipcp_Name : Bounded_String;
      DIF_Type : DIF_Types;
      DIF_Name : Bounded_String;
   end record;
   pragma Pack(Create);

   overriding
   procedure Deserialize (Self : in out Create; fd : OS.File_Descriptor);

   overriding
   function Serialize (Self : in Create) return Byte_Buffer;

end Bindings.Rlite.Msg.IPCP;