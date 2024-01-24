--  Temp disabling
pragma Style_Checks (Off);

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

   type Create_Response is new Rl_Msg_Base with record 
      Ipcp_Id : Rl_Ipcp_Id_T;
   end record;
   pragma Pack(Create_Response);

   overriding
   procedure Deserialize (Self : in out Create_Response; fd : OS.File_Descriptor);

   overriding
   function Serialize (Self : in Create_Response) return Byte_Buffer;

end Bindings.Rlite.Msg.IPCP;