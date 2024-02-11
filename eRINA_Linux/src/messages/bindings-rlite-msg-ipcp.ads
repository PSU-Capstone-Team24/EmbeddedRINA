--  Temp disabling
pragma Style_Checks (Off);

package Bindings.Rlite.Msg.IPCP is

   type DIF_Types is (Normal, Ethernet);

   --  (Application --> Kernel) message to create a new IPC process.
   type Create is new Rl_Msg_Base with record
      Ipcp_Name : Bounded_String;
      DIF_Type  : DIF_Types;
      DIF_Name  : Bounded_String;
   end record;
   pragma Pack (Create);

   overriding function Serialize (Self : in Create) return Byte_Buffer;

   type Create_Response is new Rl_Msg_Base with record
      Ipcp_Id : Rl_Ipcp_Id_T;
   end record;
   pragma Pack (Create_Response);

   overriding function Serialize
     (Self : in Create_Response) return Byte_Buffer;

   overriding procedure Deserialize
     (Self : in out Create_Response; Fd : OS.File_Descriptor);

   type Destroy is new Rl_Msg_Base with record
      Ipcp_Id : Rl_Ipcp_Id_T;
   end record;
   pragma Pack (Destroy);

   overriding function Serialize (Self : in Destroy) return Byte_Buffer;

end Bindings.Rlite.Msg.IPCP;
