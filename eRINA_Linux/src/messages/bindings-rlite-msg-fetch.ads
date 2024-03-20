--  Temp disabling
pragma Style_Checks (Off);

with Bindings.Rlite.Msg;
  use Bindings.Rlite.Msg;

package Bindings.Rlite.Msg.Fetch is
   
   --  (Application --> Kernel) message to ask for a list of flows
   type Request is new Rl_Msg_Base with record
      --  If ipcp_id != ~0 filter flows by ipcp_id, otherwise fetch all of them
      Ipcp_Id : Rl_Ipcp_Id_T;
   end record;
   pragma Pack(Request);

   overriding
   procedure Deserialize (Self : in out Request; fd : OS.File_Descriptor);

   overriding
   function Serialize (Self : in Request) return Byte_Buffer;

   type Rlm_Addr_T is new Unsigned_64;

   --  (Application <-- Kernel) message to fetch flow information
   type Response is new Rl_Msg_Base with record
      End1           : Unsigned_8;
      Flow_Control   : Unsigned_8;
      Ipcp_Id        : Rl_Ipcp_Id_T;
      Local_Port     : Rl_Port_T;
      Remote_Port    : Rl_Port_T;
      Local_Addr     : Rlm_Addr_T;
      Remote_Addr    : Rlm_Addr_T;
      Spec           : Flow.RINA_Flow_Spec;
   end record;
   pragma Pack(Response);

end Bindings.Rlite.Msg.Register;