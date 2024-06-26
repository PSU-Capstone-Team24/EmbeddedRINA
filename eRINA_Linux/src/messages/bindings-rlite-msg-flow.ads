--  Temp disabling
pragma Style_Checks (Off);

with Bindings.Rlite.Msg; use Bindings.Rlite.Msg;

package Bindings.Rlite.Msg.Flow is

   RINA_FLOW_SPEC_VERSION  : constant Unsigned_32 := 2;
   RINA_FLOW_SPEC_LOSS_MAX : constant Unsigned_16 := 10_000;

   --  Record to specify the flow QoS parameters asked by
   --  the application issuing a flow allocation request.
   type RINA_Flow_Spec is record
      --  Version = 2 for rLite
      Version : Unsigned_32 := RINA_FLOW_SPEC_VERSION;
      --  Max delay in microseconds
      Max_Delay : Unsigned_32 := 0;
      --  Max in SDUs (Service Data Units)
      Max_SDU_Gap : Unsigned_64 := Unsigned_64'Last;
      --  Average bandwith in bits per second
      Avg_Bandwith : Unsigned_64 := 0;
      --  Maximum packet loss from 0 (0%) to 10000 (100%)
      Max_Loss : Unsigned_16 := RINA_FLOW_SPEC_LOSS_MAX;
      --  In order delivery
      In_Order_Delivery : Unsigned_8 := 0;
      --  Message boundaries
      Msg_Boundaries : Unsigned_8 := 0;
      --  Max jitter in microseconds
      Max_Jitter : Unsigned_32 := 0;
   end record;

   function Buffer_To_Flow_Spec is new Ada.Unchecked_Conversion
     (Source => Byte_Buffer, Target => Flow.RINA_Flow_Spec);

   -- struct rl_kmsg_fa_req
   -- (Application --> Kernel) to initiate a flow allocation.
   type Request is new Rl_Msg_Base with record
      Flow_Spec : RINA_Flow_Spec;

      --  The local port, local CEP and unique id are
      --  filled by kernel before reflection to userspace
      Upper_Ipcp_Id : Rl_Ipcp_Id_T;
      Local_Port    : Rl_Port_T;
      Local_Cep     : Rlm_Cepid_T;
      Uid           : Unsigned_32;

      --  A value associated to the requesting application.
      --  It may be used by the uipcp e.g. for consistent load balancing
      Cookie : Unsigned_32;

      Local_Appl  : Bounded_String;
      Remote_Appl : Bounded_String;
      Dif_Name    : Bounded_String;
   end record;

   overriding procedure Deserialize
     (Self : in out Request; fd : OS.File_Descriptor);

   overriding function Serialize (Self : in Request) return Byte_Buffer;

   --  struct rl_kmsg_fa_resp_arrived
   --  (Application <-- Kernel) to notify about an incoming flow response.
   type Response_Arrived is new Rl_Msg_Base with record
      Port_Id  : Rl_Port_T;
      Response : Unsigned_8;
   end record;

   overriding procedure Deserialize
     (Self : in out Response_Arrived; fd : OS.File_Descriptor);

   overriding function Serialize
     (Self : in Response_Arrived) return Byte_Buffer;

   --  struct rl_kmsg_fa_resp
   --  (Application --> Kernel) to respond to an incoming flow request.
   type Response is new Rl_Msg_Base with record
      Kevent_Id : Unsigned_32;

   --  The ipcp_id field is currently unused, since port-id are currently
   --  global, while the architecture says they should be unique only per IPCP
      Ipcp_Id : Rl_Ipcp_Id_T;

      --  The ipcp_id_bind field tells to bind the kernel datapath for this
      --  flow to the specified upper IPCP
      Upper_Ipcp_Id : Rl_Ipcp_Id_T;
      Port_Id       : Rl_Port_T;
      Response      : Unsigned_8;
      Pad1          : Unsigned_8;

      --  Filled by kernel before reflecting to userspace
      Cep_Id : Rlm_Cepid_T;
   end record;

   overriding procedure Deserialize
     (Self : in out Response; fd : OS.File_Descriptor);

   overriding function Serialize (Self : in Response) return Byte_Buffer;

   --  struct rl_kmsg_fa_req_arrived
   --  (Application <-- Kernel) to notify an incoming flow request.
   type Request_Arrived is new Rl_Msg_Base with record
      Kevent_Id   : Unsigned_32;
      Port_Id     : Msg.Rl_Port_T;
      Ipcp_Id     : Msg.Rl_Ipcp_Id_T;
      Flowspec    : Flow.RINA_Flow_Spec;
      Local_Appl  : Bounded_String;
      Remote_Appl : Bounded_String;
      Dif_Name    : Bounded_String;
   end record;

   overriding procedure Deserialize
     (Self : in out Request_Arrived; fd : OS.File_Descriptor);

   overriding function Serialize
     (Self : in Request_Arrived) return Byte_Buffer;

end Bindings.Rlite.Msg.Flow;
