--  Temp disabling
pragma Style_Checks (Off);

with Bindings.Rlite.Msg;
  use Bindings.Rlite.Msg;

with Buffers;
  use Buffers;

package Bindings.Rlite.Msg.Flow is

   RINA_FLOW_SPEC_VERSION : constant Unsigned_32 := 2;

   --  Record to specify the flow QoS parameters asked by
   --  the application issuing a flow allocation request.
   type RINA_Flow_Spec is record
      --  Version = 2 for rLite
      Version : Unsigned_32 := RINA_FLOW_SPEC_VERSION;
      --  Max delay in microseconds
      Max_Delay : Unsigned_32;
      --  Max in SDUs (Service Data Units)
      --  MT: TODO: This is originally a uint64_t, will this be an issue?
      Max_SDU_Gap : Unsigned_32;
      --  Average bandwith in bits per second
      Avg_Bandwith : Unsigned_32;
      --  Maximum packet loss from 0 (0%) to 10000 (100%)
      Max_Loss : Unsigned_16;
      --  In order delivery
      In_Order_Delivery : Unsigned_8;
      --  Message boundaries
      Msg_Boundaries : Unsigned_8;
      --  Max jitter in microseconds
      Max_Jitter : Unsigned_32;
   end record;

   -- struct rl_kmsg_fa_req
   -- (Application --> Kernel) to initiate a flow allocation.
   type Request(Local_Appl_Size : Natural; Remote_Appl_Size : Natural; Dif_Name_Size : Natural) is new Rl_Msg_Base with record
      Flow_Spec      : RINA_Flow_Spec;
      
      --  The local port, local CEP and unique id are
      --  filled by kernel before reflection to userspace
      Upper_Ipcp_Id  : Rl_Ipcp_Id_T;
      Local_Port     : Rl_Port_T;
      Local_Cep      : Rlm_Cepid_T;
      Uid            : Unsigned_32;

      --  A value associated to the requesting application.
      --  It may be used by the uipcp e.g. for consistent load balancing
      Cookie         : Unsigned_32;

      Local_Appl     : Byte_Buffer(1 .. Local_Appl_Size);
      Remote_Appl    : Byte_Buffer(1 .. Remote_Appl_Size);
      Dif_Name       : Byte_Buffer(1 .. Dif_Name_Size);
   end record;

   overriding
   function Serialize (Self : in Request) return Byte_Buffer;

   --  struct rl_kmsg_fa_resp_arrived
   --  (Application <-- Kernel) to notify about an incoming flow response.
   type Response_Arrived is new Rl_Msg_Base with record
      Port_Id : Rl_Port_T;
      Response : Unsigned_8;
   end record;

   overriding
   function Serialize (Self : in Response_Arrived) return Byte_Buffer;

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
      Port_Id : Rl_Port_T;
      Response : Unsigned_8;
      Pad1 : Unsigned_8;

      --  Filled by kernel before reflecting to userspace
      Cep_Id : Rlm_Cepid_T;
   end record;

   overriding
   function Serialize (Self : in Response) return Byte_Buffer;

   --  (Application <-- Kernel) to notify an incoming flow request.
   type Request_Arrived(Local_Appl_Size : Natural; Remote_Appl_Size : Natural; Dif_Name_Size : Natural) is new Rl_Msg_Base with record
      Kevent_Id   : Unsigned_32;
      Port_Id     : Msg.Rl_Port_T;
      Ipcp_Id     : Msg.Rl_IPCP_Id_T;
      Flowspec    : Flow.RINA_Flow_Spec;
      Local_Appl  : Byte_Buffer(1 .. Local_Appl_Size);
      Remote_Appl : Byte_Buffer(1 .. Remote_Appl_Size);
      Dif_Name    : Byte_Buffer(1 .. Dif_Name_Size);
   end record;

   overriding
   function Serialize (Self : in Request_Arrived) return Byte_Buffer;


end Bindings.Rlite.Msg.Flow;