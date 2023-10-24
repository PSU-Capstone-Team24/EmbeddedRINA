--  Temp disabling
pragma Style_Checks (Off);

with Bindings.Rlite.Common;

with Interfaces.C.Strings;
with Interfaces; use Interfaces;

package Bindings.Rlite.Kernel_Msg is

   package Common renames Bindings.Rlite.Common;

   type Rl_Ipcp_Id_T is new Unsigned_16;

   --  Message types, must be listed alternating requests with corresponding responses   
   type Rl_Msg_T is (RLITE_DUMMY, RLITE_KER_IPCP_CREATE, RLITE_KER_IPCP_CREATE_RESP, RLITE_KER_IPCP_DESTROY,
                     RLITE_KER_APPL_REGISTER, RLITE_KER_APPL_REGISTER_RESP, RLITE_KER_FA_REQ,
                     RLITE_KER_FA_RESP_ARRIVED, RLITE_KER_FA_RESP, RLITE_KER_FA_REQ_ARRIVED,
                     RLITE_KER_IPCP_CONFIG, RLITE_KER_IPCP_PDUFT_SET, RLITE_KER_IPCP_PDUFT_FLUSH,
                     RLITE_KER_IPCP_UIPCP_SET, RLITE_KER_UIPCP_FA_REQ_ARRIVED, RLITE_KER_UIPCP_FA_RESP_ARRIVED,
                     RLITE_KER_FLOW_DEALLOCATED, RLITE_KER_FLOW_DEALLOC, RLITE_KER_IPCP_UPDATE,
                     RLITE_KER_FLOW_FETCH, RLITE_KER_FLOW_FETCH_RESP, RLITE_KER_IPCP_UIPCP_WAIT,
                     RLITE_KER_FLOW_STATS_REQ, RLITE_KER_FLOW_STATS_RESP, RLITE_KER_FLOW_CFG_UPDATE, 
                     RLITE_KER_IPCP_QOS_SUPPORTED, RLITE_KER_APPL_MOVE, RLITE_KER_IPCP_PDUFT_DEL,
                     RLITE_KER_MEMTRACK_DUMP, RLITE_KER_REG_FETCH, RLITE_KER_REG_FETCH_RESP,
                     RLITE_KER_FLOW_STATE, RLITE_KER_IPCP_STATS_REQ, RLITE_KER_IPCP_STATS_RESP,
                     RLITE_KER_IPCP_CONFIG_GET_REQ, RLITE_KER_IPCP_CONFIG_GET_RESP, RLITE_KER_IPCP_SCHED_WRR,
                     RLITE_KER_IPCP_SCHED_PFIFO, RLITE_KER_MSG_MAX);

   -- Header for all rLite messages, all the possible messages begin with this
   type Rl_Msg_Hdr is record
      Version  : Unsigned_16;
      Msg_Type : Rl_Msg_T;
      Event_Id : Unsigned_32;
   end record;

   --  Base message augmented with an ipcp_id
   type Rl_Msg_Ipcp is record
      Hdr      : Rl_Msg_Hdr;
      Ipcp_Id  : Rl_IPCP_Id_T;
      Pad1     : Unsigned_16;
   end record;

   type Rl_Msg_Base is abstract tagged record
      Hdr : Rl_Msg_Hdr;
   end record;
   
   --  (Application --> Kernel) message to create a new IPC process.
   type Rl_Kmsg_IPCP_Create is new Rl_Msg_Base with record
      Name     : C.Strings.chars_ptr;
      DIF_Type : C.Strings.chars_ptr;
      DIF_Name : C.Strings.chars_ptr;
   end record;

   --  MT: 6 element char array for padding (kernel <--> application) messages
   subtype rl_kmsg_pad1_array is C.char_array (0 .. 5);

   --  (Kernel --> Application) message to inform the application about the ID of a new IPC process.
   type Rl_Kmsg_IPCP_Create_Resp is new Rl_Msg_Base with record
      IPCP_Id  : Common.Rl_IPCP_Id_T;
      Pad1     : rl_kmsg_pad1_array;
   end record;

   --  This message struct is the same as the above ^
   --  (Application --> Kernel) message to destroy an IPC process.
   type Rl_Kmsg_IPCP_Destroy is new Rl_Msg_Base with record
      IPCP_Id  : Common.Rl_IPCP_Id_T;
      Pad1     : rl_kmsg_pad1_array;
   end record;

   --  (Application --> Kernel) message to ask for a list of flows.
   type Rl_Kmsg_Flow_Fetch is new Rl_Msg_Base with record

      --  If ipcp_id != ~0 filter flows by ipcp_id, otherwise fetch all of them.
      IPCP_Id  : Common.Rl_IPCP_Id_T;
      Pad1     : rl_kmsg_pad1_array;
   end record;

   --  (Application <-- Kernel) message to fetch flow information.
   type Rl_Kmsg_Flow_Fetch_Resp is new Rl_Msg_Base with record

       --  MT: We can't use the End keyword, so suffixing it with a 1 for now
      End1        : Unsigned_8;
      Flow_Control: Unsigned_8;
      IPCP_Id     : Common.Rl_IPCP_Id_T;
      Local_Port  : Common.Rl_Port_T;
      Remote_Port : Common.Rl_Port_T;
      Local_Addr  : Common.Rlm_Addr_T;
      Remote_Addr : Common.Rlm_Addr_T;
      Spec        : Common.RINA_Flow_Spec;
   end record;

   subtype Rl_Kmsg_Reg_Fetch is Rl_Kmsg_Flow_Fetch;

   -- MT: TODO: Translate Pending into a boolean?
   type Rl_Kmsg_Reg_Fetch_Resp is new Rl_Msg_Base with record

      End1     : Unsigned_8;  --  MT: We can't use the End keyword, so suffixing it with a 1 for now
      Pending  : Unsigned_8; -- Is registration pending?
      Ipcp_Id  : Rl_Ipcp_Id_T;
      Pad1     : Unsigned_32;
      Appl_Name: C.Strings.chars_ptr;
   end record;

   Rl_IPCP_Update_Add      : constant Integer := 1;
   Rl_IPCP_Update_Upd      : constant Integer := 2;
   Rl_IPCP_Update_Uipcp_Del: constant Integer := 3;
   Rl_IPCP_Update_Del      : constant Integer := 4;

   --  (Application <-- Kernel) message to report updated IPCP information.
   type Rl_Kmsg_Ipcp_Update is new Rl_Msg_Base with record

      Update_Type : Unsigned_8;
      Pad1        : Unsigned_8;
      Ipcp_Id     : Rl_Ipcp_Id_T;
      Max_Sdu_Size: Unsigned_32;
      Ipcp_Addr   : Common.Rlm_Addr_T;
      txhdroom    : Unsigned_16;
      rxhdroom    : Unsigned_16;
      tailroom    : Unsigned_16;
      Pad2        : Unsigned_16;
      pcisizes    : Common.Pci_Sizes;
      Ipcp_Name   : C.Strings.chars_ptr;
      Dif_Name    : C.Strings.chars_ptr;
      Dif_Type    : C.Strings.chars_ptr;
   end record;

   Rl_Flow_State_Down   : constant Integer := 0;
   Rl_Flow_State_Up     : constant Integer := 1;

   --  (Application <-- Kernel) message to report flow up/down state.
   type Rl_Kmsg_Flow_State is new Rl_Msg_Base with record
      Ipcp_Id     : Rl_Ipcp_Id_T;
      Local_Port  : Common.Rl_Port_T;
      Flow_State  : Unsigned_16;
      Pad1        : Unsigned_16;
   end record;

   -- (Application --> Kernel) to register a name.
   type Rl_Kmsg_Appl_Register is new Rl_Msg_Base with record
      Reg         : Unsigned_8;
      Pad1        : rl_kmsg_pad1_array;
      Appl_Name   : Interfaces.C.Strings.chars_ptr;
      Dif_Name    : Interfaces.C.Strings.chars_ptr;
   end record;

  --  (Application <-- Kernel) report the result of (un)registration.  
   type Rl_Kmsg_Appl_Register_Resp is new Rl_Msg_Base with record
      Ipcp_Id     : Rl_Ipcp_Id_T;
      Reg         : Unsigned_8;
      Response    : Unsigned_8;
      Pad1        : Unsigned_32;
      Appl_Name   : Interfaces.C.Strings.chars_ptr;
   end record;

  --  (Application --> Kernel) to finalize a registration operation.  
   type Rl_Kmsg_Appl_Move is new Rl_Msg_Base with record
      Ipcp_Id     : Rl_Ipcp_Id_T;
      Pad1        : Unsigned_16;
      Fd          : Integer_32;
   end record;

  --  (Application --> Kernel) to initiate a flow allocation.  
   type Rl_Kmsg_Fa_Req is new Rl_Msg_Base with record
      Flowspec    : Common.RINA_Flow_Spec;
      Upper_Ipcp_Id : Rl_Ipcp_Id_T;
      Local_Port  : Common.Rl_Port_T;
      Local_Cep   : Common.Rlm_Cepid_T;
      Uid         : Unsigned_32;
      Cookie      : Unsigned_32;
      Local_Appl  : Interfaces.C.Strings.chars_ptr;
      Remote_Appl : Interfaces.C.Strings.chars_ptr;
      Dif_Name    : Interfaces.C.Strings.chars_ptr;
   end record;

   --  (Application <-- Kernel) to notify about an incoming flow response.
   type Rl_Kmsg_Fa_Resp_Arrived_Pad1_Arr is array (0 .. 4) of Unsigned_8;
   type Rl_Kmsg_Fa_Resp_Arrived is new Rl_Msg_Base with record
      Port_Id     : Common.Rl_Port_T;
      Response    : Unsigned_8;
      Pad1        : Rl_Kmsg_Fa_Resp_Arrived_Pad1_Arr;
   end record;

   --  (Application <-- Kernel) to notify an incoming flow request.
   type Rl_Kmsg_Fa_Req_Arrived is new Rl_Msg_Base with record
      Kevent_Id   : Unsigned_32;
      Port_Id     : Common.Rl_Port_T;
      Ipcp_Id     : Common.Rl_IPCP_Id_T;
      Flowspec    : Common.RINA_Flow_Spec;
      Local_Appl  : Interfaces.C.Strings.chars_ptr;
      Remote_Appl : Interfaces.C.Strings.chars_ptr;
      Dif_Name    : Interfaces.C.Strings.chars_ptr;
   end record;

   --  (Application --> Kernel) to respond to an incoming flow request.
   type RL_Kmsg_Fa_Resp is new Rl_Msg_Base with record
      Kevent_Id      : Unsigned_32;
      --  The ipcp_id field is currently unused, since port-id are currently
      --  global, while the architecture says they should be unique only per
      --  IPCP.
      Ipcp_Id        : Common.Rl_IPCP_Id_T;
      --  The ipcp_id_bind field tells to bind the kernel datapath for this
      --  flow to the specified upper IPCP.
      Upper_Ipcp_Id  : Common.Rl_IPCP_Id_T;
      Port_Id        : Common.Rl_Port_T;
      Response       : Unsigned_8;
      Pad1           : Unsigned_8;
      Cep_Id         : Common.Rlm_Cepid_T;
   end record;

   --  (Application --> Kernel) to configure an IPC process.
   type RL_Kmsg_Ipcp_Config_Pad1 is array(0 .. 2) of Unsigned_16;
   type RL_Kmsg_Ipcp_Config is new Rl_Msg_Base with record
      Ipcp_Id  : Common.Rl_IPCP_Id_T;
      Pad1     : RL_Kmsg_Ipcp_Config_Pad1;
      Name     : Interfaces.C.Strings.chars_ptr;
      Value    : Interfaces.C.Strings.chars_ptr;
   end record;

   --  (Application --> Kernel) to ask for IPCP config value.
   type RL_Kmsg_Ipcp_Config_Get_Req_Pad1 is array(0 .. 2) of Unsigned_16;
   type RL_Kmsg_Ipcp_Config_Get_Req is new Rl_Msg_Base with record
      Ipcp_Id     : Common.Rl_IPCP_Id_T;
      Pad1        : RL_Kmsg_Ipcp_Config_Get_Req_Pad1;
      Param_Name  : Interfaces.C.Strings.chars_ptr;
   end record;

   --  (Application <-- Kernel) to return IPCP config value.
   type RL_Kmsg_Ipcp_Config_Get_Resp is new Rl_Msg_Base with record
      Param_Value : Interfaces.C.Strings.chars_ptr;
   end record;

   -- (Application --> Kernel) to modify an IPCP PDUFT
   -- (PDU Forwarding Table) entry.
   type RL_Kmsg_Ipcp_Pduft_Mod is new Rl_Msg_Base with record
      --  The IPCP whose PDUFT is to be modified.
      Ipcp_Id     : Common.Rl_IPCP_Id_T;
      --  The local port where matching packets must be forwarded.
      Local_Port  : Common.Rl_Port_T;
      Pad1        : Unsigned_32;
      --  Values of PCI fields that must match in order for this
      --  entry to be selected.
      Match       : Common.Rl_PCI_Match;
   end record;

   --  (Application --> Kernel) message to flush the PDUFT of an IPC Process.
   subtype Rl_Kmsg_Ipcp_Pduft_Flush is Rl_Kmsg_IPCP_Create_Resp;

   --  uipcp (Application --> Kernel) to tell the kernel that this event
   --  loop corresponds to an uipcp
   type Rl_Kmsg_Ipcp_Uipcp_Set_Pad1 is array (0 .. 2) of Unsigned_16;
   type Rl_Kmsg_Ipcp_Uipcp_Set is new Rl_Msg_Base with record
      Ipcp_Id     : Rl_Ipcp_Id_T;
      Pad1        : Rl_Kmsg_Ipcp_Uipcp_Set_Pad1;
   end record;

   subtype Rl_Kmsg_Ipcp_Uipcp_Wait is Rl_Kmsg_Ipcp_Uipcp_Set;

   --  (Application --> Kernel) to tell the kernel that a flow allocation request has arrived.
   type RL_Kmsg_Uipcp_Fa_Req_Arrived is new Rl_Msg_Base with record
      Kevent_Id   : Unsigned_32;
      Ipcp_Id     : Common.Rl_IPCP_Id_T;
      Remote_Port : Common.Rl_Port_T;
      Remote_Cep  : Common.Rlm_Cepid_T;
      Qos_Id      : Common.Rlm_Qosid_T;
      Remote_Addr : Common.Rlm_Addr_T;
      Flowcfg     : Common.Rl_Flow_Config;
      Flowspec    : Common.RINA_Flow_Spec;
      --  Request application
      Local_Appl  : Interfaces.C.Strings.chars_ptr;
      --  Requesting application
      Remote_Appl : Interfaces.C.Strings.chars_ptr;
   end record;

   --  uipcp (Application --> Kernel) to tell the kernel that a flow
   --  allocation response has arrived
   type Rl_Kmsg_Uipcp_Fa_Resp_Arrived is new Rl_Msg_Base with record
      Ipcp_Id     : Common.Rl_IPCP_Id_T;
      Local_Port  : Common.Rl_Port_T;
      Remote_Port : Common.Rl_Port_T;
      Response    : Unsigned_8;
      Pad1        : Unsigned_8;
      Remote_Cep  : Common.Rlm_Cepid_T;
      Qos_Id      : Common.Rlm_Cepid_T;
      Remote_Addr : Common.Rlm_Addr_T;
      Flowcfg     : Common.Rl_Flow_Config;
   end record;

   --  uipcp (Application --> Kernel) to update the configuration
   --  of a flow
   type Rl_Kmsg_Flow_Cfg_Update is new Rl_Msg_Base with record
      Ipcp_Id     : Common.Rl_IPCP_Id_T;
      Port_Id     : Common.Rl_Port_T;
      Pad1        : Unsigned_32;
      Flowcfg     : Common.Rl_Flow_Config;
   end record;

   --  uipcp (Application <-- Kernel) to inform an uipcp that
   --  a flow has been deallocated locally
   type Rl_Kmsg_Flow_Deallocated is new Rl_Msg_Base with record
      Ipcp_Id        : Common.Rl_IPCP_Id_T;
      Local_Port_Id  : Common.Rl_Port_T;
      Remote_Port_Id : Common.Rl_Port_T;
      Pad1           : Unsigned_16;
      Remote_Addr    : Common.Rlm_Addr_T;
   end record;
   
   --  uipcp (Application --> Kernel) message to ask
   --  for a flow to be deallocated
   type Rl_Kmsg_Flow_Dealloc is new Rl_Msg_Base with record
      Ipcp_Id     : Common.Rl_IPCP_Id_T;
      Port_Id     : Common.Rl_Port_T;

      --  Unique id of the flow to deallocate, used to avoid
      --  accidental removal of different flows reusing the
      --  same port-id
      Uid         : Unsigned_32;
   end record;

   --  (Application --> Kernel) message to ask for
   --  statistics of a given flow
   type Rl_Kmsg_Flow_Stats_Req_Pad1 is array (0 .. 2) of Unsigned_16;
   type Rl_Kmsg_Flow_Stats_Req is new Rl_Msg_Base with record
      Port_Id     : Common.Rl_Port_T;
      Pad1        : RL_Kmsg_Flow_Stats_Req_Pad1;
   end record;

   --  (Application <-- Kernel) message to report statistics
   --  about a given flow.
   type Rl_Kmsg_Flow_Stats_Resp is new Rl_Msg_Base with record
      Stats       : Common.Rl_Flow_Stats;
      Dtp         : Common.Rl_Flow_Dtp;
   end record;

   --  (Application --> Kernel) message to ask an IPCP if a given
   --  QoS can be supported
   type Rl_Kmsg_Ipcp_Qos_Supported_Pad1 is array (0 .. 2) of Unsigned_16;
   type Rl_Kmsg_Ipcp_Qos_Supported is new Rl_Msg_Base with record
      Ipcp_Id     : Common.Rl_IPCP_Id_T;
      Pad1        : Rl_Kmsg_Ipcp_Config_Pad1;
      Flowspec    : Common.RINA_Flow_Spec;
   end record;

   --  (Application --> Kernel) message to ask for
   --  statistics of a given IPCP RMT.
   type Rl_Kmsg_Ipcp_Stats_Req_Pad1 is array (0 .. 2) of Unsigned_16;
   type Rl_Kmsg_Ipcp_Stats_Req is new Rl_Msg_Base with record
      Ipcp_Id     : Common.Rl_IPCP_Id_T;
      Pad1        : Rl_Kmsg_Ipcp_Stats_Req_Pad1;
   end record;

   --  (Application <-- Kernel) message to report statistics
   --  about a given IPCP RMT.
   type Rl_Kmsg_Ipcp_Stats_Resp is new Rl_Msg_Base with record
      Stats       : Common.Rl_Ipcp_Stats;
   end record;

   --  MT: TODO: Re-enable this when Rl_Msg_Array_Field is implemented
   --  (Application --> Kernel) message to configure a WRR PDU scheduler.
   --  type Rl_Kmsg_Ipcp_Sched_WRR is new Rl_Msg_Base with record
   --     Ipcp_Hdr    : Common.Rl_Msg_Ipcp;

      --  Max queue size in bytes.
   --     Max_Queue_Size : Unsigned_32;

      --  Quantum size in bytes.
   --     Quantum     : Unsigned_32;

      --  WRR weights are dwords.
   --     Weights     : Common.Rl_Msg_Array_Field;
   --  end record;

end Bindings.Rlite.Kernel_Msg;