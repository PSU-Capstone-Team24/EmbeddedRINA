--  Temp disabling
pragma Style_Checks (Off);

with Bindings.Rlite.Common;
with Interfaces.C.Strings;
with Interfaces; use Interfaces;

package Bindings.Rlite.Kernel_Msg is

   package Common renames Bindings.Rlite.Common;

   type Rl_Msg_T     is new Unsigned_16;
   type Rl_Ipcp_Id_T is new Unsigned_16;

   --  Message types, must be listed alternating requests with corresponding responses
   --  MT:  These were originally anonymous enums in C,
   --       converting these to const integers for now
   RLITE_KER_IPCP_CREATE            : constant Rl_Msg_T := 1;
   RLITE_KER_IPCP_CREATE_RESP       : constant Rl_Msg_T := 2;
   RLITE_KER_IPCP_DESTROY           : constant Rl_Msg_T := 3;
   RLITE_KER_APPL_REGISTER          : constant Rl_Msg_T := 4;
   RLITE_KER_APPL_REGISTER_RESP     : constant Rl_Msg_T := 5;
   RLITE_KER_FA_REQ                 : constant Rl_Msg_T := 6;
   RLITE_KER_FA_RESP_ARRIVED        : constant Rl_Msg_T := 7;
   RLITE_KER_FA_RESP                : constant Rl_Msg_T := 8;
   RLITE_KER_FA_REQ_ARRIVED         : constant Rl_Msg_T := 9;
   RLITE_KER_IPCP_CONFIG            : constant Rl_Msg_T := 10;
   RLITE_KER_IPCP_PDUFT_SET         : constant Rl_Msg_T := 11;
   RLITE_KER_IPCP_PDUFT_FLUSH       : constant Rl_Msg_T := 12;
   RLITE_KER_IPCP_UIPCP_SET         : constant Rl_Msg_T := 13;
   RLITE_KER_UIPCP_FA_REQ_ARRIVED   : constant Rl_Msg_T := 14;
   RLITE_KER_UIPCP_FA_RESP_ARRIVED  : constant Rl_Msg_T := 15;
   RLITE_KER_FLOW_DEALLOCATED       : constant Rl_Msg_T := 16;
   RLITE_KER_FLOW_DEALLOC           : constant Rl_Msg_T := 17;
   RLITE_KER_IPCP_UPDATE            : constant Rl_Msg_T := 18;
   RLITE_KER_FLOW_FETCH             : constant Rl_Msg_T := 19;
   RLITE_KER_FLOW_FETCH_RESP        : constant Rl_Msg_T := 20;
   RLITE_KER_IPCP_UIPCP_WAIT        : constant Rl_Msg_T := 21;
   RLITE_KER_FLOW_STATS_REQ         : constant Rl_Msg_T := 22;
   RLITE_KER_FLOW_STATS_RESP        : constant Rl_Msg_T := 23;
   RLITE_KER_FLOW_CFG_UPDATE        : constant Rl_Msg_T := 24;
   RLITE_KER_IPCP_QOS_SUPPORTED     : constant Rl_Msg_T := 25;
   RLITE_KER_APPL_MOVE              : constant Rl_Msg_T := 26;
   RLITE_KER_IPCP_PDUFT_DEL         : constant Rl_Msg_T := 27;
   RLITE_KER_MEMTRACK_DUMP          : constant Rl_Msg_T := 28;
   RLITE_KER_REG_FETCH              : constant Rl_Msg_T := 29;
   RLITE_KER_REG_FETCH_RESP         : constant Rl_Msg_T := 30;
   RLITE_KER_FLOW_STATE             : constant Rl_Msg_T := 31;
   RLITE_KER_IPCP_STATS_REQ         : constant Rl_Msg_T := 32;
   RLITE_KER_IPCP_STATS_RESP        : constant Rl_Msg_T := 33;
   RLITE_KER_IPCP_CONFIG_GET_REQ    : constant Rl_Msg_T := 34;
   RLITE_KER_IPCP_CONFIG_GET_RESP   : constant Rl_Msg_T := 35;
   RLITE_KER_IPCP_SCHED_WRR         : constant Rl_Msg_T := 36;
   RLITE_KER_IPCP_SCHED_PFIFO       : constant Rl_Msg_T := 37;
   RLITE_KER_MSG_MAX                : constant Rl_Msg_T := 38;

   --  (Application --> Kernel) message to create a new IPC process.
   type Rl_Kmsg_IPCP_Create is record
      Hdr      : Common.Rl_Msg_Hdr;
      Name     : C.Strings.chars_ptr;
      DIF_Type : C.Strings.chars_ptr;
      DIF_Name : C.Strings.chars_ptr;
   end record;

   --  MT: 6 element char array for padding (kernel <--> application) messages
   subtype rl_kmsg_pad1_array is C.char_array (0 .. 5);

   --  (Kernel --> Application) message to inform the application about the ID of a new IPC process.
   type Rl_Kmsg_IPCP_Create_Resp is record
      Hdr      : Common.Rl_Msg_Hdr;
      IPCP_Id  : Common.Rl_IPCP_Id_T;
      Pad1     : rl_kmsg_pad1_array;
   end record;

   --  This message struct is the same as the above ^
   --  (Application --> Kernel) message to destroy an IPC process.
   type Rl_Kmsg_IPCP_Destroy is record
      Hdr      : Common.Rl_Msg_Hdr;
      IPCP_Id  : Common.Rl_IPCP_Id_T;
      Pad1     : rl_kmsg_pad1_array;
   end record;

   --  (Application --> Kernel) message to ask for a list of flows.
   type Rl_Kmsg_Flow_Fetch is record
      Hdr      : Common.Rl_Msg_Hdr;

      --  If ipcp_id != ~0 filter flows by ipcp_id, otherwise fetch all of them.
      IPCP_Id  : Common.Rl_IPCP_Id_T;
      Pad1     : rl_kmsg_pad1_array;
   end record;

   --  (Application <-- Kernel) message to fetch flow information.
   type Rl_Kmsg_Flow_Fetch_Resp is record
      Hdr : Common.Rl_Msg_Hdr;

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
   type Rl_Kmsg_Reg_Fetch_Resp is record
      Hdr : Common.Rl_Msg_Hdr;

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
   type Rl_Kmsg_Ipcp_Update is record
      Hdr : Common.Rl_Msg_Hdr;

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
   type Rl_Kmsg_Flow_State is record
      Hdr         : Common.Rl_Msg_Hdr;
      Ipcp_Id     : Rl_Ipcp_Id_T;
      Local_Port  : Common.Rl_Port_T;
      Flow_State  : Unsigned_16;
      Pad1        : Unsigned_16;
   end record;

   -- (Application --> Kernel) to register a name.
   type Rl_Kmsg_Appl_Register is record
      Hdr         : Common.Rl_Msg_Hdr;
      Reg         : Unsigned_8;
      Pad1        : rl_kmsg_pad1_array;
      Appl_Name   : Interfaces.C.Strings.chars_ptr;
      Dif_Name    : Interfaces.C.Strings.chars_ptr;
   end record;

  --  (Application <-- Kernel) report the result of (un)registration.  
   type Rl_Kmsg_Appl_Register_Resp is record
      Hdr         : Common.Rl_Msg_Hdr;
      Ipcp_Id     : Rl_Ipcp_Id_T;
      Reg         : Unsigned_8;
      Response    : Unsigned_8;
      Pad1        : Unsigned_32;
      Appl_Name   : Interfaces.C.Strings.chars_ptr;
   end record;

  --  (Application --> Kernel) to finalize a registration operation.  
   type Rl_Kmsg_Appl_Move is record
      Hdr         : Common.Rl_Msg_Hdr;
      Ipcp_Id     : Rl_Ipcp_Id_T;
      Pad1        : Unsigned_16;
      Fd          : Integer_32;
   end record;

  --  (Application --> Kernel) to initiate a flow allocation.  
   type Rl_Kmsg_Fa_Req is record
      Hdr         : Common.Rl_Msg_Hdr;
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
   type Rl_Kmsg_Fa_Resp_Arrived is record
      Hdr         : Common.Rl_Msg_Hdr;
      Port_Id     : Common.Rl_Port_T;
      Response    : Unsigned_8;
      Pad1        : Rl_Kmsg_Fa_Resp_Arrived_Pad1_Arr;
   end record;

   --  (Application <-- Kernel) to notify an incoming flow request.
   type Rl_Kmsg_Fa_Req_Arrived is record
      Hdr         : Common.Rl_Msg_Hdr;
      Kevent_Id   : Unsigned_32;
      Port_Id     : Common.Rl_Port_T;
      Ipcp_Id     : Common.Rl_IPCP_Id_T;
      Flowspec    : Common.RINA_Flow_Spec;
      Local_Appl  : Interfaces.C.Strings.chars_ptr;
      Remote_Appl : Interfaces.C.Strings.chars_ptr;
      Dif_Name    : Interfaces.C.Strings.chars_ptr;
   end record;

   --  (Application --> Kernel) to respond to an incoming flow request.
   type RL_Kmsg_Fa_Resp is record
      Hdr            : Common.Rl_Msg_Hdr;
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
   type RL_Kmsg_Ipcp_Config is record
      Hdr      : Common.Rl_Msg_Hdr;
      Ipcp_Id  : Common.Rl_IPCP_Id_T;
      Pad1     : RL_Kmsg_Ipcp_Config_Pad1;
      Name     : Interfaces.C.Strings.chars_ptr;
      Value    : Interfaces.C.Strings.chars_ptr;
   end record;

   --  (Application --> Kernel) to ask for IPCP config value.
   type RL_Kmsg_Ipcp_Config_Get_Req_Pad1 is array(0 .. 2) of Unsigned_16;
   type RL_Kmsg_Ipcp_Config_Get_Req is record
      Hdr         : Common.Rl_Msg_Hdr;
      Ipcp_Id     : Common.Rl_IPCP_Id_T;
      Pad1        : RL_Kmsg_Ipcp_Config_Get_Req_Pad1;
      Param_Name  : Interfaces.C.Strings.chars_ptr;
   end record;

   --  (Application <-- Kernel) to return IPCP config value.
   type RL_Kmsg_Ipcp_Config_Get_Resp is record
      Hdr         : Common.Rl_Msg_Hdr;
      Param_Value : Interfaces.C.Strings.chars_ptr;
   end record;

   -- (Application --> Kernel) to modify an IPCP PDUFT
   -- (PDU Forwarding Table) entry.
   type RL_Kmsg_Ipcp_Pduft_Mod is record
      Hdr         : Common.Rl_Msg_Hdr;
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

   --  (Application --> Kernel) to tell the kernel that a flow allocation request has arrived.
   type RL_Kmsg_Uipcp_Fa_Req_Arrived is record
      Hdr         : Common.Rl_Msg_Hdr;
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

end Bindings.Rlite.Kernel_Msg;