with Bindings.Rlite.Common;
with Interfaces.C.Strings;
with Interfaces;
with System;

package Bindings.Rlite.Kernel_Msg is

   type Rl_Msg_T is new Interfaces.Unsigned_16;
   type Rl_Ipcp_Id_T is new Interfaces.Unsigned_16;

   --  Message types, must be listed alternating requests with corresponding responses
   --  These were originally anonymous enums in C,
   --  converting these to const integers for now
   RLITE_KER_IPCP_CREATE            : constant Integer := 1;
   RLITE_KER_IPCP_CREATE_RESP       : constant Integer := 2;
   RLITE_KER_IPCP_DESTROY           : constant Integer := 3;
   RLITE_KER_APPL_REGISTER          : constant Integer := 4;
   RLITE_KER_APPL_REGISTER_RESP     : constant Integer := 5;
   RLITE_KER_FA_REQ                 : constant Integer := 6;
   RLITE_KER_FA_RESP_ARRIVED        : constant Integer := 7;
   RLITE_KER_FA_RESP                : constant Integer := 8;
   RLITE_KER_FA_REQ_ARRIVED         : constant Integer := 9;
   RLITE_KER_IPCP_CONFIG            : constant Integer := 10;
   RLITE_KER_IPCP_PDUFT_SET         : constant Integer := 11;
   RLITE_KER_IPCP_PDUFT_FLUSH       : constant Integer := 12;
   RLITE_KER_IPCP_UIPCP_SET         : constant Integer := 13;
   RLITE_KER_UIPCP_FA_REQ_ARRIVED   : constant Integer := 14;
   RLITE_KER_UIPCP_FA_RESP_ARRIVED  : constant Integer := 15;
   RLITE_KER_FLOW_DEALLOCATED       : constant Integer := 16;
   RLITE_KER_FLOW_DEALLOC           : constant Integer := 17;
   RLITE_KER_IPCP_UPDATE            : constant Integer := 18;
   RLITE_KER_FLOW_FETCH             : constant Integer := 19;
   RLITE_KER_FLOW_FETCH_RESP        : constant Integer := 20;
   RLITE_KER_IPCP_UIPCP_WAIT        : constant Integer := 21;
   RLITE_KER_FLOW_STATS_REQ         : constant Integer := 22;
   RLITE_KER_FLOW_STATS_RESP        : constant Integer := 23;
   RLITE_KER_FLOW_CFG_UPDATE        : constant Integer := 24;
   RLITE_KER_IPCP_QOS_SUPPORTED     : constant Integer := 25;
   RLITE_KER_APPL_MOVE              : constant Integer := 26;
   RLITE_KER_IPCP_PDUFT_DEL         : constant Integer := 27;
   RLITE_KER_MEMTRACK_DUMP          : constant Integer := 28;
   RLITE_KER_REG_FETCH              : constant Integer := 29;
   RLITE_KER_REG_FETCH_RESP         : constant Integer := 30;
   RLITE_KER_FLOW_STATE             : constant Integer := 31;
   RLITE_KER_IPCP_STATS_REQ         : constant Integer := 32;
   RLITE_KER_IPCP_STATS_RESP        : constant Integer := 33;
   RLITE_KER_IPCP_CONFIG_GET_REQ    : constant Integer := 34;
   RLITE_KER_IPCP_CONFIG_GET_RESP   : constant Integer := 35;
   RLITE_KER_IPCP_SCHED_WRR         : constant Integer := 36;
   RLITE_KER_IPCP_SCHED_PFIFO       : constant Integer := 37;
   RLITE_KER_MSG_MAX                : constant Integer := 38;

   --  (Application --> Kernel) message to create a new IPC process.
   type Rl_Kmsg_IPCP_Create is record
      Hdr : aliased Bindings.Rlite.Common.Rl_Msg_Hdr;
      Name : Interfaces.C.Strings.chars_ptr;
      DIF_Type : Interfaces.C.Strings.chars_ptr;
      DIF_Name : Interfaces.C.Strings.chars_ptr;
   end record;

   --  6 element char array for padding (kernel <--> application) messages
   subtype rl_kmsg_pad1_array is Interfaces.C.char_array (0 .. 5);

   --  (Kernel --> Application) message to inform the application about the ID of a new IPC process.
   type Rl_Kmsg_IPCP_Create_Resp is record
      Hdr : aliased Bindings.Rlite.Common.Rl_Msg_Hdr;
      IPCP_Id : aliased Bindings.Rlite.Common.Rl_IPCP_Id_T;
      Pad1 : aliased rl_kmsg_pad1_array;
   end record;

   --  This message struct is the same as the above ^
   --  (Application --> Kernel) message to destroy an IPC process.
   type Rl_Kmsg_IPCP_Destroy is record
      Hdr : aliased Bindings.Rlite.Common.Rl_Msg_Hdr;
      IPCP_Id : aliased Bindings.Rlite.Common.Rl_IPCP_Id_T;
      Pad1 : aliased rl_kmsg_pad1_array;
   end record;

   --  (Application --> Kernel) message to ask for a list of flows.
   type Rl_Kmsg_Flow_Fetch is record
      Hdr : aliased Bindings.Rlite.Common.Rl_Msg_Hdr;

      --  If ipcp_id != ~0 filter flows by ipcp_id, otherwise fetch all of them.
      IPCP_Id : aliased Bindings.Rlite.Common.Rl_IPCP_Id_T;
      Pad1 : aliased rl_kmsg_pad1_array;
   end record;

   --  (Application <-- Kernel) message to fetch flow information.
   type Rl_Kmsg_Flow_Fetch_Resp is record
      Hdr : aliased Bindings.Rlite.Common.Rl_Msg_Hdr;
      End1 : Interfaces.Unsigned_8; --  We can't use the End keyword, so suffixing it with a 1 for now
      Flow_Control : Interfaces.Unsigned_8;
      IPCP_Id : aliased Bindings.Rlite.Common.Rl_IPCP_Id_T;
      Local_Port : aliased Bindings.Rlite.Common.Rl_Port_T;
      Remote_Port : aliased Bindings.Rlite.Common.Rl_Port_T;
      Local_Addr : aliased Bindings.Rlite.Common.Rlm_Addr_T;
      Remote_Addr : aliased Bindings.Rlite.Common.Rlm_Addr_T;
   end record;

   type Pad1_Arr is array (0 .. 6) of aliased Interfaces.Unsigned_8;
   type Rl_Kmsg_Appl_Register is record
      Hdr : Rl_Msg_T;
      Reg : Interfaces.Unsigned_8;
      Pad1 : Pad1_Arr;
      Appl_Name : System.Address;
      Dif_Name : System.Address;
   end record;

   type Rl_Kmsg_Appl_Register_Resp is record
      Hdr : Rl_Msg_T;
      Ipcp_Id : Rl_Ipcp_Id_T;
      Reg : Interfaces.Unsigned_8;
      Response : Interfaces.Unsigned_8;
      Pad1 : Interfaces.Unsigned_32;
      Appl_Name : System.Address;
   end record;

end Bindings.Rlite.Kernel_Msg;