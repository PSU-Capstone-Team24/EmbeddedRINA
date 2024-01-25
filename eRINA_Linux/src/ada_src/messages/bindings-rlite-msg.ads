--  Temp disabling
pragma Style_Checks (Off);

with Ada.Unchecked_Conversion;

with Interfaces;
  use Interfaces;

with Buffers;
  use Buffers;

with Names;
  use Names.Name_String;

with GNAT.OS_Lib;

package Bindings.Rlite.Msg is

   -- Renames
   package OS renames GNAT.OS_Lib;

   --  Types
   type Rl_Ipcp_Id_T is new Unsigned_16;
   type Rl_Port_T    is new Unsigned_16;

   --  Maximum sizes for data transfer constants, to be used in CDAP messages,
   --  user/kernel interfaces and the management layer in general
   type Rlm_Addr_T   is new Unsigned_64;
   type Rlm_Seq_T    is new Unsigned_64;
   type Rlm_Pdulen_T is new Unsigned_64;
   type Rlm_Cepid_T  is new Unsigned_32;
   type Rlm_Qosid_T  is new Unsigned_64;

   --  Constants
   --  RLite version, hardcoded to 8
   RLITE_API_VERSION : constant Unsigned_16 := 8;

   --  Casual value used for assert (0x7a6b)
   RINA_REG_EVENT_ID : constant Unsigned_32 := 16#7a6b#;

   -- Casual value used for assert (0x6271)
   RINA_FA_EVENT_ID : constant Unsigned_32 := 16#6271#;

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

   --  Make sure this is always 16 bits 
   for Rl_Msg_T'Size use 16;

   --  Header for all rLite messages, all the possible messages begin with this
   type Rl_Msg_Hdr is record
      Version  : Unsigned_16  := RLITE_API_VERSION;
      Msg_Type : Rl_Msg_T     := RLITE_DUMMY;
      Event_Id : Unsigned_32;
   end record;

   --  Only use these when we are super-duper sure we know what we're doing
   --  Otherwise what we assume is usable data can end up being garbage...
   function Buffer_To_Rl_Msg_Hdr is
      new Ada.Unchecked_Conversion (Source => Byte_Buffer,
                                    Target => Rl_Msg_Hdr);
                                    
   function Buffer_To_Unsigned_8 is
      new Ada.Unchecked_Conversion (Source => Byte_Buffer,
                                    Target => Unsigned_8);

   function Buffer_To_Unsigned_16 is
      new Ada.Unchecked_Conversion (Source => Byte_Buffer,
                                    Target => Unsigned_16);

   function Buffer_To_Unsigned_32 is
      new Ada.Unchecked_Conversion (Source => Byte_Buffer,
                                    Target => Unsigned_32);


   type Rl_Msg_Base is abstract tagged record
      Hdr : Rl_Msg_Hdr;
   end record;

   --  Abstract base serialization function, must be overridden by child messages
   function Serialize (Self : in Rl_Msg_Base) return Byte_Buffer is abstract;
   procedure Deserialize (Self : in out Rl_Msg_Base; fd : OS.File_Descriptor) is null;

   function Read_Next_Msg(fd : OS.File_Descriptor) return Byte_Buffer;

   --  Match values for PDUFT entries.
   type Rl_PCI_Match is record
      Dst_Addr    : Rlm_Addr_T;
      Src_Addr    : Rlm_Addr_T;
      Dst_Cepid   : Rlm_Qosid_T;
      Src_Cepid   : Rlm_Qosid_T;
      Qos_Id      : Rlm_Qosid_T;
      Pad2        : Rlm_Qosid_T;
   end record;

   --  Statistics for an rl_io device
   type Rl_Flow_Stats is record
      tx_pkt         : Unsigned_64;
      tx_byte        : Unsigned_64;
      rx_pkt         : Unsigned_64;
      rx_byte        : Unsigned_64;
      rx_overrun_pkt : Unsigned_64;
      rx_overrun_byte : Unsigned_64;
   end record;

   --  RMT statistics. All counters must be 64 bits wide
   type Rl_Rmt_Stats is record
      Fwd_Pkt        : Unsigned_64;
      Fwd_Byte       : Unsigned_64;
      Queued_Pkt     : Unsigned_64;
      Queue_Drop     : Unsigned_64;
      Noroute_Drop   : Unsigned_64;
      Csum_Drop      : Unsigned_64;
      Ttl_Drop       : Unsigned_64;
      Noflow_Drop    : Unsigned_64;
      Other_Drop     : Unsigned_64;
   end record;

   --  IPCP statistics. All counters must be 64 bits wide. 
   type RL_Ipcp_Stats is record
      TX_Pkt  : Unsigned_64;
      TX_Byte : Unsigned_64;
      TX_Err  : Unsigned_64;
      
      RX_Pkt  : Unsigned_64;
      RX_Byte : Unsigned_64;
      RX_Err  : Unsigned_64;
      
      RTX_Pkt : Unsigned_64;
      RTX_Byte: Unsigned_64;
      
      Rmt     : Rl_Rmt_Stats;
   end record;

   --  DTP state exported to userspace
   type Rl_Flow_DTP is record
      -- Sender state
      Snd_LWE              : RLM_Seq_T;
      Snd_RWE              : RLM_Seq_T;
      Next_Seq_Num_To_Use  : RLM_Seq_T;
      Last_Seq_Num_Sent    : RLM_Seq_T;
      Last_Ctrl_Seq_Num_Rcvd : RLM_Seq_T;
      CWQ_Len              : Unsigned_32;
      Max_CWQ_Len          : Unsigned_32;
      RTXQ_Len             : Unsigned_32;
      Max_RTXQ_Len         : Unsigned_32;

      -- Estimated round trip time, in usecs
      RTT                  : Unsigned_32;

      -- Standard deviation in usecs
      RTT_Stddev           : Unsigned_32;

      -- Congestion window size, in PDUs
      CGWin                : Unsigned_32;
      Pad1                 : Unsigned_32;

      -- Receiver state
      Rcv_LWE              : RLM_Seq_T;
      Rcv_Next_Seq_Num     : RLM_Seq_T;
      Rcv_RWE              : RLM_Seq_T;
      Max_Seq_Num_Rcvd     : RLM_Seq_T;
      Last_LWE_Sent        : RLM_Seq_T;
      Last_Seq_Num_Acked   : RLM_Seq_T;
      Next_Snd_Ctl_Seq     : Unsigned_32;
      Seqq_Len             : Unsigned_32;
      Pad2                 : Unsigned_32;
   end record;

   type Rl_Flow_Config_Pad1 is array(0 .. 5) of Unsigned_8;
   type Rl_Flow_Config_Pad2 is array(0 .. 2) of Unsigned_16;
   type Rl_Flow_Config is record
      Msg_Boundaries    : Unsigned_8;
      In_Order_Delivery : Unsigned_8;
      Pad1              : Rl_Flow_Config_Pad1;
      Max_Sdu_Gap       : Rlm_Seq_T;
      
      --  Currently used by shim-tcp4 and shim-udp4
      Fd                : Integer_32;
      Inet_Ip           : Unsigned_32;
      Inet_Port         : Unsigned_16;
      Pad2              : Rl_Flow_Config_Pad2;
   end record;
   
   type Pci_Sizes_Pad1 is array(1..3) of Unsigned_16;
   type Pci_Sizes is record
      Addr : Unsigned_16;
      Seq : Unsigned_16;
      Pdulen : Unsigned_16;
      Cepid : Unsigned_16;
      Qosid : Unsigned_16;
      Pad1 : Pci_Sizes_Pad1;
   end record;

end Bindings.Rlite.Msg;