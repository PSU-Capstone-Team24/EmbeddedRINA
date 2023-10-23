--  Temp disabling
pragma Style_Checks (Off);

with Interfaces; use Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;

package Bindings.Rlite.Common is
   package C renames Interfaces.C;

   type Rl_IPCP_Id_T is new Unsigned_16;
   type Rl_Port_T    is new Unsigned_16;

   --  MT: TODO: Temp set to 2**63 until I can get this to compile lol
   type Unsigned_64 is range 0 .. 2**63 - 1;
   pragma Convention (C, Unsigned_64);

   --  Application naming information:
   --  Application Process Name
   --  Application Process Instance
   --  Application Entity Name
   --  Application Entity Instance
   type Rina_Name is record
      Apn : C.Strings.chars_ptr;
      Api : C.Strings.chars_ptr;
      Aen : C.Strings.chars_ptr;
      Aei : C.Strings.chars_ptr;
   end record;

   --  64-bit type defs
   -- type Rlm_Addr_T is new Unsigned_64;

   --  Maximum sizes for data transfer constants, to be used in CDAP messages,
   --  user/kernel interfaces and the management layer in general
   type Rlm_Addr_T   is new Unsigned_64;
   type Rlm_Seq_T    is new Unsigned_64;
   type Rlm_Pdulen_T is new Unsigned_64;
   type Rlm_Cepid_T  is new Unsigned_64;
   type Rlm_Qosid_T  is new Unsigned_64;

   --  Match values for PDUFT entries.
   type Rl_PCI_Match is record
      Dst_Addr    : Rlm_Addr_T;
      Src_Addr    : Rlm_Addr_T;
      Dst_Cepid   : Rlm_Qosid_T;
      Src_Cepid   : Rlm_Qosid_T;
      Qos_Id      : Rlm_Qosid_T;
      Pad2        : Rlm_Qosid_T;
   end record;

   --  Record to specify the flow QoS parameters asked by
   --  the application issuing a flow allocation request.
   type RINA_Flow_Spec is record
      --  Version = 2 for rLite
      Version : Unsigned_32;
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
      --  MT: TODO: Make this into a boolean?
      In_Order_Delivery : Unsigned_8;
      --  Message boundaries
      --  MT: TODO: Make this into a boolean?
      Msg_Boundaries : Unsigned_8;
      --  Max jitter in microseconds
      Max_Jitter : Unsigned_32;
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
      
      Rmt     : Common.Rl_Rmt_Stats;
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

end Bindings.Rlite.Common;
