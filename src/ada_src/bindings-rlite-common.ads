--  Temp disabling
pragma Style_Checks (Off);

with Interfaces; use Interfaces;

package Bindings.Rlite.Common is
   type Rl_IPCP_Id_T is new Unsigned_16;
   type Rl_Port_T    is new Unsigned_16;
   type Rl_Msg_T     is new Unsigned_16;

   type Unsigned_64 is range 0 .. 2**64;
   pragma Convention (C, Unsigned_64);

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

   --  Maximum sizes for data transfer constants, to be used in CDAP messages,
   --  user/kernel interfaces and the management layer in general
   --  TODO: No unsigned 64 bit type available in Ada. Reinvestigate if this will be an issue
   type Rlm_Addr_T   is new Unsigned_32;
   type Rlm_Seq_T    is new Unsigned_32;
   type Rlm_Pdulen_T is new Unsigned_32;
   type Rlm_Cepid_T  is new Unsigned_32;
   type Rlm_Qosid_T  is new Unsigned_32;

   type Pci_Sizes_Pad1 is array(1..3) of Unsigned_16;
   type Pci_Sizes is record
      Addr : Unsigned_16;
      Seq : Unsigned_16;
      Pdulen : Unsigned_16;
      Cepid : Unsigned_16;
      Qosid : Unsigned_16;
      Pad1 : Pci_Sizes_Pad1;
   end record;

   -- Header for all rLite messages, all the possible messages begin with this
   type Rl_Msg_Hdr is record
      Version  : Unsigned_16;
      Msg_Type : Rl_Msg_T;
      Event_Id : Unsigned_32;
   end record;

   type Rl_Msg_Base is record
      Hdr : Rl_Msg_Hdr;
   end record;

end Bindings.Rlite.Common;
