with Bindings.Rlite.API;
with Interfaces; use Interfaces;

package Bindings.Rlite.Common is
   type Rl_IPCP_Id_T is new Unsigned_16;
   type Rl_Port_T    is new Unsigned_16;
   type Rl_Msg_T     is new Unsigned_16;

   type Unsigned_64 is range 0 .. 2**64;
   pragma Convention (C, Unsigned_64);

   --  Maximum sizes for data transfer constants, to be used in CDAP messages,
   --  user/kernel interfaces and the management layer in general
   --  TODO: No unsigned 64 bit type available in Ada. Reinvestigate if this will be an issue
   type Rlm_Addr_T   is new Unsigned_32;
   type Rlm_Seq_T    is new Unsigned_32;
   type Rlm_Pdulen_T is new Unsigned_32;
   type Rlm_Cepid_T  is new Unsigned_32;
   type Rlm_Qosid_T  is new Unsigned_32;

   type Pci_Sizes is record
      Addr : Unsigned_16;
      Seq : Unsigned_16;
      Pdulen : Unsigned_16;
      Cepid : Unsigned_16;
      Qosid : Unsigned_16;
      Pad1 : array(1..3) of Unsigned_16; --  MT: Can I really do this?
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
