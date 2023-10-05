with Interfaces;

package Bindings.Rlite.Common is
   type Rl_IPCP_Id_T is new Interfaces.Unsigned_16;
   type Rl_Port_T is new Interfaces.Unsigned_16;
   type Rl_Msg_T is new Interfaces.Unsigned_16;

   --  Maximum sizes for data transfer constants, to be used in CDAP messages,
   --  user/kernel interfaces and the management layer in general
   --  TODO: No unsigned 64 bit type available in Ada. Reinvestigate if this will be an issue
   type Rlm_Addr_T is new Interfaces.Unsigned_32;
   type Rlm_Seq_T is new Interfaces.Unsigned_32;
   type Rlm_Pdulen_T is new Interfaces.Unsigned_32;
   type Rlm_Cepid_T is new Interfaces.Unsigned_32;
   type Rlm_Qosid_T is new Interfaces.Unsigned_32;

   -- Header for all rLite messages, all the possible messages begin with this
   type Rl_Msg_Hdr is record
      Version : Interfaces.Unsigned_16;
      Msg_Type : Rl_Msg_T;
      Event_Id : Interfaces.Unsigned_32;
   end record;

   type Rl_Msg_Base is record
      Hdr : Rl_Msg_Hdr;
   end record;

end Bindings.Rlite.Common;