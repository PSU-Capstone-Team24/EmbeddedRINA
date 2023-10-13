--  Temp disabling
pragma Style_Checks (Off);

with System;
with Interfaces; use Interfaces;
with Bindings.Rlite.Kernel_Msg;

package Bindings.Rlite.Utils is

    --  Because I don't want to keep having to type Bindings.Rlite....
    package Kernel_Msg renames Bindings.Rlite.Kernel_Msg;

    type Rl_Msg_Layout is record
        Copylen   : Unsigned_32;
        Names     : Unsigned_32;
        Strings   : Unsigned_32;
        Buffers   : Unsigned_32;
        Arrays    : Unsigned_32;
    end record;

   --  Kernel message type enums
   type RLITE_KER is (RLITE_KER_IPCP_CREATE, RLITE_KER_IPCP_CREATE_RESP, RLITE_KER_IPCP_DESTROY,
                  RLITE_KER_FLOW_FETCH, RLITE_KER_FLOW_FETCH_RESP, RLITE_KER_IPCP_UPDATE,
                  RLITE_KER_APPL_REGISTER, RLITE_KER_APPL_REGISTER_RESP, RLITE_KER_FA_REQ,
                  RLITE_KER_FA_RESP_ARRIVED, RLITE_KER_FA_RESP, RLITE_KER_FA_REQ_ARRIVED,
                  RLITE_KER_IPCP_CONFIG, RLITE_KER_IPCP_PDUFT_SET, RLITE_KER_IPCP_PDUFT_DEL,
                  RLITE_KER_IPCP_PDUFT_FLUSH, RLITE_KER_IPCP_UIPCP_SET,
                  RLITE_KER_UIPCP_FA_REQ_ARRIVED, RLITE_KER_UIPCP_FA_RESP_ARRIVED,
                  RLITE_KER_FLOW_DEALLOCATED, RLITE_KER_FLOW_DEALLOC, RLITE_KER_IPCP_UIPCP_WAIT,
                  RLITE_KER_FLOW_STATS_REQ, RLITE_KER_FLOW_STATS_RESP, RLITE_KER_FLOW_CFG_UPDATE,
                  RLITE_KER_IPCP_QOS_SUPPORTED, RLITE_KER_APPL_MOVE, RLITE_KER_MEMTRACK_DUMP,
                  RLITE_KER_REG_FETCH, RLITE_KER_REG_FETCH_RESP, RLITE_KER_FLOW_STATE,
                  RLITE_KER_IPCP_STATS_REQ, RLITE_KER_IPCP_STATS_RESP, RLITE_KER_IPCP_CONFIG_GET_REQ,
                  RLITE_KER_IPCP_CONFIG_GET_RESP, RLITE_KER_IPCP_SCHED_WRR, RLITE_KER_IPCP_SCHED_PFIFO,
                  RLITE_KER_MSG_MAX);

   --  Create our needed Ker_Numtables array
   Rl_Ker_Numtables : array (RLITE_KER range <>) of Rl_Msg_Layout := (
      RLITE_KER_IPCP_CREATE =>
      ( 
         Copylen => Kernel_Msg.Rl_Kmsg_Ipcp_Create'Size - 3 * System.Address'Size,
         Names => 0,
         Strings => 3,
         Buffers => 0,
         Arrays => 0
      ),

      RLITE_KER_IPCP_CREATE_RESP =>
      (
         Copylen => Kernel_Msg.Rl_Kmsg_Ipcp_Create_Resp'Size,
         Names => 0,
         Strings => 0,
         Buffers => 0,
         Arrays => 0
      ),

      RLITE_KER_IPCP_DESTROY =>
      (
         Copylen => Kernel_Msg.Rl_Kmsg_Ipcp_Destroy'Size,
         Names   => 0,
         Strings => 0,
         Buffers => 0,
         Arrays  => 0
      ),

      RLITE_KER_FLOW_FETCH =>
      (
         Copylen => Kernel_Msg.Rl_Kmsg_Flow_Fetch'Size,
         Names   => 0,
         Strings => 0,
         Buffers => 0,
         Arrays  => 0
      ),

      RLITE_KER_FLOW_FETCH_RESP =>
      (
         Copylen => Kernel_Msg.Rl_Kmsg_Flow_Fetch_Resp'Size,
         Names   => 0,
         Strings => 0,
         Buffers => 0,
         Arrays  => 0
      ),

      RLITE_KER_IPCP_UPDATE =>
      (
         Copylen => Kernel_Msg.Rl_Kmsg_Ipcp_Update'Size - 3 * System.Address'Size,
         Names   => 0,
         Strings => 3,
         Buffers => 0,
         Arrays  => 0
      ),

      RLITE_KER_APPL_REGISTER =>
      (
         Copylen => Kernel_Msg.Rl_Kmsg_Appl_Register'Size - 2 * System.Address'Size,
         Names   => 0,
         Strings => 2,
         Buffers => 0,
         Arrays  => 0
      ),

      RLITE_KER_APPL_REGISTER_RESP =>
      (
         Copylen => Kernel_Msg.Rl_Kmsg_Appl_Register_Resp'Size - System.Address'Size,
         Names   => 0,
         Strings => 1,
         Buffers => 0,
         Arrays  => 0
      ),

      RLITE_KER_FA_REQ =>
      (
         Copylen => Kernel_Msg.Rl_Kmsg_Fa_Req'Size - 3 * System.Address'Size,
         Names   => 0,
         Strings => 3,
         Buffers => 0,
         Arrays  => 0
      ),

      RLITE_KER_FA_RESP_ARRIVED =>
      (
         Copylen => Kernel_Msg.Rl_Kmsg_Fa_Resp_Arrived'Size,
         Names   => 0,
         Strings => 0,
         Buffers => 0,
         Arrays  => 0
      )

      --  MT: TODO: Add rest of Kernel_Msg records so we can continue adding here
   );

end Bindings.Rlite.Utils;