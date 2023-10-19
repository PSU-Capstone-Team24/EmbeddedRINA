--  Temp disabling
pragma Style_Checks (Off);

with System;
with Interfaces; use Interfaces;
with Bindings.Rlite.Kernel_Msg;

package Bindings.Rlite.Utils is

    --  Because I don't want to keep having to type Bindings.Rlite....
    package Kernel_Msg renames Bindings.Rlite.Kernel_Msg;

    type Rl_Msg_Layout is record
        copylen   : Unsigned_32;
        names     : Unsigned_32;
        strings   : Unsigned_32;
        buffers   : Unsigned_32;
        arrays    : Unsigned_32;
    end record;

   --  Kernel message type enums
   type RLITE_KER is (RLITE_DUMMY, RLITE_KER_IPCP_CREATE, RLITE_KER_IPCP_CREATE_RESP, RLITE_KER_IPCP_DESTROY,
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

   --  Create our needed Ker_Numtables array
   Rl_Ker_Numtables : array (RLITE_KER range <>) of Rl_Msg_Layout := (
      RLITE_DUMMY => (
         copylen => 0,
         names   => 0,
         strings => 0,
         buffers => 0,
         arrays  => 0
      ),
      RLITE_KER_IPCP_CREATE =>
      ( 
         copylen => Kernel_Msg.Rl_Kmsg_IPCP_Create'Size / 8 - 3 * System.Address'Size / 8,
         names => 0,
         strings => 3,
         buffers => 0,
         arrays => 0
      ),

      RLITE_KER_IPCP_CREATE_RESP =>
      (
         copylen => Kernel_Msg.Rl_Kmsg_Ipcp_Create_Resp'Size / 8,
         names => 0,
         strings => 0,
         buffers => 0,
         arrays => 0
      ),

      RLITE_KER_IPCP_DESTROY =>
      (
         copylen => Kernel_Msg.Rl_Kmsg_Ipcp_Destroy'Size / 8,
         names   => 0,
         strings => 0,
         buffers => 0,
         arrays  => 0
      ),

      RLITE_KER_FLOW_FETCH =>
      (
         copylen => Kernel_Msg.Rl_Kmsg_Flow_Fetch'Size / 8,
         names   => 0,
         strings => 0,
         buffers => 0,
         arrays  => 0
      ),

      RLITE_KER_FLOW_FETCH_RESP =>
      (
         copylen => Kernel_Msg.Rl_Kmsg_Flow_Fetch_Resp'Size / 8,
         names   => 0,
         strings => 0,
         buffers => 0,
         arrays  => 0
      ),

      RLITE_KER_IPCP_UPDATE =>
      (
         copylen => Kernel_Msg.Rl_Kmsg_Ipcp_Update'Size / 8 - 3 * System.Address'Size / 8,
         names   => 0,
         strings => 3,
         buffers => 0,
         arrays  => 0
      ),

      RLITE_KER_APPL_REGISTER =>
      (
         copylen => Kernel_Msg.Rl_Kmsg_Appl_Register'Size / 8 - 2 * System.Address'Size / 8,
         names   => 0,
         strings => 2,
         buffers => 0,
         arrays  => 0
      ),

      RLITE_KER_APPL_REGISTER_RESP =>
      (
         copylen => Kernel_Msg.Rl_Kmsg_Appl_Register_Resp'Size / 8 - System.Address'Size / 8,
         names   => 0,
         strings => 1,
         buffers => 0,
         arrays  => 0
      ),

      RLITE_KER_FA_REQ =>
      (
         copylen => Kernel_Msg.Rl_Kmsg_Fa_Req'Size / 8 - 3 * System.Address'Size / 8,
         names   => 0,
         strings => 3,
         buffers => 0,
         arrays  => 0
      ),

      RLITE_KER_FA_RESP_ARRIVED =>
      (
         copylen => Kernel_Msg.Rl_Kmsg_Fa_Resp_Arrived'Size / 8,
         names   => 0,
         strings => 0,
         buffers => 0,
         arrays  => 0
      ),
      
      RLITE_KER_FA_RESP =>
      (
         copylen => Kernel_Msg.RL_Kmsg_Fa_Resp'Size / 8,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_FA_REQ_ARRIVED =>
      (
         copylen => Kernel_Msg.Rl_Kmsg_Fa_Req_Arrived'Size / 8 - 3 * System.Address'Size / 8,
         names => 0,
         buffers => 0,
         strings => 3,
         arrays  => 0
      ),

      RLITE_KER_IPCP_CONFIG =>
      (
         copylen => Kernel_Msg.RL_Kmsg_Ipcp_Config'Size / 8 - 2 * System.Address'Size / 8,
         names => 0,
         buffers => 0,
         strings => 2,
         arrays  => 0
      ),

      RLITE_KER_IPCP_PDUFT_SET =>
      (
         copylen => Kernel_Msg.Rl_Kmsg_Ipcp_Pduft_Mod'Size / 8,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_IPCP_PDUFT_DEL =>
      (
         copylen => Kernel_Msg.Rl_Kmsg_Ipcp_Pduft_Mod'Size / 8,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_IPCP_PDUFT_FLUSH =>
      (
         copylen => Kernel_Msg.Rl_Kmsg_Ipcp_Pduft_Flush'Size / 8,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      --  MT: TODO: Add rest of Kernel_Msg records so we can continue adding here

      RLITE_KER_IPCP_UIPCP_SET =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_UIPCP_FA_REQ_ARRIVED =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_UIPCP_FA_RESP_ARRIVED =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_FLOW_DEALLOCATED =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_FLOW_DEALLOC =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_IPCP_UIPCP_WAIT =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_FLOW_STATS_REQ =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_FLOW_STATS_RESP =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_FLOW_CFG_UPDATE =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_IPCP_QOS_SUPPORTED =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_APPL_MOVE =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_MEMTRACK_DUMP =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_REG_FETCH =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_REG_FETCH_RESP =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_FLOW_STATE =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_IPCP_STATS_REQ =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_IPCP_STATS_RESP =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_IPCP_CONFIG_GET_REQ =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_IPCP_CONFIG_GET_RESP =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_IPCP_SCHED_WRR =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_IPCP_SCHED_PFIFO =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      ),

      RLITE_KER_MSG_MAX =>
      (
         copylen => 0,
         names => 0,
         buffers => 0,
         strings => 0,
         arrays  => 0
      )
   );

end Bindings.Rlite.Utils;