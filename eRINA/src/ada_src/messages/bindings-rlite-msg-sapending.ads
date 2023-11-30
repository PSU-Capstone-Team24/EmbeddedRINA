with Ada.Containers.Doubly_Linked_Lists;
with Bindings.Rlite.Msg;

package Bindings.Rlite.Msg.SaPending is

   type Sa_Pending_Item is record
      Handle : Integer;
      Req    : Rl_Msg_T := RLITE_KER_FA_REQ_ARRIVED;
   end record;

end Bindings.Rlite.Msg.SaPending;
