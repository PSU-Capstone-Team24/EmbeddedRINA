pragma Style_Checks (Off);
with Ada.Containers.Doubly_Linked_Lists;
with Bindings.Rlite.Msg.SaPending; use Bindings.Rlite.Msg.SaPending;

package Bindings.Rlite.List is

    --https://stackoverflow.com/questions/26682057/link-list-in-ada
    type List_Head;
    type List_Head_Ptr is access List_Head;
    type List_Head is record
        prev : List_Head_Ptr;
        succ : List_Head_Ptr;
    end record;

end Bindings.Rlite.List;

