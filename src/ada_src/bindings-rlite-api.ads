with Interfaces;

package Bindings.Rlite.API is

--  Record to specify the flow QoS parameters asked by
--  the application issuing a flow allocation request.
type RINA_Flow_Spec is record
    --  Version = 2 for rLite
    Version : Interfaces.Unsigned_32;
    --  Max delay in microseconds
    Max_Delay : Interfaces.Unsigned_32;
    --  Max in SDUs (Service Data Units)
    -- MT: TODO: This is originally a uint64_t, will this be an issue?
    Max_SDU_Gap : Interfaces.Unsigned_32;
    --  Average bandwith in bits per second
    Avg_Bandwith : Interfaces.Unsigned_32;
    --  Maximum packet loss from 0 (0%) to 10000 (100%)
    Max_Loss : Interfaces.Unsigned_16;
    --  In order delivery
    --  MT: TODO: Make this into a boolean?
    In_Order_Delivery : Interfaces.Unsigned_8;
    --  Message boundaries
    --  MT: TODO: Make this into a boolean?
    Msg_Boundaries : Interfaces.Unsigned_8;
    --  Max jitter in microseconds
    Max_Jitter : Interfaces.Unsigned_32;
end record;

end Bindings.Rlite.API;