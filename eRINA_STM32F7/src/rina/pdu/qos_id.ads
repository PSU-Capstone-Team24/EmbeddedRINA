with Conversion; use Conversion;

generic
    type T is private;
package Qos_ID is
    type QoS_ID is new T and Getter_Setter;
end Qos_ID;
