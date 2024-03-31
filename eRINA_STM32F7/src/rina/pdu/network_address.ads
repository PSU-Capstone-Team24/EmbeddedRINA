with Conversion; use Conversion;

generic
    type T is private;
package Network_Address is
    type IPCP_Network_Address is new T and Getter_Setter;
end Network_Address;
