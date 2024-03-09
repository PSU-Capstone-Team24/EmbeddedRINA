with Net.Interfaces;
with Net.Buffers;
with CDAP;    use CDAP;
with Buffers; use Buffers;

package Net.Protos.EFCP is

   procedure Receive
     (Ifnet  : in out Net.Interfaces.Ifnet_Type'Class;
      Packet : in out Net.Buffers.Buffer_Type);

   procedure Send
     (Ifnet : in out Net.Interfaces.Ifnet_Type'Class; CDAP_Buffer : in Byte_Buffer);

end Net.Protos.EFCP;
