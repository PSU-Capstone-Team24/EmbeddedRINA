with Net.Interfaces;
with Net.Buffers;
with Buffers; use Buffers;

package Net.Protos.EFCP is

   --  Management PDU
   PDU_T_MGMT : constant Byte := 16#40#;

   --  Data transfer PDU
   PDU_T_DT : constant Byte := 16#80#;

   --  Control PDU
   PDU_T_CTRL : constant Byte := 16#C0#;

   procedure Receive
     (Ifnet  : in out Net.Interfaces.Ifnet_Type'Class;
      Packet : in out Net.Buffers.Buffer_Type);

   procedure Send
     (Ifnet       : in out Net.Interfaces.Ifnet_Type'Class;
      CDAP_Buffer : in     Byte_Buffer);

end Net.Protos.EFCP;
