with Debug;
with Net.Headers;
with CDAP; use CDAP;
with Buffers; use Buffers;

package body Net.Protos.EFCP is

   procedure Receive
     (Ifnet  : in out Net.Interfaces.Ifnet_Type'Class;
      Packet : in out Net.Buffers.Buffer_Type) is
      
      Buf : Byte_Buffer (1 .. Integer(Packet.Get_Data_Size(Net.Buffers.EFCP_PACKET)))
         with Address => Packet.Get_Data_Address(Net.Buffers.Offsets(Net.Buffers.EFCP_PACKET));

      Message : CDAPMessage;
   begin
   
      Debug.Print(Debug.Info, "Received EFCP packet of" & Packet.Get_Length'Image & " Bytes");

      Debug.Print(Debug.Info, Buffer_To_Byte_String (Buf));
      Message.To_CDAP (Byte_Buffer_To_Vector (Buf));
      Message.Put;
   end Receive;

end Net.Protos.EFCP;