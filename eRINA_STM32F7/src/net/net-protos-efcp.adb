with Ada.Real_Time;
with DIF_Manager;
with Debug;
with Net.Headers;
with Net.Buffers;
with CDAP; use CDAP;
with Buffers; use Buffers;

package body Net.Protos.EFCP is

   use type Ada.Real_Time.Time;

   procedure Receive
     (Ifnet  : in out Net.Interfaces.Ifnet_Type'Class;
      Packet : in out Net.Buffers.Buffer_Type) is
      Req : constant Net.Headers.Efcp_Packet_Access := Packet.Efcp;
      
      Buf : Byte_Buffer (1 .. Integer(Packet.Get_Data_Size(Net.Buffers.EFCP_PACKET)))
         with Address => Packet.Get_Data_Address(Net.Buffers.Offsets(Net.Buffers.EFCP_PACKET));

      Msg : CDAPMessage;
   begin
   
      Debug.Print(Debug.Info, "Received EFCP packet of" & Packet.Get_Length'Image & " Bytes");

      Debug.Print(Debug.Info, Buffer_To_Byte_String (Buf));
      Msg := CDAP.To_CDAP (Byte_Buffer_To_Vector (Buf));
   end Receive;

end Net.Protos.EFCP;