with Debug;
with Net.Headers;

package body Net.Protos.EFCP is

   procedure Receive
     (Ifnet  : in out Net.Interfaces.Ifnet_Type'Class;
      Packet : in out Net.Buffers.Buffer_Type)
   is
      Buf :
        Byte_Buffer
          (1 .. Integer (Packet.Get_Data_Size (Net.Buffers.EFCP_PACKET))) with
        Address =>
         Packet.Get_Data_Address
           (Net.Buffers.Offsets (Net.Buffers.EFCP_PACKET));

      Message : CDAPMessage;
   begin

      Debug.Print
        (Debug.Info,
         "Received EFCP packet of" & Packet.Get_Length'Image & " Bytes");

      Debug.Print (Debug.Info, Buffer_To_Byte_String (Buf));
      Message.To_CDAP (Byte_Buffer_To_Vector (Buf));

      Message.Put;

      --  Incoming connection request
      if Message.OpCode = M_CONNECT then
         Send (Ifnet, Message.Encode ((Abs_Syntax, OpCode, Invoke_Id, Flags, Obj_Class, Obj_Name)));
         Debug.Print (Debug.Info, "Connect Request");
      end if;
   end Receive;


   procedure Send (Ifnet : in out Net.Interfaces.Ifnet_Type'Class; CDAP_Buffer : in Byte_Buffer) is
      Buf : Net.Buffers.Buffer_Type;
      Req : Net.Headers.EFCP_Packet_Access := new Net.Headers.EFCP_Packet;
   begin
      Net.Buffers.Allocate(Buf);
      
      if Buf.Is_Null then
         Debug.Print(Debug.Error, "Error allocating EFCP packet");
         return;
      end if;
   
      Req := Buf.EFCP;
      Req.Ethernet.Ether_Dhost := (16#ff#, 16#ff#, 16#ff#, 16#ff#, 16#ff#, 16#ff#);
      Req.Ethernet.Ether_Shost := Ifnet.Mac;
      Req.Ethernet.Ether_Type  := Net.Headers.To_Network (16#D1F0#);
      Req.EFCP.PDU_Version := 0;
      Req.EFCP.PDU_Type := 64;
      Req.EFCP.PDU_Flags := 0;
      Req.EFCP.PDU_CSum := 0;
      Req.EFCP.PDU_TTL := Net.Headers.To_Network (16#4000#);
      Req.EFCP.PDU_SeqNum := 0;
      Req.EFCP.Dst_Addr := 0;
      Req.EFCP.Src_Addr := Net.Headers.To_Network (Uint32 (1));
      Req.EFCP.Dst_Cep := 0;
      Req.EFCP.Src_Cep := 0;
      Req.EFCP.QOS_Id := 0;
      Req.CDAP := (others => 0);

      for I in CDAP_Buffer'Range loop
         Req.CDAP(I) := CDAP_Buffer(I);
      end loop;
      
      Req.EFCP.PDU_Len := ((Req.EFCP'Size + CDAP_Buffer'Size) / 8);

      Buf.Set_Length ((Req.Ethernet'Size + Req.EFCP'Size + CDAP_Buffer'Size) / 8);
      Ifnet.Send (Buf);
   end Send;

end Net.Protos.EFCP;
