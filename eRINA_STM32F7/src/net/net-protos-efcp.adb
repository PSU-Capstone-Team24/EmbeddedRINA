with Debug;
with Net.Headers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

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
         Message.Abs_Syntax   := 73;
         Message.OpCode       := M_CONNECT_R;
         Message.Invoke_Id    := 2;
         Message.Flags        := F_NO_FLAGS;
         Message.Src_AP_Name  := To_Unbounded_String ("b.IPCP");
         Message.Dest_AP_Name := To_Unbounded_String ("a.IPCP");
         Message.Version      := 1;
         Send
           (Ifnet,
            Message.Encode
              ((Abs_Syntax, OpCode, Invoke_Id, Flags,
                Obj_Class, Obj_Name, Result, Dest_AE_Inst,
                Dest_AE_Name, Dest_Ap_Inst, Dest_AP_Name,
                Src_AE_Inst, Src_AE_Name, Src_AP_Inst,
                SRC_AP_Name, Version)));
         Debug.Print (Debug.Info, "Connect Request");
      end if;
   end Receive;

   procedure Send
     (Ifnet       : in out Net.Interfaces.Ifnet_Type'Class;
      CDAP_Buffer : in     Byte_Buffer)
   is
      Buf : Net.Buffers.Buffer_Type;
      Req : Net.Headers.EFCP_Packet_Access := new Net.Headers.EFCP_Packet;
   begin
      Net.Buffers.Allocate (Buf);

      if Buf.Is_Null then
         Debug.Print (Debug.Error, "Error allocating EFCP packet");
         return;
      end if;

      Req                      := Buf.EFCP;
      Req.Ethernet.Ether_Dhost :=
        (16#ff#, 16#ff#, 16#ff#, 16#ff#, 16#ff#, 16#ff#);
      Req.Ethernet.Ether_Shost := Ifnet.Mac;
      Req.Ethernet.Ether_Type  := Net.Headers.To_Network (16#D1F0#);
      Req.Efcp.PDU_Version     := 0;
      Req.Efcp.PDU_Type        := 64;
      Req.Efcp.PDU_Flags       := 0;
      Req.Efcp.PDU_CSum        := 0;
      Req.Efcp.PDU_TTL         := Net.Headers.To_Network (16#4000#);
      Req.Efcp.PDU_SeqNum      := 0;
      Req.Efcp.Dst_Addr        := 0;
      Req.Efcp.Src_Addr        := Net.Headers.To_Network (Uint32 (1));
      Req.Efcp.Dst_Cep         := 0;
      Req.Efcp.Src_Cep         := 0;
      Req.Efcp.QOS_Id          := 0;
      Req.Cdap                 := (others => 0);

      for I in CDAP_Buffer'Range loop
         Req.Cdap (I) := CDAP_Buffer (I);
      end loop;

      Req.Efcp.PDU_Len := ((Req.Efcp'Size + CDAP_Buffer'Size) / 8);

      Buf.Set_Length
        ((Req.Ethernet'Size + Req.Efcp'Size + CDAP_Buffer'Size) / 8);
      Ifnet.Send (Buf);
   end Send;

end Net.Protos.EFCP;
