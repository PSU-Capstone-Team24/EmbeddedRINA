with Debug;
with Net.Headers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DIF_Manager; use DIF_Manager;

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

      --Debug.Print (Debug.Info, Buffer_To_Byte_String (Buf));
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

      -- Incoming start request
      if Message.OpCode = M_START then
         if To_String (Message.Obj_Class) = "enrollment" then
            if To_String (Message.Obj_Name) = "/mgmt/enrollment" then
               
               --  M_START response
               Message.Abs_Syntax := 0;
               Message.OpCode := M_START_R;
               Message.Invoke_Id := 3;
               Message.Flags := F_NO_FLAGS;
               
               Message.Result := 0;
               Message.Version := 1;

               declare
                  Buf : constant Byte_Buffer := Byte_Vector_To_Buffer (Message.ObjValue.Byteval);
                  New_Msg : Enrollment_Info;
                  Str_Len : constant Integer := Integer (Buf(4));
                  DIF_Name : constant String := Buffer_To_String (Buf(5 .. 5 + Str_Len - 1));
               begin
                  if Exists (DIF_Name, Ethernet) then
                     Debug.Print (Debug.Info, "DIF '" & DIF_Name & "' Exists!");

                     New_Msg.Address := 3;
                     New_Msg.Lower_Difs := To_Unbounded_String (DIF_Name);

                     New_Msg.Dt_Constants.Max_PDU_Size := 65536;
                     New_Msg.Dt_Constants.Address_Width := 4;
                     New_Msg.Dt_Constants.Port_Id_Width := 2;
                     New_Msg.Dt_Constants.Cep_Id_Width := 2;
                     New_Msg.Dt_Constants.Qos_Id_Width := 2;
                     New_Msg.Dt_Constants.Seq_Num_Width := 4;
                     New_Msg.Dt_Constants.Length_Width := 2;
                     New_Msg.Dt_Constants.Seq_Rollover_Thresh := 18446744071562067968; --  -(2 ** 31);
                     New_Msg.Dt_Constants.Max_Pdu_Lifetime := 4000; --  ms
                     New_Msg.Dt_Constants.Concatenation_Enabled := False;
                     New_Msg.Dt_Constants.Fragmentation_Enabled := False;
                     New_Msg.Dt_Constants.Integrity_Enabled := False;
                     New_Msg.Dt_Constants.Max_Rtx_Time := 1000; --  ms
                     New_Msg.Dt_Constants.Max_Ack_Delay := 200; --  ms
                     New_Msg.Dt_Constants.Ctrl_Seq_Num_Width := 4;

                     Message.ObjValue.Set_Field (Byteval, Byte_Buffer_To_Vector (New_Msg.Encode ((Address, Lower_Difs, Dt_Constants))));

                     Send (Ifnet, Message.Encode
                           ((Abs_Syntax, OpCode, Invoke_Id, Flags,
                              Obj_Class, Obj_Name, ObjValue, Result, Version)));
                  end if;
               end;

            end if;
         end if;
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
