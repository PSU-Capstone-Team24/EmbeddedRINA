--  Temp disabling
pragma Style_Checks (Off);

with Debug;

with Names;
  use Names;

with Bindings.Rlite.Msg;
  use Bindings.Rlite.Msg;

package body Bindings.Rlite.Msg.Register is

   function Serialize (Self : in Request) return Byte_Buffer is

      --  This is starting to look... not so robust... :(
      Hdr_Ptr        : Byte_Buffer(1 .. Self.Hdr'Size / 8)
                        with Address => Self.Hdr'Address, Import, Volatile;

      Reg_Ptr        : Byte_Buffer(1 .. Self.Reg'Size / 8)
                        with Address => Self.Reg'Address, Import, Volatile;

      --  Rlite has some padding here
      Pad1           : Byte_Buffer(1 .. 7) := (others => 0);

      --  Rlite wants us to pass the size of names first
      Appl_Name_Size : Unsigned_16 := Unsigned_16 (Used_Size (Self.Appl_Name));
      
      Appl_Name_Size_Ptr : Byte_Buffer(1 .. Appl_Name_Size'Size / 8)
                           with Address => Appl_Name_Size'Address, Import, Volatile;

      Appl_Name_Ptr  : Byte_Buffer := To_Packed_Buffer (Self.Appl_Name);

      --  Rlite wants us to pass the size of names first
      Dif_Name_Size : Unsigned_16 := Unsigned_16 (Used_Size (Self.Dif_Name));

      Dif_Name_Size_Ptr : Byte_Buffer(1 .. Dif_Name_Size'Size / 8)
                           with Address => Dif_Name_Size'Address, Import, Volatile;

      Dif_Name_Ptr   : Byte_Buffer := To_Packed_Buffer (Self.Dif_Name);

      --  Final serialized concatenated byte buffer in Rlite format
      Serialized_Msg : Byte_Buffer := Hdr_Ptr & Reg_Ptr & Pad1 & Appl_Name_Size_Ptr & Appl_Name_Ptr & Dif_Name_Size_Ptr & Dif_Name_Ptr;
   begin
      Put_Bytes(Serialized_Msg);
      return Serialized_Msg;
   end Serialize;

   function Serialize (Self : in Response) return Byte_Buffer is
      t : Byte_Buffer(1 .. 128) := (others => 0);
   begin
      return t;
   end Serialize;

   function Deserialize (Buffer : in Byte_Buffer) return Response is
      resp : Response;
      Msg_Data : Byte_Buffer := Buffer(Rl_Msg_Hdr'Size / 8 .. Buffer'Size / 8);
   begin
      --  Byte buffer must not include any tagged record parts. This assumes
      --  byte_buffer is coming from C struct read from FD and not Ada!
      resp.Hdr := Buffer_To_Rl_Msg_Hdr (Buffer (1 .. Rl_Msg_Hdr'Size / 8));
      
      --  Malformed or received wrong msg_type/event_id, throw exception? idk maybe
      if resp.Hdr.Msg_Type /= RLITE_KER_APPL_REGISTER_RESP then
         Debug.Print ("Msg.Register.Response.Deserialize", "Deserialized wrong msg_type?", Debug.Warning);
      end if;

      if resp.Hdr.Event_Id /= RINA_REG_EVENT_ID then
         Debug.Print ("Msg.Register.Response.Deserialize", "Deserialized wrong event_id?", Debug.Warning);
      end if;

      --  [===== HDR =====][== IPCP_ID ==][= Reg =][= Resp =][===== Pad1 =====][~~~~~~ Appl_Name ~~~~~~]
      resp.Ipcp_Id := Rl_Ipcp_Id_T (Buffer_To_Unsigned_16 (Msg_Data (Msg_Data'First .. Msg_Data'First + 2)));    
      resp.Reg := Unsigned_8 (Msg_Data (Msg_Data'First + 2));
      resp.Response := Unsigned_8 (Msg_Data (Msg_Data'First + 4));
      resp.Pad1 := 0;

      declare
         --  Fix endianness
         Name_Buffer : Byte_Buffer := Buffer_Reverse (Msg_Data (Msg_Data'First + 8 .. Msg_Data'First + 9));
         Name_Length : Unsigned_16 := Buffer_To_Unsigned_16 (Name_Buffer);
         Name : String := Buffer_To_String (Msg_Data (Msg_Data'First + 11 .. Msg_Data'First + 11 + Integer (Name_Length)));
      begin
         resp.Appl_Name := To_Bounded_String (Name);
      end;
      
      return resp;
   end Deserialize;

   function Serialize (Self : in Move) return Byte_Buffer is
      t : Byte_Buffer(1 .. 128) := (others => 0);
   begin
      return t;
   end Serialize;

end Bindings.Rlite.Msg.Register;