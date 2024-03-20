--  Temp disabling
pragma Style_Checks (Off);

with Names; use Names;

with Exceptions;

package body Bindings.Rlite.Msg.Register is

   procedure Deserialize (Self : in out Request; fd : OS.File_Descriptor) is
   begin
      raise Exceptions.Not_Implemented_Exception;
   end Deserialize;

   function Serialize (Self : in Request) return Byte_Buffer is
      --  This is starting to look... not so robust... :(
      Hdr_Ptr : constant Byte_Buffer (1 .. Self.Hdr'Size / 8) with
        Address => Self.Hdr'Address, Import, Volatile;

      Reg_Ptr : constant Byte_Buffer (1 .. Self.Reg'Size / 8) with
        Address => Self.Reg'Address, Import, Volatile;

      --  Rlite has some padding here
      Pad1 : constant Byte_Buffer (1 .. 7) := (others => 0);

      --  Rlite wants us to pass the size of names first
      Appl_Name_Size : constant Unsigned_16 :=
        Unsigned_16 (Used_Size (Self.Appl_Name));

      Appl_Name_Size_Ptr :
        constant Byte_Buffer (1 .. Appl_Name_Size'Size / 8) with
        Address => Appl_Name_Size'Address, Import, Volatile;

      Appl_Name_Ptr : constant Byte_Buffer :=
        To_Packed_Buffer (Self.Appl_Name);

      --  Rlite wants us to pass the size of names first
      Dif_Name_Size : constant Unsigned_16 :=
        Unsigned_16 (Used_Size (Self.Dif_Name));

      Dif_Name_Size_Ptr :
        constant Byte_Buffer (1 .. Dif_Name_Size'Size / 8) with
        Address => Dif_Name_Size'Address, Import, Volatile;

      Dif_Name_Ptr : constant Byte_Buffer := To_Packed_Buffer (Self.Dif_Name);

      --  Final serialized concatenated byte buffer in Rlite format
      Serialized_Msg : constant Byte_Buffer :=
        Hdr_Ptr & Reg_Ptr & Pad1 & Appl_Name_Size_Ptr & Appl_Name_Ptr &
        Dif_Name_Size_Ptr & Dif_Name_Ptr;
   begin
      return Serialized_Msg;
   end Serialize;

   procedure Deserialize (Self : in out Response; fd : OS.File_Descriptor) is
      Buffer   : constant Byte_Buffer := Read_Next_Msg (fd);
      Msg_Data : constant Byte_Buffer :=
        Buffer (Rl_Msg_Hdr'Size / 8 + 1 .. Buffer'Size / 8);
   begin
      --  Byte buffer must not include any tagged record parts. This assumes
      --  byte_buffer is coming from C struct read from FD and not Ada!
      Self.Hdr := Buffer_To_Rl_Msg_Hdr (Buffer (1 .. Rl_Msg_Hdr'Size / 8));

      --  We are processing the wrong message
      if Self.Hdr.Msg_Type /= RLITE_KER_APPL_REGISTER_RESP then
         return;
      end if;

      --  Oh man, this is super ugly
      --  [===== HDR =====][== IPCP_ID ==][= Reg =][= Resp =][===== Pad1 =====][= Appl_Name_Size =][======= Appl_Name =======]
      Self.Ipcp_Id :=
        Rl_Ipcp_Id_T
          (Buffer_To_Unsigned_16
             (Buffer_Reverse
                (Msg_Data (Msg_Data'First .. Msg_Data'First + 1))));

      --  No need to correct endianness on single byte fields
      Self.Reg      := Unsigned_8 (Msg_Data (Msg_Data'First + 2));
      Self.Response := Unsigned_8 (Msg_Data (Msg_Data'First + 3));

      --  Padding always blank, we don't care what's in here
      Self.Pad1 := 0;

      --  Appl_Name decoding
      declare
         --  Covert these bytes to a 16-bit u16
         Name_Length : constant Unsigned_16 :=
           Buffer_To_Unsigned_16
             (Msg_Data (Msg_Data'First + 8 .. Msg_Data'First + 9));

--  Now we know the length of the Appl_Name string, pull it out of the buffer
         Name : constant String :=
           Buffer_To_String
             (Msg_Data
                (Msg_Data'First + 10 ..
                     Msg_Data'First + 10 + Integer (Name_Length)));
      begin
      --  Convert pulled string into a bounded one for use the response object
         Self.Appl_Name := To_Bounded_String (Name);
      end;
   end Deserialize;

   function Serialize (Self : in Response) return Byte_Buffer is
      t : constant Byte_Buffer (1 .. 128) := (others => 0);
   begin
      raise Exceptions.Not_Implemented_Exception;
      return t;
   end Serialize;

   procedure Deserialize (Self : in out Move; fd : OS.File_Descriptor) is
   begin
      raise Exceptions.Not_Implemented_Exception;
   end Deserialize;

   function Serialize (Self : in Move) return Byte_Buffer is
      Hdr_Ptr : constant Byte_Buffer (1 .. Self.Hdr'Size / 8) with
        Address => Self.Hdr'Address, Import, Volatile;

      Ipcp_Id_Ptr : constant Byte_Buffer (1 .. Self.Ipcp_Id'Size / 8) with
        Address => Self.Ipcp_Id'Address, Import, Volatile;

      --  Rlite has 16 bits (2 bytes) of padding here
      Pad1 : constant Byte_Buffer (1 .. 2) := (others => 0);

      Fd_Ptr : constant Byte_Buffer (1 .. Self.Fd'Size / 8) with
        Address => Self.Fd'Address, Import, Volatile;

      --  Final serialized concatenated byte buffer in Rlite format
      Serialized_Msg : constant Byte_Buffer :=
        Hdr_Ptr & Ipcp_Id_Ptr & Pad1 & Fd_Ptr;
   begin
      return Serialized_Msg;
   end Serialize;

end Bindings.Rlite.Msg.Register;
