with Exceptions;

with Names;
  use Names;

with Ada.Characters.Handling;

package body Bindings.Rlite.Msg.IPCP is
   
   function Serialize (Self : in Create) return Byte_Buffer is
      Hdr_Ptr        : constant Byte_Buffer(1 .. Self.Hdr'Size / 8)
                        with Address => Self.Hdr'Address, Import, Volatile;

      IPCP_Name_Size : constant Unsigned_16 := Unsigned_16 (Used_Size (Self.Ipcp_Name));

      IPCP_Name_Size_Ptr : constant Byte_Buffer(1 .. IPCP_Name_Size'Size / 8)
                           with Address => IPCP_Name_Size'Address, Import, Volatile;

      IPCP_Name_Ptr   : constant Byte_Buffer := To_Packed_Buffer (Self.Ipcp_Name);

      DIF_Type : String := Ada.Characters.Handling.To_Lower (DIF_Types'Image (Self.DIF_Type));

      DIF_Type_Size : constant Unsigned_16 := Unsigned_16 (DIF_Type'Size / 8);

      DIF_Type_Size_Ptr : constant Byte_Buffer(1 .. DIF_Type_Size'Size / 8)
                           with Address => DIF_Type_Size'Address, Import, Volatile;

      DIF_Type_Ptr : constant Byte_Buffer(1 .. DIF_Type'Size / 8)
                           with Address => DIF_Type'Address, Import, Volatile;

      DIF_Name_Size : constant Unsigned_16 := Unsigned_16 (Used_Size (Self.DIF_Name));

      DIF_Name_Size_Ptr : constant Byte_Buffer(1 .. DIF_Name_Size'Size / 8)
                           with Address => DIF_Name_Size'Address, Import, Volatile;

      DIF_Name_Ptr   : constant Byte_Buffer := To_Packed_Buffer (Self.DIF_Name);

      Serialized_Msg : constant Byte_Buffer := Hdr_Ptr & IPCP_Name_Size_Ptr & IPCP_Name_Ptr & DIF_Type_Size_Ptr & DIF_Type_Ptr & DIF_Name_Size_Ptr & DIF_Name_Ptr;
   begin
      return Serialized_Msg;
   end Serialize;
   
   procedure Deserialize (Self : in out Create; fd : OS.File_Descriptor) is
   begin
      raise Exceptions.Not_Implemented_Exception;
   end Deserialize;

   function Serialize (Self : in Create_Response) return Byte_Buffer is
      t : constant Byte_Buffer(1 .. 128) := (others => 0);
   begin
      raise Exceptions.Not_Implemented_Exception;
      return t;
   end Serialize;
   
   procedure Deserialize (Self : in out Create_Response; Fd : OS.File_Descriptor) is
      Buffer   : constant Byte_Buffer := Read_Next_Msg (Fd);
      Msg_Data : constant Byte_Buffer := Buffer(Rl_Msg_Hdr'Size / 8 + 1 .. Buffer'Size / 8);
      Offset   : Integer := Msg_Data'First;
   begin
      --  Byte buffer must not include any tagged record parts. This assumes
      --  byte_buffer is coming from C struct read from FD and not Ada!
      Self.Hdr := Buffer_To_Rl_Msg_Hdr (Buffer (1 .. Rl_Msg_Hdr'Size / 8));

      --  We are processing the wrong message
      if Self.Hdr.Msg_Type /= RLITE_KER_IPCP_CREATE_RESP then
         return;
      end if;

      Self.Ipcp_Id := Rl_Ipcp_Id_T (Buffer_To_Unsigned_16(Msg_Data(Offset .. Offset + 1)));
   end Deserialize;

end Bindings.Rlite.Msg.IPCP;