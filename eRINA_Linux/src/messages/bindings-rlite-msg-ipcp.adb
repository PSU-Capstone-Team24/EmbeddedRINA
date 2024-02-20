with Exceptions;

with Names; use Names;

package body Bindings.Rlite.Msg.IPCP is

   function DIF_To_String (Self : DIF_Types) return String is
   begin
      case Self is
         when Normal =>
            return "normal";
         when Ethernet =>
            return "shim-eth";
      end case;
   end DIF_To_String;

   function Serialize (Self : in Create) return Byte_Buffer is
      Hdr_Ptr : constant Byte_Buffer (1 .. Self.Hdr'Size / 8) with
        Address => Self.Hdr'Address, Import, Volatile;

      IPCP_Name_Size : constant Unsigned_16 :=
        Unsigned_16 (Used_Size (Self.Ipcp_Name));

      IPCP_Name_Size_Ptr :
        constant Byte_Buffer (1 .. IPCP_Name_Size'Size / 8) with
        Address => IPCP_Name_Size'Address, Import, Volatile;

      IPCP_Name_Ptr : constant Byte_Buffer :=
        To_Packed_Buffer (Self.Ipcp_Name);
      
      DIF_Type : constant String := DIF_To_String (Self.DIF_Type);

      DIF_Type_Size : constant Unsigned_16 := Unsigned_16 (DIF_Type'Size / 8);

      DIF_Type_Size_Ptr :
        constant Byte_Buffer (1 .. DIF_Type_Size'Size / 8) with
        Address => DIF_Type_Size'Address, Import, Volatile;

      DIF_Type_Ptr : constant Byte_Buffer (1 .. DIF_Type'Size / 8) with
        Address => DIF_Type'Address, Import, Volatile;

      DIF_Name_Size : constant Unsigned_16 :=
        Unsigned_16 (Used_Size (Self.DIF_Name));

      DIF_Name_Size_Ptr :
        constant Byte_Buffer (1 .. DIF_Name_Size'Size / 8) with
        Address => DIF_Name_Size'Address, Import, Volatile;

      DIF_Name_Ptr : constant Byte_Buffer := To_Packed_Buffer (Self.DIF_Name);

      Serialized_Msg : constant Byte_Buffer :=
        Hdr_Ptr & IPCP_Name_Size_Ptr & IPCP_Name_Ptr & DIF_Type_Size_Ptr &
        DIF_Type_Ptr & DIF_Name_Size_Ptr & DIF_Name_Ptr;
   begin
      return Serialized_Msg;
   end Serialize;

   function Serialize (Self : in Create_Response) return Byte_Buffer is
      t : constant Byte_Buffer (1 .. 128) := (others => 0);
   begin
      raise Exceptions.Not_Implemented_Exception;
      return t;
   end Serialize;

   overriding procedure Deserialize
     (Self : in out Create_Response; Fd : OS.File_Descriptor)
   is
      Buffer   : constant Byte_Buffer := Read_Next_Msg (Fd);
      Msg_Data : constant Byte_Buffer :=
        Buffer (Rl_Msg_Hdr'Size / 8 + 1 .. Buffer'Size / 8);
      Offset : constant Integer := Msg_Data'First;
   begin
      --  Byte buffer must not include any tagged record parts. This assumes
      --  byte_buffer is coming from C struct read from FD and not Ada!
      Self.Hdr := Buffer_To_Rl_Msg_Hdr (Buffer (1 .. Rl_Msg_Hdr'Size / 8));

      --  We are processing the wrong message
      if Self.Hdr.Msg_Type /= RLITE_KER_IPCP_CREATE_RESP then
         return;
      end if;

      Self.Ipcp_Id :=
        Rl_Ipcp_Id_T (Buffer_To_Unsigned_16 (Msg_Data (Offset .. Offset + 1)));
   end Deserialize;

   overriding function Serialize (Self : Destroy) return Byte_Buffer is
      Hdr_Ptr : constant Byte_Buffer (1 .. Self.Hdr'Size / 8) with
        Address => Self.Hdr'Address, Import, Volatile;

      IPCP_Id : constant Byte_Buffer (1 .. Self.Ipcp_Id'Size / 8) with
        Address => Self.Ipcp_Id'Address, Import, Volatile;

      Pad1 : constant Byte_Buffer (1 .. 6) := (others => 0);

      Serialized_Msg : constant Byte_Buffer := Hdr_Ptr & IPCP_Id & Pad1;
   begin
      return Serialized_Msg;
   end Serialize;


   overriding function Serialize (Self : in Config) return Byte_Buffer is
      Hdr_Ptr : constant Byte_Buffer (1 .. Self.Hdr'Size / 8) with
        Address => Self.Hdr'Address, Import, Volatile;
      
      IPCP_Id : constant Byte_Buffer (1 .. Self.Ipcp_Id'Size / 8) with
        Address => Self.Ipcp_Id'Address, Import, Volatile;
      
      Pad1 : constant Byte_Buffer (1 .. 3) := (others => 0);

      Name_Size : constant Unsigned_16 :=
        Unsigned_16 (Used_Size (Self.Name));

      Name_Size_Ptr :
        constant Byte_Buffer (1 .. Name_Size'Size / 8) with
        Address => Name_Size'Address, Import, Volatile;

      Name_Ptr : constant Byte_Buffer :=
        To_Packed_Buffer (Self.Name);

      Value_Size : constant Unsigned_16 :=
        Unsigned_16 (Used_Size (Self.Value));

      Value_Size_Ptr :
        constant Byte_Buffer (1 .. Value_Size'Size / 8) with
        Address => Value_Size'Address, Import, Volatile;

      Value_Ptr : constant Byte_Buffer :=
        To_Packed_Buffer (Self.Value);

      Serialized_Msg : constant Byte_Buffer := Hdr_Ptr & IPCP_Id & Pad1 & Name_Size_Ptr & Name_Ptr & Value_Size_Ptr & Value_Ptr;
   begin
     return Serialized_Msg;
   end Serialize;

end Bindings.Rlite.Msg.IPCP;
