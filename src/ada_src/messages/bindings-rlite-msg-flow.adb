--  Temp disabling
pragma Style_Checks (Off);

with Names;
  use Names;

package body Bindings.Rlite.Msg.Flow is

   function Serialize (Self : in Request) return Byte_Buffer is
      Hdr_Ptr        : constant Byte_Buffer(1 .. Self.Hdr'Size / 8)
                        with Address => Self.Hdr'Address, Import, Volatile;

      Flow_Spec      : constant Byte_Buffer(1 .. Self.Flow_Spec'Size / 8)
                        with Address => Self.Flow_Spec'Address, Import, Volatile;

      Upper_Ipcp_Id  : constant Byte_Buffer(1 .. Self.Upper_Ipcp_Id'Size / 8)
                        with Address => Self.Upper_Ipcp_Id'Address, Import, Volatile;

      Local_Port     : constant Byte_Buffer(1 .. Self.Local_Port'Size / 8)
                        with Address => Self.Local_Port'Address, Import, Volatile;
         
      Local_Cep      : constant Byte_Buffer(1 .. Self.Local_Cep'Size / 8)
                        with Address => Self.Local_Cep'Address, Import, Volatile;

      Uid            : constant Byte_Buffer(1 .. Self.Uid'Size / 8)
                        with Address => Self.Uid'Address, Import, Volatile;
                     
      Cookie         : constant Byte_Buffer(1 .. Self.Cookie'Size / 8)
                        with Address => Self.Cookie'Address, Import, Volatile;

      Local_Appl_Size : constant Unsigned_16 := Unsigned_16 (Used_Size (Self.Local_Appl));

      Local_Appl_Size_Ptr : constant Byte_Buffer(1 .. Local_Appl_Size'Size / 8)
                           with Address => Local_Appl_Size'Address, Import, Volatile;

      Local_Appl_Ptr   : constant Byte_Buffer := To_Packed_Buffer (Self.Local_Appl);

      Remote_Appl_Size : constant Unsigned_16 := Unsigned_16 (Used_Size (Self.Remote_Appl));

      Remote_Appl_Size_Ptr : constant Byte_Buffer(1 .. Remote_Appl_Size'Size / 8)
                           with Address => Remote_Appl_Size'Address, Import, Volatile;

      Remote_Appl_Ptr   : constant Byte_Buffer := To_Packed_Buffer (Self.Remote_Appl);

      DIF_Name_Size : constant Unsigned_16 := Unsigned_16 (Used_Size (Self.Dif_Name));

      DIF_Name_Size_Ptr : constant Byte_Buffer(1 .. DIF_Name_Size'Size / 8)
                           with Address => DIF_Name_Size'Address, Import, Volatile;

      DIF_Name_Ptr   : constant Byte_Buffer := To_Packed_Buffer (Self.Dif_Name);

      Serialized_Msg : constant Byte_Buffer := Hdr_Ptr & Flow_Spec & Upper_Ipcp_Id & Local_Port & Local_Cep & Uid & Cookie & Local_Appl_Size_Ptr & Local_Appl_Ptr & Remote_Appl_Size_Ptr & Remote_Appl_Ptr & DIF_Name_Size_Ptr & DIF_Name_Ptr;
   begin
      return Serialized_Msg;
   end Serialize;

   function Serialize (Self : Response_Arrived) return Byte_Buffer is
      Buf : Byte_Buffer(0 .. 128) := (others => 0);
   begin
      return Buf;
   end Serialize;

   function Serialize (Self : Response) return Byte_Buffer is
      Buf : Byte_Buffer(0 .. 128) := (others => 0);
   begin
      return Buf;
   end Serialize;

   function Serialize (Self : Request_Arrived) return Byte_Buffer is
      Buf : Byte_Buffer(0 .. 128) := (others => 0);
   begin
      return Buf;
   end Serialize;

end Bindings.Rlite.Msg.Flow;