package body Builder is
    
    function Serialize (Self : in Picture_Message) return Byte_Buffer is
      Magic_Number_Ptr : constant Byte_Buffer(1 .. Self.Magic_Number'Size / 8)
                        with Address => Self.Magic_Number'Address, Import, Volatile;

      File_Type_Ptr  : constant Byte_Buffer(1 .. 1) := (others => Byte (File_Types'Pos (Self.File_Type)));

      Frame_Num_Ptr   : constant Byte_Buffer(1 .. Self.Frame_Number'Size / 8)
                        with Address => Self.Frame_Number'Address, Import, Volatile;

      Frame_Total_Ptr : constant Byte_Buffer(1 .. Self.Frame_Total'Size / 8)
                        with Address => Self.Frame_Total'Address, Import, Volatile;

      Payload_Size_Ptr : constant Byte_Buffer(1 .. Self.Payload_Size'Size / 8)
                        with Address => Self.Payload_Size'Address, Import, Volatile;

      Identifier : constant String := To_String (Self.Identifier);
      Identifier_Length : constant Uint16 := Uint16 (Length (Self.Identifier));

      Identifier_Size_Ptr   : constant Byte_Buffer(1 .. Identifier_Length'Size / 8)
                        with Address => Identifier_Length'Address, Import, Volatile;

      Identifier_Ptr   : constant Byte_Buffer(1 .. Identifier'Size / 8)
                        with Address => Identifier'Address, Import, Volatile;

      Payload_Data_Ptr   : constant Byte_Buffer(1 .. Self.Payload_Data.all'Size / 8)
                        with Address => Self.Payload_Data.all'Address, Import, Volatile;

      --  Final serialized concatenated byte buffer in Rlite format
      Serialized_Msg : constant Byte_Buffer := Magic_Number_Ptr & File_Type_Ptr & Frame_Num_Ptr & Frame_Total_Ptr & Payload_Size_Ptr & Identifier_Size_Ptr & Identifier_Ptr & Payload_Data_Ptr;
   begin
      return Serialized_Msg;
   end Serialize;

end Builder;