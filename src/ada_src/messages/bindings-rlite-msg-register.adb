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
      
      Dif_Name_Size : Unsigned_16 := Unsigned_16 (Used_Size (Self.Dif_Name));

      Dif_Name_Size_Ptr : Byte_Buffer(1 .. Dif_Name_Size'Size / 8)
                           with Address => Dif_Name_Size'Address, Import, Volatile;

      Dif_Name_Ptr   : Byte_Buffer := To_Packed_Buffer (Self.Dif_Name);

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

end Bindings.Rlite.Msg.Register;