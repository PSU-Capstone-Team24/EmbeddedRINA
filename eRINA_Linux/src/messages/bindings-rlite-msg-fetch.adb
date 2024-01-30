--  Temp disabling
pragma Style_Checks (Off);

with Names;
  use Names;

with Exceptions;

package body Bindings.Rlite.Msg.Fetch is

   function Serialize (Self : in Request) return Byte_Buffer is
      Hdr_Ptr        : constant Byte_Buffer(1 .. Self.Hdr'Size / 8)
                        with Address => Self.Hdr'Address, Import, Volatile;

      Ipcp_Id_Ptr    : constant Byte_Buffer(1 .. Self.Ipcp_Id'Size / 8)
                        with Address => Self.Ipcp_Id'Address, Import, Volatile;

      --  6 single byte padding characters (0x00)
      Pad1           : constant Byte_Buffer(1 .. 6) := (others => 0);

      --  Final serialized concatenated byte buffer in Rlite format
      Serialized_Msg : constant Byte_Buffer := Hdr_Ptr & Ipcp_Id_Ptr & Pad1;
   begin
      return Serialized_Msg;
   end Serialize;

   procedure Deserialize (Self : in out Request; fd : OS.File_Descriptor) is
   begin
      raise Exceptions.Not_Implemented_Exception;
   end Deserialize;

end Bindings.Rlite.Msg.Fetch;