--  Temp disabling
pragma Style_Checks (Off);

with Bindings.Rlite.Msg;
  use Bindings.Rlite.Msg;

package body Bindings.Rlite.Msg.Flow is

   --  MT: TODO
   function Serialize (Self : Request) return Byte_Buffer is
      Buf : Byte_Buffer(0 .. 128) := (others => 0);
   begin
      return Buf;
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