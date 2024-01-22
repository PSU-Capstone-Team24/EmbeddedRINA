with Exceptions;

package body Bindings.Rlite.Msg.IPCP is
   
   function Serialize (Self : in Create) return Byte_Buffer is
      t : constant Byte_Buffer(1 .. 128) := (others => 0);
   begin
      raise Exceptions.Not_Implemented_Exception;
      return t;
   end Serialize;
   
   procedure Deserialize (Self : in out Create; fd : OS.File_Descriptor) is
   begin
      raise Exceptions.Not_Implemented_Exception;
   end Deserialize;

end Bindings.Rlite.Msg.IPCP;