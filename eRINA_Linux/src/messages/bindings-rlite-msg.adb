--  Temp disabling
pragma Style_Checks (Off);

with Exceptions;
with Debug;

package body Bindings.Rlite.Msg is

   function Read_Next_Msg (fd : OS.File_Descriptor) return Byte_Buffer is
      Bytes_Read : Integer;
      Buffer     : Byte_Buffer (1 .. 4096) := (others => 0);
   begin
      --  Read 4096 bytes from our file descriptor
      Bytes_Read := OS.Read (fd, Buffer'Address, 4096);

      --  Make sure we've actually read something
      if Bytes_Read < 0 then
         Debug.Print
           ("Read_Next_Msg",
            "Error reading from file descriptor",
            Debug.Error);
         raise Exceptions.RINA_Control_Failure;
      end if;

      return Buffer;
   end Read_Next_Msg;

end Bindings.Rlite.Msg;
