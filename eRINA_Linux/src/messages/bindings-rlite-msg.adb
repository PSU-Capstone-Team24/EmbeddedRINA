--  Temp disabling
pragma Style_Checks (Off);

with GNAT.OS_Lib;        use GNAT.OS_Lib;
with Bindings.Rlite.API; use Bindings.Rlite.API;
with Exceptions;
with Debug;

package body Bindings.Rlite.Msg is

   EWOULDBLOCK : constant Integer := 11;

   function C_fcntl (fd : Uint32; cmd : Uint32; value : Uint32) return Uint32;
   pragma Import (C, C_fcntl, "fcntl");

   function Read_Next_Msg (Rfd : OS.File_Descriptor) return Byte_Buffer is
      Bytes_Read : Integer;
      Buffer     : Byte_Buffer (1 .. 4_096) := (others => 0);
      Flags      : Uint32 := C_fcntl (Uint32 (Rfd), 16#0003#, 0);
   begin
      if Rfd = Invalid_FD then
         raise Exceptions.RINA_Control_Failure;
      end if;

      Flags := C_fcntl (Uint32 (Rfd), 16#0004#, Flags or 8#00_4000#);

      for I in 1 .. 4 loop
         -- Perform the blocking read operation
         Bytes_Read := OS.Read (Rfd, Buffer'Address, Buffer'Length);

         --  Make sure we've actually read something
         if Bytes_Read < 0 then
            if Errno /= EWOULDBLOCK then
               Debug.Print
                 ("Read_Next_Msg", "Error reading from file descriptor",
                  Debug.Error);

               raise Exceptions.RINA_Control_Failure;
            end if;
         else
            exit;
         end if;

         --  Has failed to read bytes for 3 iterations
         if I = 4 then
            raise Exceptions.RINA_Control_Failure;
         end if;

         -- Delay a bit before trying again
         delay 0.25;
      end loop;

      return Buffer;
   end Read_Next_Msg;

end Bindings.Rlite.Msg;
