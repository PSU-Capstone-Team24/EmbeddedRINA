--  Temp disabling
pragma Style_Checks (Off);

with Bindings.Rlite.Api; use Bindings.Rlite.Api;
with Exceptions;
with Debug;

package body Bindings.Rlite.Msg is
   
   function Read_Next_Msg
     (Rfd : OS.File_Descriptor) return Byte_Buffer
   is
      Timeout_Exception : exception;

   function OS_Read return Byte_Buffer is
      Bytes_Read : Integer;
      Buffer : Byte_Buffer(1 .. 4096) := (others => 0);
   begin

      -- Perform the blocking read operation
      Bytes_Read := OS.Read (Rfd, Buffer'Address, Buffer'Length);

      --  Make sure we've actually read something
      if Bytes_Read < 0 then
         Debug.Print
         ("Read_Next_Msg", "Error reading from file descriptor",
            Debug.Error);
         raise Exceptions.RINA_Control_Failure;
      end if;

      return Buffer;
   end OS_Read;

   task OS_Read_Task is
      entry Start;
      entry Get (Result : out Byte_Buffer);
   end OS_Read_Task;

   task body OS_Read_Task is
      Buffer : Byte_Buffer(1 .. 4096);
   begin
      accept Start;

      Buffer := OS_Read;
      
      select
         accept Get (Result : out Byte_Buffer) do
            Result := Buffer;
         end Get;
      or
         terminate;
      end select;
   end OS_Read_Task;

      Final_Result : Byte_Buffer(1 .. 4096) := (others => 0);
   begin
      OS_Read_Task.Start;

      select
         OS_Read_Task.Get (Final_Result);
         return Final_Result;
      or
         delay 0.5;

         --  MT: TODO: For some reason this still blocks, preventing execution
         --  This needs to be investigated further.
         Debug.Print("Read_Next_Msg", "OS.Read Timeout!", Debug.Error);
         raise Timeout_Exception;
      end select;
   end Read_Next_Msg;

end Bindings.Rlite.Msg;
