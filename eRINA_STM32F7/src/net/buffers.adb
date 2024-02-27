package body Buffers is

   function Buffer_To_String (Buffer : Byte_Buffer) return String is
      --  +1 to account for ASCII.NUL
      Result : String (1 .. Buffer'Length) := (others => ASCII.NUL);
   begin
      for I in Buffer'Range loop
         Result (I - Buffer'First + 1) := Character'Val (Buffer (I));

         --  We've hit a null terminator, stop reading early
         exit when Character'Val (Buffer (I)) = ASCII.NUL;
      end loop;

      --  Append null terminator
      --  Result (Result'Last) := ASCII.NUL;

      return Result;
   end Buffer_To_String;

   function Byte_Vector_To_Buffer (Vector : Byte_Vector) return Byte_Buffer is
      Buffer : Byte_Buffer (1 .. Natural (Vector.Length)) := (others => 0);
   begin
      for I in Buffer'Range loop
         Buffer (I) := Vector.Element (I);
      end loop;

      return Buffer;
   end Byte_Vector_To_Buffer;

   function Buffer_Reverse (Buffer : in Byte_Buffer) return Byte_Buffer is
      Return_Buffer : Byte_Buffer (Buffer'First .. Buffer'Last) :=
        (others => 0);
   begin
      for I in reverse Buffer'First .. Buffer'Last loop
         Return_Buffer (I) := Buffer (Buffer'Last - I + Buffer'First);
      end loop;

      return Return_Buffer;
   end Buffer_Reverse;

end Buffers;
