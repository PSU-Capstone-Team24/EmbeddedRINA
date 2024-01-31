--  Temp disabling
pragma Style_Checks (Off);

package body Names is

   function Used_Size (Input : Bounded_String) return Natural is
      Len : Natural := 0;
   begin
      for I in 1 .. Length (Input) loop
         exit when Element (Input, I) = ASCII.NUL;
         Len := Len + 1;
      end loop;

      return Len;
   end Used_Size;

   function To_Packed_Buffer (Input : Bounded_String) return Byte_Buffer is
      Size   : constant Positive       := Used_Size (Input);
      Buffer : Byte_Buffer (1 .. Size) := (others => 0);
   begin
      for I in Buffer'Range loop
         Buffer (I) := Character'Pos (Element (Input, I));
      end loop;

      return Buffer;
   end To_Packed_Buffer;

   function Hash (Name : in Bounded_String) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (To_String (Name));
   end Hash;
end Names;
