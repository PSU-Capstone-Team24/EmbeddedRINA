with Net.Utils;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

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

   function String_To_Byte_Vector (Input : Unbounded_String) return Byte_Vector
   is
      Str : constant String := To_String (Input);
      Vec : Byte_Vector;
   begin
      for I in Str'Range loop
         Vec.Append (Character'Pos (Str (I)));
      end loop;

      return Vec;
   end String_To_Byte_Vector;

   function Byte_Vector_To_Buffer (Vector : Byte_Vector) return Byte_Buffer is
      Buffer : Byte_Buffer (1 .. Natural (Vector.Length)) := (others => 0);
   begin
      for I in Buffer'Range loop
         Buffer (I) := Vector.Element (I);
      end loop;

      return Buffer;
   end Byte_Vector_To_Buffer;

   function Byte_Buffer_To_Vector (Buffer : in Byte_Buffer) return Byte_Vector
   is
      Vector : Byte_Vector;
   begin
      for E of Buffer loop
         Vector.Append (E);
      end loop;

      return Vector;
   end Byte_Buffer_To_Vector;

   function Buffer_To_Byte_String (Buffer : Byte_Buffer) return String is
      -- 3 characters per byte [XX ]
      Hex_String : String (1 .. Buffer'Length * 3) := (others => ' ');
      Hex_Index  : Natural                         := 1;
   begin
      for I in Buffer'First .. Buffer'Last loop
         declare
            Hex : constant String := Net.Utils.Hex (Unsigned_8 (Buffer (I)));
         begin
            Hex_String (Hex_Index .. Hex_Index + 1) := Hex;
            Hex_String (Hex_Index + 2)              := ' ';
            Hex_Index                               := Hex_Index + 3;
         end;
      end loop;

      return Hex_String;
   end Buffer_To_Byte_String;

   procedure Append_Byte_Buffer_To_Vector
     (Buffer : in Byte_Buffer; Vector : in out Byte_Vector)
   is
   begin
      for I in Buffer'Range loop
         Vector.Append (Buffer (I));
      end loop;
   end Append_Byte_Buffer_To_Vector;

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
