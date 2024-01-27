--  Temp disabling
pragma Style_Checks (Off);

with Ada.Strings;
with Ada.Strings.Hash;
with Ada.Strings.Bounded;

with Ada.Containers;

with Buffers;
  use Buffers;

--  Max name length to be used for DIF_Name, Appl_Name, etc
package Names is
    package Name_String is new Ada.Strings.Bounded.Generic_Bounded_Length
        (Max => 128);
    use Name_String;

    --  Gets the length of the characters in the bounded string up to the null terminator
    function Used_Size(Input : Bounded_String) return Natural;

    --  Takes a Bounded String and returns the packed buffer of all characters up to null terminator
    function To_Packed_Buffer(Input : Bounded_String) return Byte_Buffer;

    -- Takes in a bounded_string and returns a unique hash code
    function Hash(Name : in Bounded_String) return Ada.Containers.Hash_Type;
end Names;