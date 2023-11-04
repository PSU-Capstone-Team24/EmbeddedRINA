--  Temp disabling
pragma Style_Checks (Off);

with Ada.Strings.Bounded;
with Buffers; use Buffers;

--  Max name length to be used for DIF_Name, Appl_Name, etc
package Names is
    package Name_String is new Ada.Strings.Bounded.Generic_Bounded_Length
        (Max => 128);
    use Name_String;

    --  Gets the length of the characters in the bounded string up to the null terminator
    function Used_Size(Input : Bounded_String) return Natural;

    --  Takes a Bounded String and returns the packed buffer of all characters up to null terminator
    function To_Packed_Buffer(Input : Bounded_String) return Byte_Buffer;
end Names;