--  Temp disabling
pragma Style_Checks (Off);

with Ada.Text_IO;
with Interfaces; use Interfaces;

package Buffers is
    type Byte_Buffer is array (Natural range <>) of Unsigned_8;

    procedure Put_Bytes (Input : Byte_Buffer);
end Buffers;