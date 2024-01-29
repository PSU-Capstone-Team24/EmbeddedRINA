--  TODO: Temp only, we should try to only use a single type
--  of byte buffer (i.e. those provided by ada_enet)
with Interfaces;
  use Interfaces;

package Buffers is
   --  subtypes copied from ada_enet
   subtype Uint8 is Unsigned_8;
   subtype Uint16 is Unsigned_16;
   subtype Uint32 is Unsigned_32;
   subtype Uint64 is Unsigned_64;

    type Byte is new Uint8;
    type Byte_Buffer is array (Natural range <>) of Byte;
end Buffers;