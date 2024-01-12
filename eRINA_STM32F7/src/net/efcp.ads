-- For testing only, probably best to fork ada_enet and add there instead!

with System;
with Interfaces;

package EFCP is

   --  subtypes copied from ada_enet
   subtype Uint8 is Interfaces.Unsigned_8;
   subtype Uint16 is Interfaces.Unsigned_16;
   subtype Uint32 is Interfaces.Unsigned_32;
   subtype Uint64 is Interfaces.Unsigned_64;

   --  EFCP header as defined by GR-NGP-009.
   type EFCP_Header is record
      Version : Uint8;
      Daddr   : Uint16; --  This is the IPC process that hosts the endpoint (not an IP)
      Saddr   : Uint16;
      Qosid   : Uint32;
      Dcepid  : Uint32; --  Connection Endpoint Id field in EFCP PDUs
      Scepid  : Uint32;
      Pdutype : Uint8;
      Flags   : Uint8;
      Len     : Uint32;
      Seq     : Uint32;
   end record;

   type EFCP_Header_Access is access all EFCP_Header;

   --  Sizes consistent with
   --   https://github.com/rlite/rlite/blob/6ac276b11a36832d58a9674f100a705614947d34/user/uipcps/uipcp-gpb/BaseRIB.proto#L19
   for EFCP_Header use record
      Version  at 0 range 0 .. 7;
      Daddr    at 1 range 0 .. 15;
      Saddr    at 3 range 0 .. 15;
      Qosid    at 5 range 0 .. 39;
      Dcepid   at 10 range 0 .. 31;
      Scepid   at 14 range 0 .. 31;
      Pdutype  at 18 range 0 .. 7;
      Flags    at 19 range 0 .. 7;
      Len      at 20 range 0 .. 55;
      Seq      at 27 range 0 .. 47;
   end record;

end EFCP;