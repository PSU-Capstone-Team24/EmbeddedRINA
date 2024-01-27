-- For testing only, probably best to fork ada_enet and add there instead!

with System;
with Interfaces;

package EFCP is

   --  subtypes copied from ada_enet
   subtype Uint8 is Interfaces.Unsigned_8;
   subtype Uint16 is Interfaces.Unsigned_16;
   subtype Uint32 is Interfaces.Unsigned_32;
   subtype Uint64 is Interfaces.Unsigned_64;

   --  PCI header to be used for transfer PDUs.
   --  The order of the fields is extremely important, because we only
   --  accept struct layouts where the compiler does not insert any
   --  padding.
   --
   --  struct rina_pci {
   --      uint8_t pdu_version;
   --      uint8_t pdu_type;
   --      uint16_t pdu_flags;
   --      uint16_t pdu_csum;
   --      uint16_t pdu_ttl;
   --      rl_seq_t seqnum;
   --      rl_addr_t dst_addr;
   --      rl_addr_t src_addr;
   --      rl_cepid_t dst_cep;
   --      rl_cepid_t src_cep;
   --      rl_pdulen_t pdu_len;
   --      rl_qosid_t qos_id;
   --  } __attribute__((__packed__));

   --  EFCP header as defined by rLite PDUs
   type EFCP_Header is record
      PDU_Version : Uint8;
      PDU_Type    : Uint8;
      PDU_Flags   : Uint16;
      PDU_CSum    : Uint16;
      PDU_TTL     : Uint16;
      PDU_SeqNum  : Uint32;
      Dst_Addr    : Uint32;
      Src_Addr    : Uint32;
      Dst_Cep     : Uint16;
      Src_Cep     : Uint16;
      PDU_Len     : Uint16;
      QOS_Id      : Uint16; 
   end record;

   type EFCP_Header_Access is access all EFCP_Header;

   for EFCP_Header use record
      PDU_Version    at 0 range 0 .. 7;
      PDU_Type       at 1 range 0 .. 7;
      PDU_Flags      at 2 range 0 .. 15;
      PDU_CSum       at 4 range 0 .. 15;
      PDU_TTL        at 6 range 0 .. 15;
      PDU_SeqNum     at 8 range 0 .. 31;
      Dst_Addr       at 12 range 0 .. 31;
      Src_Addr       at 16 range 0 .. 31;
      Dst_Cep        at 20 range 0 .. 15;
      Src_Cep        at 22 range 0 .. 15;
      PDU_Len        at 24 range 0 .. 15;
      QOS_Id         at 26 range 0 .. 15;
   end record;

end EFCP;