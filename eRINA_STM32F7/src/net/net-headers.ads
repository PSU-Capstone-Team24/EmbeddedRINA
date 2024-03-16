with Buffers; use Buffers;

package Net.Headers is

   pragma Preelaborate;

   --  Convert integers to network byte order.
   function To_Network (Val : in Uint32) return Uint32;
   function To_Network (Val : in Uint16) return Uint16;

   --  Convert integers to host byte order.
   function To_Host (Val : in Uint32) return Uint32;
   function To_Host (Val : in Uint16) return Uint16;

   --  Ethernet header as defined for 802.3 Ethernet packet.
   type Ether_Header is record
      Ether_Dhost : Ether_Addr :=
        (16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#);
      Ether_Shost : Ether_Addr :=
        (16#00#, 16#81#, 16#E1#, 16#05#, 16#05#, 16#01#);
      Ether_Type : Uint16;
   end record;
   type Ether_Header_Access is access all Ether_Header;

   type Arp_Header is record
      Ar_Hdr : Uint16;
      Ar_Pro : Uint16;
      Ar_Hln : Uint8;
      Ar_Pln : Uint8;
      Ar_Op  : Uint16;
   end record;

   type Arp_Header_Access is access Arp_Header;
   type Length_Delimited_String is access String;

   type Ether_Arp is record
      Ea_Hdr  : Arp_Header;
      Arp_Sha : Ether_Addr := (0, 16#81#, 16#E1#, 5, 5, 1);
      Arp_Spa : Length_Delimited_String;
      Arp_Tha : Ether_Addr := (others => 16#00#);
      Arp_Tpa : Length_Delimited_String;
   end record;
   type Ether_Arp_Access is access all Ether_Arp;

   --  ARP Ethernet packet
   type Arp_Packet is record
      Ethernet : Ether_Header;
      Arp      : Ether_Arp;
   end record;
   type Arp_Packet_Access is access all Arp_Packet;

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

   for EFCP_Header use record
      PDU_Version at  0 range 0 ..  7;
      PDU_Type    at  1 range 0 ..  7;
      PDU_Flags   at  2 range 0 .. 15;
      PDU_CSum    at  4 range 0 .. 15;
      PDU_TTL     at  6 range 0 .. 15;
      PDU_SeqNum  at  8 range 0 .. 31;
      Dst_Addr    at 12 range 0 .. 31;
      Src_Addr    at 16 range 0 .. 31;
      Dst_Cep     at 20 range 0 .. 15;
      Src_Cep     at 22 range 0 .. 15;
      PDU_Len     at 24 range 0 .. 15;
      QOS_Id      at 26 range 0 .. 15;
   end record;

   type EFCP_Header_Access is access all EFCP_Header;

   --  EFCP Ethernet packet
   type EFCP_Packet is record
      Ethernet : Ether_Header;
      Efcp     : EFCP_Header;
      Cdap     : Byte_Buffer (1 .. 2_048);
   end record;
   pragma Pack (EFCP_Packet);

   type EFCP_Packet_Access is access all EFCP_Packet;

end Net.Headers;
