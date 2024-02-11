-----------------------------------------------------------------------
--  net-headers -- Network headers
--  Copyright (C) 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
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
      Ether_Dhost : Ether_Addr;
      Ether_Shost : Ether_Addr;
      Ether_Type  : Uint16;
   end record;
   type Ether_Header_Access is access all Ether_Header;

   type Arp_Header is record
      Ar_Hdr : Uint16;
      Ar_Pro : Uint16;
      Ar_Hln : Uint8;
      Ar_Pln : Uint8;
      Ar_Op  : Uint16;
   end record;

   type Ether_Arp (Appl_Length : Positive) is record
      Ea_Hdr  : Arp_Header;
      Arp_Sha : Ether_Addr;
      Arp_Spa : String (1 .. Appl_Length);
      Arp_Tha : Ether_Addr;
      Arp_Tpa : String (1 .. Appl_Length);
   end record;
   type Ether_Arp_Access is access all Ether_Arp;

   --  ARP Ethernet packet
   type Arp_Packet (Appl_Length : Positive) is record
      Ethernet : Net.Headers.Ether_Header;
      Arp      : Net.Headers.Ether_Arp (Appl_Length);
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

   type EFCP_Header_Access is access all EFCP_Header;

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

   type Ether_EFCP is record
      Ea_Hdr : EFCP_Header;
   end record;

   type Ether_EFCP_Access is access all Ether_EFCP;

   --  EFCP Ethernet packet
   type EFCP_Packet is record
      Ethernet : Net.Headers.Ether_Header;
      Arp      : Net.Headers.Ether_EFCP;
   end record;

   type EFCP_Packet_Access is access all EFCP_Packet;

end Net.Headers;
