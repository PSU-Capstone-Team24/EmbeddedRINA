-----------------------------------------------------------------------
--  net-buffers -- Network buffers
--  Copyright (C) 2016, 2017, 2018 Stephane Carrez
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
with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;
with Net.Headers;             use Net.Headers;
with Buffers;                 use Buffers;
with Net.Utils;

package body Net.Buffers is

   function As_Ethernet is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Net.Headers.Ether_Header_Access);

   --  An unchecked conversion no longer works here since we have length-delimited strings
   --  i.e. we have no idea the length of the source and dest IPCP strings at compile time

   --  function As_Arp is new Ada.Unchecked_Conversion
   --    (Source => System.Address, Target => Net.Headers.Arp_Packet_Access);

   function As_Arp_Header is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Net.Headers.Arp_Header_Access);

   type Ether_Addr_Access is access all Ether_Addr;

   function As_Ether_Addr is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Ether_Addr_Access);

   function Address_To_Access_String
     (Address : System.Address; Length : Positive)
      return Net.Headers.Length_Delimited_String
   is
      subtype Byte is System.Storage_Elements.Storage_Element;
      type Byte_Array is array (1 .. Length) of Byte;

      Temp : Byte_Array;
      for Temp'Address use Address;

      Allocated_String : constant Net.Headers.Length_Delimited_String :=
        new String'(1 .. Length => ' ');
   begin
      for I in 1 .. Length loop
         Allocated_String (I) := Character'Val (Temp (I));
      end loop;

      return Allocated_String;
   end Address_To_Access_String;

   function As_Arp
     (Source : System.Address) return Net.Headers.Arp_Packet_Access
   is
      --  Arp packet to be returned
      Arp : constant Net.Headers.Arp_Packet_Access :=
        new Net.Headers.Arp_Packet;
      --  Ethernet header (this can be unchecked converted)
      Ether : constant Net.Headers.Ether_Header_Access := As_Ethernet (Source);
      --  Address starting where ARP packet begins (this can also be unchecked converted)
      Ea_Hdr : constant Net.Headers.Arp_Header_Access :=
        As_Arp_Header (Source + Storage_Offset (Offsets (ETHER_PACKET)));
      --  Source MAC addr
      Arp_Sha : constant Ether_Addr_Access :=
        As_Ether_Addr (Source + Storage_Offset (Offsets (ARP_PACKET)));
      --  Dest MAC addr
      Arp_Tha : constant Ether_Addr_Access :=
        As_Ether_Addr
          (Source +
           Storage_Offset (Offsets (ARP_PACKET) + Uint16 (Ea_Hdr.Ar_Pln)));
   begin
      Arp.Ethernet := Ether.all;

      --  I don't know why but the below doesn't work for me
      --  Arp.Arp.Ea_Hdr := Ea_Hdr.all;

      Arp.Arp.Ea_Hdr.Ar_Hdr := Ea_Hdr.Ar_Hdr;
      Arp.Arp.Ea_Hdr.Ar_Pro := Ea_Hdr.Ar_Pro;
      Arp.Arp.Ea_Hdr.Ar_Hln := Ea_Hdr.Ar_Hln;
      Arp.Arp.Ea_Hdr.Ar_Pln := Ea_Hdr.Ar_Pln;
      Arp.Arp.Ea_Hdr.Ar_Op  := Ea_Hdr.Ar_Op;

      Arp.Arp.Arp_Sha := Arp_Sha.all;
      Arp.Arp.Arp_Spa :=
        Address_To_Access_String
          (Source +
           Storage_Offset (Offsets (ARP_PACKET) + Uint16 (Ea_Hdr.Ar_Hln)),
           Positive (Arp.Arp.Ea_Hdr.Ar_Pln));

      Arp.Arp.Arp_Tha := Arp_Tha.all;
      Arp.Arp.Arp_Tpa :=
        Address_To_Access_String
          (Source +
           Storage_Offset
             (Offsets (ARP_PACKET) +
              Uint16 (Ea_Hdr.Ar_Hln + Ea_Hdr.Ar_Pln + Ea_Hdr.Ar_Hln)),
           Positive (Arp.Arp.Ea_Hdr.Ar_Pln));

      return Arp;
   end As_Arp;

   function As_EFCP is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Net.Headers.EFCP_Packet_Access);

   protected Manager with
     Priority => Net.Network_Priority
   is
      procedure Allocate (Packet : out Packet_Buffer_Access);

      procedure Release (Packet : in out Packet_Buffer_Access);

      procedure Add_Region (Addr : in System.Address; Count : in Uint32);

      procedure Release (List : in out Buffer_List);

      procedure Allocate (List : in out Buffer_List; Count : in Natural);

   private
      Free_List : Packet_Buffer_Access;
   end Manager;

   function Data_Type_To_String (Buffer : Data_Type; Size : Natural) return String is
      -- 3 characters per byte [XX ]
      Hex_String : String(1 .. Size * 3) := (others => ' ');
      Hex_Index : Natural := 1;
   begin
      for I in 0 .. Size loop
         if I < Natural(Buffer'Size / 8) then
            declare
               Hex : String := Net.Utils.Hex(Buffer(Interfaces.Unsigned_16(I)));
            begin
               Hex_String(Hex_Index .. Hex_Index + 1) := Hex;
               Hex_String(Hex_Index + 2) := ' ';
               Hex_Index := Hex_Index + 3;
            end;
         end if;
      end loop;

      return Hex_String;
   end Data_Type_To_String;

   --  ------------------------------
   --  Returns true if the buffer is null (allocation failed).
   --  ------------------------------
   function Is_Null (Buf : in Buffer_Type) return Boolean is
   begin
      return Buf.Packet = null;
   end Is_Null;

   --  ------------------------------
   --  Allocate a buffer from the pool.  No exception is raised if there is no available buffer.
   --  The <tt>Is_Null</tt> operation must be used to check the buffer allocation.
   --  ------------------------------
   procedure Allocate (Buf : out Buffer_Type) is
   begin
      Manager.Allocate (Buf.Packet);
      Buf.Size := 0;
   end Allocate;

   --  ------------------------------
   --  Release the buffer back to the pool.
   --  ------------------------------
   procedure Release (Buf : in out Buffer_Type) is
   begin
      if Buf.Packet /= null then
         Manager.Release (Buf.Packet);
      else
         raise Constraint_Error with "Release packet null";
      end if;
   end Release;

   --  ------------------------------
   --  Transfer the ownership of the buffer from <tt>From</tt> to <tt>To</tt>.
   --  If the destination has a buffer, it is first released.
   --  ------------------------------
   procedure Transfer (To : in out Buffer_Type; From : in out Buffer_Type) is
   begin
      if To.Packet /= null then
         Manager.Release (To.Packet);
      end if;
      To.Packet   := From.Packet;
      To.Size     := From.Size;
      From.Packet := null;
   end Transfer;

   --  ------------------------------
   --  Switch the ownership of the two buffers.  The typical usage is on the Ethernet receive
   --  ring to peek a received packet and install a new buffer on the ring so that there is
   --  always a buffer on the ring.
   --  ------------------------------
   procedure Switch (To : in out Buffer_Type; From : in out Buffer_Type) is
      Size   : constant Uint16               := To.Size;
      Packet : constant Packet_Buffer_Access := To.Packet;
   begin
      To.Size     := From.Size;
      To.Packet   := From.Packet;
      From.Size   := Size;
      From.Packet := Packet;
   end Switch;

   function Get_Data_Address (Buf : in Buffer_Type; Offset : in Uint16 := 0) return System.Address is
   begin
      return Buf.Packet.Data (Buf.Packet.Data'First + Offset)'Address;
   end Get_Data_Address;

   function Get_Data_Size
     (Buf : in Buffer_Type; Kind : in Packet_Type) return Uint16
   is
   begin
      if Buf.Size = 0 then
         return Buf.Pos - Offsets (Kind);
      else
         return Buf.Size - Offsets (Kind);
      end if;
   end Get_Data_Size;

   procedure Set_Data_Size (Buf : in out Buffer_Type; Size : in Uint16) is
   begin
      Buf.Pos  := Size + Offsets (Buf.Kind);
      Buf.Size := 0;
   end Set_Data_Size;

   function Get_Length (Buf : in Buffer_Type) return Uint16 is
   begin
      return Buf.Size;
   end Get_Length;

   procedure Set_Length (Buf : in out Buffer_Type; Size : in Uint16) is
   begin
      Buf.Size        := Size;
      Buf.Packet.Size := Size;
   end Set_Length;

   --  ------------------------------
   --  Set the packet type.
   --  ------------------------------
   procedure Set_Type (Buf : in out Buffer_Type; Kind : in Packet_Type) is
   begin
      Buf.Kind := Kind;
      Buf.Pos  := Offsets (Kind);
   end Set_Type;

   --  ------------------------------
   --  Add a byte to the buffer data, moving the buffer write position.
   --  ------------------------------
   procedure Put_Uint8 (Buf : in out Buffer_Type; Value : in Net.Uint8) is
   begin
      Buf.Packet.Data (Buf.Pos) := Value;
      Buf.Pos                   := Buf.Pos + 1;
   end Put_Uint8;

   --  ------------------------------
   --  Add a 16-bit value in network byte order to the buffer data,
   --  moving the buffer write position.
   --  ------------------------------
   procedure Put_Uint16 (Buf : in out Buffer_Type; Value : in Net.Uint16) is
   begin
      Buf.Packet.Data (Buf.Pos) :=
        Net.Uint8 (Interfaces.Shift_Right (Value, 8));
      Buf.Packet.Data (Buf.Pos + 1) := Net.Uint8 (Value and 16#0ff#);
      Buf.Pos                       := Buf.Pos + 2;
   end Put_Uint16;

   --  ------------------------------
   --  Add a 48-bit MAC address in network byte order to the buffer data,
   --  moving the buffer write position.
   --  ------------------------------
   procedure Put_Ether_Addr (Buf : in out Buffer_Type; Value : in Net.Ether_Addr) is
   begin
      Buf.Put_Uint8 (Value(1));
      Buf.Put_Uint8 (Value(2));
      Buf.Put_Uint8 (Value(3));
      Buf.Put_Uint8 (Value(4));
      Buf.Put_Uint8 (Value(5));
      Buf.Put_Uint8 (Value(6));
   end Put_Ether_Addr;

   --  ------------------------------
   --  Add a 32-bit value in network byte order to the buffer data,
   --  moving the buffer write position.
   --  ------------------------------
   procedure Put_Uint32 (Buf : in out Buffer_Type; Value : in Net.Uint32) is
   begin
      Buf.Packet.Data (Buf.Pos) :=
        Net.Uint8 (Interfaces.Shift_Right (Value, 24));
      Buf.Packet.Data (Buf.Pos + 1) :=
        Net.Uint8 (Interfaces.Shift_Right (Value, 16) and 16#0ff#);
      Buf.Packet.Data (Buf.Pos + 2) :=
        Net.Uint8 (Interfaces.Shift_Right (Value, 8) and 16#0ff#);
      Buf.Packet.Data (Buf.Pos + 3) := Net.Uint8 (Value and 16#0ff#);
      Buf.Pos                       := Buf.Pos + 4;
   end Put_Uint32;

   --  ------------------------------
   --  Add a string to the buffer data, moving the buffer write position.
   --  When <tt>With_Null</tt> is set, a NUL byte is added after the string.
   --  ------------------------------
   procedure Put_String
     (Buf        : in out Buffer_Type; Value : in String;
      Pad_Length : in     Uint8 := 0)
   is
      Pos : Uint16 := Buf.Pos;
   begin
      for C of Value loop
         Buf.Packet.Data (Pos) := Character'Pos (C);
         Pos                   := Pos + 1;
      end loop;
      if Pad_Length > 0 and Value'Length > Pad_Length then
         for I in 0 .. (Value'Length - Pad_Length) loop
            Buf.Packet.Data (Pos) := 0;
            Pos                   := Pos + 1;
         end loop;
      end if;
      Buf.Pos := Pos;
   end Put_String;

   function To_String (Buf : in Buffer_Type) return String is
   begin
      return Data_Type_To_String (Buf.Packet.Data, Natural (Buf.Get_Length));
   end To_String;

   --  ------------------------------
   --  Add an IP address to the buffer data, moving the buffer write position.
   --  ------------------------------
   procedure Put_Ip (Buf : in out Buffer_Type; Value : in Ip_Addr) is
      Pos : Uint16 := Buf.Pos;
   begin
      for C of Value loop
         Buf.Packet.Data (Pos) := C;
         Pos                   := Pos + 1;
      end loop;
      Buf.Pos := Pos;
   end Put_Ip;

   --  ------------------------------
   --  Get a byte from the buffer, moving the buffer read position.
   --  ------------------------------
   function Get_Uint8 (Buf : in out Buffer_Type) return Net.Uint8 is
      Pos : constant Net.Uint16 := Buf.Pos;
   begin
      Buf.Pos := Pos + 1;
      return Buf.Packet.Data (Pos);
   end Get_Uint8;

   --  ------------------------------
   --  Get a 16-bit value in network byte order from the buffer, moving the buffer read position.
   --  ------------------------------
   function Get_Uint16 (Buf : in out Buffer_Type) return Net.Uint16 is
      Pos : constant Net.Uint16 := Buf.Pos;
   begin
      Buf.Pos := Pos + 2;
      return
        Interfaces.Shift_Left (Net.Uint16 (Buf.Packet.Data (Pos)), 8) or
        Net.Uint16 (Buf.Packet.Data (Pos + 1));
   end Get_Uint16;

   --  ------------------------------
   --  Get a 32-bit value in network byte order from the buffer, moving the buffer read position.
   --  ------------------------------
   function Get_Uint32 (Buf : in out Buffer_Type) return Net.Uint32 is
      Pos : constant Net.Uint16 := Buf.Pos;
   begin
      Buf.Pos := Pos + 4;
      return
        Interfaces.Shift_Left (Net.Uint32 (Buf.Packet.Data (Pos)), 24) or
        Interfaces.Shift_Left (Net.Uint32 (Buf.Packet.Data (Pos + 1)), 16) or
        Interfaces.Shift_Left (Net.Uint32 (Buf.Packet.Data (Pos + 2)), 8) or
        Net.Uint32 (Buf.Packet.Data (Pos + 3));
   end Get_Uint32;

   --  ------------------------------
   --  Get an IPv4 value from the buffer, moving the buffer read position.
   --  ------------------------------
   function Get_Ip (Buf : in out Buffer_Type) return Net.Ip_Addr is
      Pos    : constant Net.Uint16 := Buf.Pos;
      Result : Ip_Addr;
   begin
      Buf.Pos    := Pos + 4;
      Result (1) := Buf.Packet.Data (Pos);
      Result (2) := Buf.Packet.Data (Pos + 1);
      Result (3) := Buf.Packet.Data (Pos + 2);
      Result (4) := Buf.Packet.Data (Pos + 3);
      return Result;
   end Get_Ip;

   --  ------------------------------
   --  Get a string whose length is specified by the target value.
   --  ------------------------------
   procedure Get_String (Buf : in out Buffer_Type; Into : out String) is
      Pos : Net.Uint16 := Buf.Pos;
   begin
      for I in Into'Range loop
         Into (I) := Character'Val (Buf.Packet.Data (Pos));
         Pos      := Pos + 1;
      end loop;
      Buf.Pos := Pos;
   end Get_String;

   --  ------------------------------
   --  Skip a number of bytes in the buffer, moving the buffer position <tt>Size<tt> bytes ahead.
   --  ------------------------------
   procedure Skip (Buf : in out Buffer_Type; Size : in Net.Uint16) is
   begin
      Buf.Pos := Buf.Pos + Size;
   end Skip;

   --  ------------------------------
   --  Get the number of bytes still available when reading the packet.
   --  ------------------------------
   function Available (Buf : in Buffer_Type) return Net.Uint16 is
   begin
      return Buf.Size - Buf.Pos;
   end Available;

   --  ------------------------------
   --  Get access to the Ethernet header.
   --  ------------------------------
   function Ethernet
     (Buf : in Buffer_Type) return Net.Headers.Ether_Header_Access
   is
   begin
      return As_Ethernet (Buf.Packet.Data (Buf.Packet.Data'First)'Address);
   end Ethernet;

   --  ------------------------------
   --  Get access to the ARP packet.
   --  ------------------------------
   function Arp (Buf : in Buffer_Type) return Net.Headers.Arp_Packet_Access is
   begin
      return As_Arp (Buf.Packet.Data (Buf.Packet.Data'First)'Address);
   end Arp;

   --  ------------------------------
   --  Get access to the EFCP packet.
   --  ------------------------------
   function EFCP (Buf : in Buffer_Type) return Net.Headers.EFCP_Packet_Access
   is
   begin
      return As_EFCP (Buf.Packet.Data (Buf.Packet.Data'First)'Address);
   end EFCP;

   --  ------------------------------
   --  Returns True if the list is empty.
   --  ------------------------------
   function Is_Empty (List : in Buffer_List) return Boolean is
   begin
      return List.Head = null;
   end Is_Empty;

   --  ------------------------------
   --  Insert the buffer to the list.
   --  ------------------------------
   procedure Insert (Into : in out Buffer_List; Buf : in out Buffer_Type) is
   begin
      if Into.Tail = null then
         Into.Tail       := Buf.Packet;
         Buf.Packet.Next := null;
      else
         Buf.Packet.Next := Into.Head;
      end if;
      Into.Head  := Buf.Packet;
      Buf.Packet := null;
   end Insert;

   --  ------------------------------
   --  Release all the buffers held by the list.
   --  ------------------------------
   procedure Release (List : in out Buffer_List) is
   begin
      Manager.Release (List);
   end Release;

   --  ------------------------------
   --  Allocate <tt>Count</tt> buffers and add them to the list.
   --  There is no guarantee that the required number of buffers will be allocated.
   --  ------------------------------
   procedure Allocate (List : in out Buffer_List; Count : in Natural) is
   begin
      Manager.Allocate (List, Count);
   end Allocate;

   --  ------------------------------
   --  Peek a buffer from the list.
   --  ------------------------------
   procedure Peek (From : in out Buffer_List; Buf : in out Buffer_Type) is
   begin
      Buf.Packet := From.Head;
      Buf.Size   := Buf.Packet.Size;
      From.Head  := From.Head.Next;
      if From.Head = null then
         From.Tail := null;
      end if;
   end Peek;

   --  ------------------------------
   --  Transfer the list of buffers held by <tt>From</tt> at end of the list held
   --  by <tt>To</tt>.  After the transfer, the <tt>From</tt> list is empty.
   --  The complexity is in O(1).
   --  ------------------------------
   procedure Transfer (To : in out Buffer_List; From : in out Buffer_List) is
   begin
      if To.Tail /= null then
         To.Tail.Next := From.Head;
         From.Head    := To.Head;
      else
         To.Tail := From.Tail;
         To.Head := From.Head;
      end if;
      From.Head := null;
      From.Tail := null;
   end Transfer;

   --  ------------------------------
   --  Add a memory region to the buffer pool.
   --  ------------------------------
   procedure Add_Region (Addr : in System.Address; Size : in Uint32) is
      Count : constant Uint32 := Size / NET_ALLOC_SIZE;
   begin
      Manager.Add_Region (Addr, Count);
   end Add_Region;

   protected body Manager is

      procedure Allocate (Packet : out Packet_Buffer_Access) is
      begin
         Packet := Free_List;
         if Packet /= null then
            Free_List   := Packet.Next;
            Packet.Size := 0;
         end if;
      end Allocate;

      procedure Allocate (List : in out Buffer_List; Count : in Natural) is
         Packet : Packet_Buffer_Access;
      begin
         for I in 1 .. Count loop
            exit when Free_List = null;
            Packet    := Free_List;
            Free_List := Packet.Next;
            if List.Tail = null then
               List.Tail := Packet;
            else
               Packet.Next := List.Head;
            end if;
            List.Head := Packet;
         end loop;
      end Allocate;

      procedure Release (Packet : in out Packet_Buffer_Access) is
      begin
         Packet.Next := Free_List;
         Free_List   := Packet;
         Packet      := null;
      end Release;

      procedure Release (List : in out Buffer_List) is
      begin
         List.Tail.Next := Free_List;
         Free_List      := List.Head;
         List.Head      := null;
         List.Tail      := null;
      end Release;

      procedure Add_Region (Addr : in System.Address; Count : in Uint32) is

         type Packet_Array is array (1 .. Count) of aliased Packet_Buffer;
         type Packet_Array_Access is access all Packet_Array;
         function As_Packet_List is new Ada.Unchecked_Conversion
           (Source => System.Address, Target => Packet_Array_Access);

         Packets : Packet_Array_Access := As_Packet_List (Addr);
      begin
         for I in 1 .. Count loop
            Packets (I).Next := Free_List;
            Free_List        := Packets (I)'Unchecked_Access;
         end loop;
      end Add_Region;

   end Manager;

end Net.Buffers;
