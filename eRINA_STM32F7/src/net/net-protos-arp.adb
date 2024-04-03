with Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with DIF_Manager;
with IPCP_Manager;
with Debug;
with Net.Utils;
with Net.Headers;
package body Net.Protos.Arp is

   use type Ada.Real_Time.Time;

   Broadcast_Mac : constant Ether_Addr := (others => 16#ff#);

   subtype Arp_Index is Uint8 range 1 .. 254;

   --  Maximum number of ARP entries we can remember.  We could increase to 253 but most
   --  application will only send packets to a small number of hosts.
   ARP_MAX_ENTRIES : constant Positive := 32;

   type Arp_Entry;
   type Arp_Entry_Access is access all Arp_Entry;

   type Arp_Entry is record
      Ether       : Ether_Addr;
      Index       : Arp_Index := Arp_Index'First;
      Name        : Unbounded_String;
   end record;

   type Arp_Entry_Table is array (1 .. ARP_MAX_ENTRIES) of aliased Arp_Entry;
   type Arp_Table is array (Arp_Index) of Arp_Entry_Access;

   protected Database is
      procedure Update;
   private
      Entries    : Arp_Entry_Table;
      Table      : Arp_Table := (others => null);
   end Database;

   protected body Database is
      procedure Update is
      begin
         null;
      end Update;
   end Database;

   use type Net.Headers.Length_Delimited_String;
   use type Net.Headers.Arp_Packet_Access;

   procedure Receive
     (Ifnet  : in out Net.Interfaces.Ifnet_Type'Class;
      Packet : in out Net.Buffers.Buffer_Type)
   is
      Req              : constant Net.Headers.Arp_Packet_Access := Packet.Arp;
      Pac              : Net.Buffers.Buffer_Type;
      Str_Fixed_Length : Uint8                                  := 0;
      Sender : Ether_Addr := (others => 0);
   begin
      begin         
         --  Do nothing if parse failed
         if Req = null then
            Debug.Print (Debug.Error, "Parse failed!");
            return;
         end if;

         --  Check for valid hardware (mac addr) length and protocol type.
         if Req.Arp.Ea_Hdr.Ar_Pro = Net.Headers.To_Network (ETHERTYPE_RINA) and
           Req.Arp.Ea_Hdr.Ar_Hln = Ifnet.Mac'Length
         then

            case Net.Headers.To_Host (Req.Arp.Ea_Hdr.Ar_Op) is
               when ARPOP_REQUEST =>
                  Debug.Print
                    (Debug.Info,
                     "RINA ARP Request Received " & Req.Arp.Arp_Spa.all &
                     " => " & Req.Arp.Arp_Tpa.all);

                  Debug.Print
                    (Debug.Info, "Searching for: " & Req.Arp.Arp_Tpa.all);
                  
                  --  Check if the requested IPCP exists in any of our local DIFs
                  if DIF_Manager.IPCP_Exists (Req.Arp.Arp_Tpa.all) or DIF_Manager.Application_Exists (Req.Arp.Arp_Tpa.all) then
                     Net.Buffers.Allocate (Pac);

                     if Pac.Is_Null then
                        Debug.Print (Debug.Error, "Buf is NULL");
                        return;
                     end if;

                     --  Packet.Set_Type (Net.Buffers.ARP_PACKET);
                     Pac.Set_Type (Net.Buffers.ETHER_PACKET);

                     Sender := Req.Ethernet.Ether_Shost;
                     Debug.Print(Debug.Info, "Request from: " & Net.Utils.To_String (Sender));

                     --  Where packet will be routed
                     Pac.Ethernet.Ether_Dhost := Sender;
                     Pac.Ethernet.Ether_Shost := Ifnet.Mac;
                     Pac.Ethernet.Ether_Type  :=
                       Net.Headers.To_Network (ETHERTYPE_ARP);

                     Pac.Put_Uint16 (ARPHRD_ETHER);
                     Pac.Put_Uint16 (Net.Protos.ETHERTYPE_RINA);
                     Pac.Put_Uint8 (Ifnet.Mac'Length);

                     Str_Fixed_Length :=
                       Uint8'Max
                         (Req.Arp.Arp_Tpa.all'Length,
                          Req.Arp.Arp_Spa.all'Length);
                     Pac.Put_Uint8 (Str_Fixed_Length);
                     Pac.Put_Uint16 (ARPOP_REPLY);

                     Pac.Put_Ether_Addr (Ifnet.Mac);
                     Pac.Put_String (Req.Arp.Arp_Tpa.all, Str_Fixed_Length);

                     Pac.Put_Ether_Addr (Req.Ethernet.Ether_Shost);
                     Pac.Put_String (Req.Arp.Arp_Spa.all, Str_Fixed_Length);

                     --  Send the corresponding ARP reply with our Ethernet address.
                     Pac.Set_Length
                       (Net.Buffers.Offsets (Net.Buffers.ETHER_PACKET) + 20 +
                        2 * Uint16 (Str_Fixed_Length));

                     Ifnet.Send (Pac);

                     Debug.Print
                       (Debug.Info,
                        "Matching local IPCP found! ARP response sent");
                  else
                     Debug.Print
                       (Debug.Warning,
                        "No matching local IPCP, ignoring ARP request");
                  end if;

               when ARPOP_REPLY =>
                  Debug.Print (Debug.Warning, "ARPOP_REPLY");
                  --if Req.Arp.Arp_Tpa = Ifnet.Ip and Req.Arp.Arp_Tha = Ifnet.Mac then
                  --   Update (Ifnet, Req.Arp.Arp_Spa, Req.Arp.Arp_Sha);
                  --end if;
               when others =>
                  Ifnet.Rx_Stats.Ignored := Ifnet.Rx_Stats.Ignored + 1;
            end case;
         else
            --  Ignore any future processing of this ARP message if it's not RINA-related
            Ifnet.Rx_Stats.Ignored := Ifnet.Rx_Stats.Ignored + 1;
            return;
         end if;
      end;
   end Receive;

end Net.Protos.Arp;
