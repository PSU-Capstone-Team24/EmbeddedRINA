with Ada.Real_Time;
with Ada.Synchronous_Task_Control;
with Net.Buffers;
with Net.Protos.Arp;
with Net.Protos.Dispatchers;
with Net.Headers;
with Network;
with Debug;

package body Receiver is

   use type Net.Uint16;

   Ready  : Ada.Synchronous_Task_Control.Suspension_Object;
   ONE_US : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (1);

   --  ------------------------------
   --  Start the receiver loop.
   --  ------------------------------
   procedure Start is
   begin
      Ada.Synchronous_Task_Control.Set_True (Ready);
   end Start;

   task body Controller is
      use type Ada.Real_Time.Time;
      use type Ada.Real_Time.Time_Span;
      use type Net.Uint64;

      Packet : Net.Buffers.Buffer_Type;
      Ether  : Net.Headers.Ether_Header_Access;
      Now    : Ada.Real_Time.Time;
      Dt     : Us_Time;
      Total  : Net.Uint64 := 0;
      Count  : Net.Uint64 := 0;
   begin

      Debug.Print (Debug.Info, "Initializing Ethernet Controller...");

      --  Wait until the Ethernet driver is ready.
      Ada.Synchronous_Task_Control.Suspend_Until_True (Ready);

      Debug.Print (Debug.Info, "Ethernet Controller Initialized!");

      --  Loop receiving packets and dispatching them.
      Min_Receive_Time := Us_Time'Last;
      Max_Receive_Time := Us_Time'First;

      loop
         if Packet.Is_Null then
            Net.Buffers.Allocate (Packet);
         end if;

         if not Packet.Is_Null then
            Network.Ifnet.Receive (Packet);
            Now   := Ada.Real_Time.Clock;
            Ether := Packet.Ethernet;

            if Ether.Ether_Type =
              Net.Headers.To_Network (Net.Protos.ETHERTYPE_ARP)
            then
               Net.Protos.Arp.Receive (Network.Ifnet, Packet);
            elsif Ether.Ether_Type =
              Net.Headers.To_Network (Net.Protos.ETHERTYPE_RINA)
            then
               Net.Protos.Dispatchers.Receive (Network.Ifnet, Packet);
            end if;

            --  For our case, we ignore IP packets
            --  if Ether.Ether_Type = Net.Headers.To_Network (Net.Protos.ETHERTYPE_IP) then
            --     Net.Protos.Dispatchers.Receive (Demos.Ifnet, Packet);
            --  end if;

            --  Compute the time taken to process the packet in microseconds.
            Dt := Us_Time ((Ada.Real_Time.Clock - Now) / ONE_US);

            --  Compute average, min and max values.
            Count            := Count + 1;
            Total            := Total + Net.Uint64 (Dt);
            Avg_Receive_Time := Us_Time (Total / Count);

            if Dt < Min_Receive_Time then
               Min_Receive_Time := Dt;
            end if;

            if Dt > Max_Receive_Time then
               Max_Receive_Time := Dt;
            end if;
         else
            delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
         end if;
      end loop;
   end Controller;

end Receiver;
