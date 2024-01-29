-----------------------------------------------------------------------
--  receiver -- Ethernet Packet Receiver
--  Copyright (C) 2016, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
-----------------------------------------------------------------------

--  Modified for use in EmbeddedRINA PSU Capstone
--  TODO: Update this notice with changes so we are license compliant

with Ada.Real_Time;
with Ada.Synchronous_Task_Control;
with Net.Buffers;
with Net.Protos.Arp;
with Net.Protos.Dispatchers;
with Net.Headers;
with Demos;

package body Receiver is

   use type Net.Ip_Addr;
   use type Net.Uint8;
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

      Packet  : Net.Buffers.Buffer_Type;
      Ether   : Net.Headers.Ether_Header_Access;
      Now     : Ada.Real_Time.Time;
      Dt      : Us_Time;
      Total   : Net.Uint64 := 0;
      Count   : Net.Uint64 := 0;
   begin
      --  Wait until the Ethernet driver is ready.
      Ada.Synchronous_Task_Control.Suspend_Until_True (Ready);

      --  Loop receiving packets and dispatching them.
      Min_Receive_Time := Us_Time'Last;
      Max_Receive_Time := Us_Time'First;

      loop

         if Packet.Is_Null then
            Net.Buffers.Allocate (Packet);
         end if;

         if not Packet.Is_Null then
            Demos.Ifnet.Receive (Packet);

            --  Used to compute processing time
            Now := Ada.Real_Time.Clock;
            Ether := Packet.Ethernet;

            --  We will try to process all ARP packets
            --  However, we will later ignore any that do not have the rLite protocol type (0xd1f0)
            if Ether.Ether_Type = Net.Headers.To_Network (Net.Protos.ETHERTYPE_ARP) then
               Net.Protos.Arp.Receive (Demos.Ifnet, Packet);
            end if;
            
            --  For our case, we ignore IP packets
            --  if Ether.Ether_Type = Net.Headers.To_Network (Net.Protos.ETHERTYPE_IP) then
            --     Net.Protos.Dispatchers.Receive (Demos.Ifnet, Packet);
            --  end if;

            --  Compute the time taken to process the packet in microseconds.
            Dt := Us_Time ((Ada.Real_Time.Clock - Now) / ONE_US);

            --  Compute average, min and max values.
            Count := Count + 1;
            Total := Total + Net.Uint64 (Dt);
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
