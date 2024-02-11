with Interfaces;
with Net;
with Net.Buffers;
with Net.Interfaces;
with Net.Interfaces.STM32;

package Network is
   use type Interfaces.Unsigned_32;

   --  Reserve 256 network buffers.
   NET_BUFFER_SIZE : constant Net.Uint32 := Net.Buffers.NET_ALLOC_SIZE * 256;

   --  The Ethernet interface driver.
   Ifnet : aliased Net.Interfaces.STM32.STM32_Ifnet;

   procedure Initialize;
   function Get_ARP_Request_Count return Natural;
end Network;
