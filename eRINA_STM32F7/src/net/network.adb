with Receiver;
with STM32.SDRAM;
with HAL;

package body Network is

   procedure Initialize is
   begin
      --  STMicroelectronics OUI = 00 81 E1
      Ifnet.Mac := (0, 16#81#, 16#E1#, 5, 5, 1);

      --  Setup some receive buffers and initialize the Ethernet driver.
      Net.Buffers.Add_Region
        (STM32.SDRAM.Reserve (Amount => HAL.UInt32 (NET_BUFFER_SIZE)),
         NET_BUFFER_SIZE);

      --  Initialize interface
      Ifnet.Initialize;

      --  Start packet receiver
      Receiver.Start;
   end Initialize;

   function Get_ARP_Request_Count return Natural is
   begin
      return Receiver.ARP_Request_Count;
   end Get_ARP_Request_Count;

end Network;
