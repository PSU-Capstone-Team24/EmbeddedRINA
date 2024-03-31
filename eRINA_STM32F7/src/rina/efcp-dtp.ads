with Buffers; use Buffers;

package EFCP.DTP is

   -- IPCP address format
   -- for now its MAC address form
   type Network_Address is
     new Natural range 16#00_00_00_00_00_00# ..
         16#FF_FF_FF_FF_FF_FF#; -- 6 bytes
   subtype QoS_Size is Byte;
   type CEPID_Size is new Natural range 16#00_00# .. 16#FF_FF#; -- 2 bytes size
   subtype PDU_Length is Byte; -- 1 byte
   type User_Data_Size is new Natural range 0 .. (8 * 1_514 - 1);

   -- PDU format
   type PDU is record
      Version    : Byte            := 1;
      Dest_Addr  : Network_Address :=
        16#FF_FF_FF_FF_FF_FF#; -- broadcast all MAC addr by default
      Send_Addr  : Network_Address;
      QoS_ID     : QoS_Size        := 1; -- default is QoS "Cube" 1
      Dest_CEPID : CEPID_Size; -- ID of the destination endpoint EFCP instance
      Src_CEPID  : CEPID_Size; -- ID of the source endpoint EFCP instance
      PDU_Type   : Byte            := 1; -- the type of PDU sent
      Flags      : Byte;
      Length     : PDU_Length; -- number of total bytes in this PDU
      Seq_Num    : Byte            := 0;
      User_Data : User_Data_Size; -- 1514 bytes. 1514 + above 22 bytes = 1536 bytes total
      -- 1536 bytes results in total bits per frame divisible cleanly by 64
      -- 64 is most modern computer's native word size
   end record;

   -- PDU addressing
   -- identify duplicates

   -- identify lost packets

   -- identify parallel connections

   -- PDU sequencing
end EFCP.DTP;
