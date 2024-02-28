with Net.Interfaces;
with Net.Buffers;

package Net.Protos.EFCP is

   procedure Receive
     (Ifnet  : in out Net.Interfaces.Ifnet_Type'Class;
      Packet : in out Net.Buffers.Buffer_Type);

   --  MT: TODO: Implement me!
   procedure Make_Ident is null;
   procedure Make_Header is null;
   procedure Send_Raw is null;
   procedure Send is null;

end Net.Protos.EFCP;
