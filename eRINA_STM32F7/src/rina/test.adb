with CDAP;
with Buffers;     use Buffers.Byte_Vectors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   Message     : CDAP.CDAPMessage;
   Test_Packet : constant Buffers.Byte_Vector :=
     16#08# & 16#00# & 16#10# & 16#0e# & 16#18# & 16#03# & 16#20# & 16#00# &
     16#2a# & 16#0a# & 16#65# & 16#6e# & 16#72# & 16#6f# & 16#6c# & 16#6c# &
     16#6d# & 16#65# & 16#6e# & 16#74# & 16#32# & 16#10# & 16#2f# & 16#6d# &
     16#67# & 16#6d# & 16#74# & 16#2f# & 16#65# & 16#6e# & 16#72# & 16#6f# &
     16#6c# & 16#6c# & 16#6d# & 16#65# & 16#6e# & 16#74# & 16#42# & 16#0f# &
     16#32# & 16#0d# & 16#08# & 16#01# & 16#12# & 16#09# & 16#65# & 16#74# &
     16#68# & 16#41# & 16#42# & 16#2e# & 16#44# & 16#49# & 16#46# & 16#48# &
     16#00# & 16#e0# & 16#01# & 16#01#;

   Test_Packet_2 : constant Buffers.Byte_Vector :=
     16#08# & 16#00# & 16#10# & 16#0C# & 16#18# & 16#05# & 16#20# & 16#00# &
     16#2A# & 16#09# & 16#6E# & 16#61# & 16#63# & 16#6B# & 16#2D# & 16#77# &
     16#61# & 16#69# & 16#74# & 16#32# & 16#16# & 16#2F# & 16#6D# & 16#67# &
     16#6D# & 16#74# & 16#2F# & 16#61# & 16#64# & 16#64# & 16#72# & 16#61# &
     16#6C# & 16#6C# & 16#6F# & 16#63# & 16#2F# & 16#70# & 16#61# & 16#72# &
     16#61# & 16#6D# & 16#73# & 16#42# & 16#08# & 16#2A# & 16#06# & 16#34# &
     16#30# & 16#30# & 16#30# & 16#6D# & 16#73# & 16#48# & 16#00# & 16#E0# &
     16#01# & 16#01#;

begin
   Message := CDAP.To_CDAP (Test_Packet);
   Message.Put;
   
   New_Line;
   
   Message := CDAP.To_CDAP (Test_Packet_2);
   Message.Put;
end Test;