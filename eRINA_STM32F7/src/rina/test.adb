with CDAP;
with Protobuf;
with Buffers; use Buffers.Byte_Vectors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
    Message         : CDAP.CDAPMessage;
    Test_Packet     : constant Buffers.Byte_Vector := 16#08# & 16#00# & 16#10# & 16#0e# & 16#18# &
                                                       16#03# & 16#20# & 16#00# & 16#2a# & 16#0a# &
                                                       16#65# & 16#6e# & 16#72# & 16#6f# & 16#6c# &
                                                       16#6c# & 16#6d# & 16#65# & 16#6e# & 16#74# &
                                                       16#32# & 16#10# & 16#2f# & 16#6d# & 16#67# &
                                                       16#6d# & 16#74# & 16#2f# & 16#65# & 16#6e# &
                                                       16#72# & 16#6f# & 16#6c# & 16#6c# & 16#6d# &
                                                       16#65# & 16#6e# & 16#74#;
begin
    Message := Protobuf.To_CDAP (Test_Packet);
    
    Put ("Message Abs_Syntax: ");
    Put_Line (Message.Abs_Syntax'Image);
end Test;