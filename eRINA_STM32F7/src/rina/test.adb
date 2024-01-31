with CDAP;
with Protobuf; use Protobuf.Byte_Vectors;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
    Message         : CDAP.CDAPMessage;
    Test_Packet     : constant Protobuf.Byte_Vector := 16#08# & 16#49#;
begin
    Message := Protobuf.To_CDAP (Test_Packet);
    
    Put ("Message Abs_Syntax: ");
    Put_Line (Message.Abs_Syntax'Image);
end Test;