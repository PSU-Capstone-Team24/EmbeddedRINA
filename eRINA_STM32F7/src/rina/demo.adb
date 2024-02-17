with GUI;
with Textures;
with Textures.PSU;
with Network;
with STM32;
with STM32.Board;
with Ada.Real_Time; use Ada.Real_Time;
with Debug;

procedure Demo is
   Period : constant Time_Span := Milliseconds (1 / GUI.Frame_Rate * 1_000);
   Next_Render : Time               := Clock;
begin
   GUI.Initialize;
   Network.Initialize;
   STM32.Board.Configure_User_Button_GPIO;
   STM32.Board.Initialize_LEDs;
   STM32.Board.All_LEDs_Off;

   --  Render loop keeps the board from immediately terminating
   loop
      Textures.Print (Textures.PSU.Bitmap, (5, 10));

      GUI.Print ("eRINA Debug", (80, 15));

      GUI.Print ("Status: ", (80, 45));
      GUI.Print ("Waiting for enrollment request", (145, 45));

      GUI.Print
        ("Ignored Packets: " & Network.Ifnet.Rx_Stats.Ignored'Image, (80, 30));

      Debug.Render;
      GUI.Update;

      --  Set activation time of next render in loop
      Next_Render := Next_Render + Period;

      delay until Next_Render;
   end loop;
end Demo;
