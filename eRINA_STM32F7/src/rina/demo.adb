with GUI;
with Textures;
with Textures.PSU;
with Network;
with STM32;
with STM32.Board;

procedure Demo is
begin
   GUI.Initialize;
   Network.Initialize;
   STM32.Board.Configure_User_Button_GPIO;
   STM32.Board.Initialize_LEDs;
   STM32.Board.All_LEDs_Off;

   --  Keep board from immediately terminating
   loop
      Textures.Print (Textures.PSU.Bitmap, (5, 10));

      GUI.Print ("eRINA Debug", (80, 15));

      GUI.Print
        ("Ignored Packets: " & Network.Ifnet.Rx_Stats.Ignored'Image,
         (80, 30));

      delay Duration(1 / GUI.Frame_Rate);
   end loop;
end Demo;