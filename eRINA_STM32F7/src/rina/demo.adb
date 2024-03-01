with GUI;
with Texture_PSU;
with Texture_Logo;
with Network;
with STM32;
with STM32.Board;
with DIF_Manager;   use DIF_Manager;
with IPCP_Manager;  use IPCP_Manager;
with Ada.Real_Time; use Ada.Real_Time;
with Demo_IPCP;
with HAL.Bitmap;
with Debug;

procedure Demo is
   Period : constant Time_Span := Milliseconds (1 / GUI.Frame_Rate * 1_000);
   Next_Render : Time               := Clock;
   This_DIF    : DIF_Access;
   This_IPCP   : IPCP;
begin
   GUI.Initialize;
   Network.Initialize;
   STM32.Board.Configure_User_Button_GPIO;
   STM32.Board.Initialize_LEDs;
   STM32.Board.All_LEDs_Off;

   --  Create a new DIF
   This_DIF := DIF_Manager.Create ("ethAB.DIF", Normal);
   DIF_Manager.DIF_List.Append (This_DIF);

   --  Create the IPC process we want to enroll into the DIF
   This_IPCP := IPCP_Manager.Create ("b.IPCP", Demo_IPCP'Access);

   --  Enroll this IPCP into the DIF
   This_DIF.Enroll (This_IPCP);

   --  Render loop keeps the board from immediately terminating
   loop
      GUI.Draw_Rectangle ((0, 0), GUI.Board_Resolution, HAL.Bitmap.White);

      --  This is really ugly, but I'm not good enough with generics to figure out how to make it look better
      --  Perhaps Texture should be a tagged record instead so we can use dot notation here
      Texture_PSU.PSU.Print (Texture_PSU.Bitmap, (5, 8));
      Texture_Logo.Logo.Print (Texture_Logo.Bitmap, (75, 0));

      GUI.Fill_Rounded_Rectangle ((75, 45), (128, 25), GUI.Button_Color, 2);
      GUI.Print ("Menu", (120, 53));

      GUI.Print_Large ("Console", (5, 90));
      GUI.Draw_Rounded_Rectangle
        ((5, 105), (GUI.Board_Resolution.Width - 8, 160), HAL.Bitmap.Black, 2,
         1);

      GUI.Draw_Rounded_Rectangle ((277, 2), (200, 60), HAL.Bitmap.Black, 2, 1);
      GUI.Print ("CPU U:            xx.xx%", (280, 12));
      GUI.Print ("RAM U:            xx.xx%", (280, 24));
      GUI.Print ("  Mac: 00:81:E1:05:05:01", (280, 36));
      GUI.Print ("Board:   STM32F746-DISCO", (280, 48));

      GUI.Draw_Rounded_Rectangle ((277, 65), (200, 20), HAL.Bitmap.Black, 2, 1);

      GUI.Fill_Rounded_Rectangle ((330, 70), (25, 10), GUI.Get_RX_Status_Color, 1);
      GUI.Print ("RX", (300, 71));

      GUI.Fill_Rounded_Rectangle ((420, 70), (25, 10), GUI.Get_TX_Status_Color, 1);
      GUI.Print ("TX", (390, 71));

      GUI.Print ("Version: " & GUI.Build_Verson, (362, 90));

      --  GUI.Print ("Status: ", (80, 45));
      --  GUI.Print ("Waiting for enrollment request", (145, 45));

      --  GUI.Print
      --    ("Ignored Packets: " & Network.Ifnet.Rx_Stats.Ignored'Image, (80, 30));

      Debug.Render;
      GUI.Update;

      --  Set activation time of next render in loop
      Next_Render := Next_Render + Period;

      delay until Next_Render;
   end loop;
end Demo;
