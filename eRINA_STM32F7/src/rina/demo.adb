with GUI;
with Texture_PSU;
with Texture_Logo;
with Texture_Close;
with Network;
with Net.Utils;
with STM32;
with STM32.Board;
with DIF_Manager;           use DIF_Manager;
with IPCP_Manager;          use IPCP_Manager;
with Ada.Real_Time;         use Ada.Real_Time;
with Demo_IPCP;
with HAL.Bitmap;
with Debug;
with HAL.Touch_Panel;
with Files;                 use Files;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Demo is
   Period : constant Time_Span := Milliseconds (1 / GUI.Frame_Rate * 1_000);
   Next_Render        : Time               := Clock;
   This_DIF           : DIF_Access;
   This_IPCP          : IPCP;
   Show_Menu          : Boolean            := False;
   Show_Network_Stats : Boolean            := False;
   Show_Device_Info   : Boolean            := False;
   Show_File_Browser  : Boolean            := False;
   Draw_Image         : Boolean            := False;
begin
   GUI.Initialize;
   STM32.Board.Touch_Panel.Initialize;
   Network.Initialize;
   STM32.Board.Configure_User_Button_GPIO;
   STM32.Board.Initialize_LEDs;
   STM32.Board.All_LEDs_Off;

   --  Create a new DIF
   This_DIF := DIF_Manager.Create ("ethAB.DIF", Ethernet);

   DIF_Manager.DIF_List.Append (This_DIF);

   --  Create the IPC process we want to enroll into the DIF
   This_IPCP := IPCP_Manager.Create ("b.IPCP");

   --  Enroll this IPCP into the DIF
   This_DIF.Enroll (This_IPCP);

   --  Register test server application to this DIF
   This_DIF.Register ("TestServer", Demo_IPCP'Access);

   --  Render loop keeps the board from immediately terminating
   loop
      declare
         State : constant HAL.Touch_Panel.TP_State :=
           STM32.Board.Touch_Panel.Get_All_Touch_Points;

         TX_Ks : constant Natural :=
           Natural (Network.Ifnet.Tx_Stats.Bytes) / 1_024;
         TX_Ms : constant Natural := TX_Ks / 1_024;

         RX_Ks : constant Natural :=
           Natural (Network.Ifnet.Rx_Stats.Bytes) / 1_024;
         RX_Ms : constant Natural := RX_Ks / 1_024;
      begin

         GUI.Draw_Rectangle ((0, 0), GUI.Board_Resolution, HAL.Bitmap.White);

         if not Show_File_Browser then
            --  This is really ugly, but I'm not good enough with generics to figure out how to make it look better
            --  Perhaps Texture should be a tagged record instead so we can use something like Texture_PSU.Print ((5, 8)) instead
            Texture_PSU.PSU.Print (Texture_PSU.Bitmap, (5, 8));
            Texture_Logo.Logo.Print (Texture_Logo.Bitmap, (75, 0));

            -- if the menu button on screen is pressed, set the Show_Menu flag, if the menu button is pressed again, unset the flag
            if GUI.Has_Touch (State => State) then
               if GUI.Has_Touch_Within_Area
                   (State => State, P => (75, 45), S => (48, 25))
               then
                  Show_Menu := not Show_Menu;
               end if;
            end if;

            --  MT: TODO: Use GUI.Add_Button instead, and allow developer
            --  to provide access procedure as button press callback
            GUI.Fill_Rounded_Rectangle
              ((75, 75), (110, 25), GUI.Button_Color, 2);

            GUI.Print ("File Browser", (82, 83), GUI.Button_Color);

            --  If file browser button is pressed, enter the file browser page
            if GUI.Has_Touch (State => State) then
               if GUI.Has_Touch_Within_Area
                   (State => State, P => (75, 75), S => (110, 25))
               then
                  Show_File_Browser := not Show_File_Browser;
               end if;
            end if;

            --  When the user presses the menu button, show the submenu
            if Show_Menu then
               GUI.Fill_Rounded_Rectangle
                 ((75, 45), (48, 25), GUI.Button_Selected_Color, 2);

               --  Filled in white with a black border
               GUI.Fill_Rounded_Rectangle
                 ((125, 45), (150, 65), HAL.Bitmap.White, 2);
               GUI.Draw_Rounded_Rectangle
                 ((125, 45), (150, 65), HAL.Bitmap.Black, 2, 1);

               --  Network stats menu option
               GUI.Fill_Rounded_Rectangle
                 ((130, 50), (140, 25), GUI.Button_Color, 2);
               GUI.Print ("Network Stats", (150, 60), GUI.Button_Color);

               --  Device info menu option
               GUI.Fill_Rounded_Rectangle
                 ((130, 78), (140, 25), GUI.Button_Color, 2);
               GUI.Print ("Device Info", (155, 87), GUI.Button_Color);

               -- if there is a touch event while the menu is open
               if (GUI.Has_Touch (State => State)) then
               -- check if the touch event is within the area of the network stats button
                  if GUI.Has_Touch_Within_Area
                      (State => State, P => (130, 50), S => (140, 25))
                  then
                     Show_Network_Stats := not Show_Network_Stats;
                     Show_Menu          := False;
                  end if;

                  -- check if the touch event is within the area of the device info button
                  if GUI.Has_Touch_Within_Area
                      (State => State, P => (130, 78), S => (140, 25))
                  then
                     Show_Device_Info := not Show_Device_Info;
                     Show_Menu        := False;
                  end if;
               end if;
            else
               GUI.Fill_Rounded_Rectangle
                 ((75, 45), (48, 25), GUI.Button_Color, 2);
            end if;

            GUI.Print ("Menu", (82, 53), GUI.Button_Color);

            GUI.Print_Large ("Console", (5, 105));
            GUI.Draw_Rounded_Rectangle
              ((5, 120), (GUI.Board_Resolution.Width - 8, 145),
               HAL.Bitmap.Black, 2, 1);

            if Show_Device_Info then
               GUI.Draw_Rounded_Rectangle
                 ((277, 2), (200, 36), HAL.Bitmap.Black, 2, 1);
               GUI.Print
                 ("  Mac: " & Net.Utils.To_String (Network.Ifnet.Mac),
                  (280, 12));
               GUI.Print ("Board:   STM32F746-DISCO", (280, 24));
            end if;

            if Show_Network_Stats then
               GUI.Draw_Rounded_Rectangle
                 ((277, 40), (200, 55), HAL.Bitmap.Black, 2, 1);

               GUI.Fill_Rounded_Rectangle
                 ((330, 46), (25, 10), GUI.Get_RX_Status_Color, 1);
               GUI.Print ("RX", (300, 48));

               GUI.Fill_Rounded_Rectangle
                 ((420, 46), (25, 10), GUI.Get_TX_Status_Color, 1);
               GUI.Print ("TX", (390, 48));

               if RX_Ms /= 0 then
                  --  Float_Text_IO.Put (RX_Bytes_String, Float (Network.Ifnet.Rx_Stats.Bytes) / 1024.0 / 1024.0, 2, 0);
                  GUI.Print
                    ("Data:" &
                     Integer'Image
                       (Integer (Network.Ifnet.Rx_Stats.Bytes) / 1_024 /
                        1_024) &
                     "MB",
                     (280, 62));
               elsif RX_Ks /= 0 then
                  --  Float_Text_IO.Put (RX_Bytes_String, Float (Network.Ifnet.Rx_Stats.Bytes) / 1024.0, 2, 0);
                  GUI.Print
                    ("Data:" &
                     Integer'Image
                       (Integer (Network.Ifnet.Rx_Stats.Bytes) / 1_024) &
                     "KB",
                     (280, 62));
               else
                  --  Float_Text_IO.Put (RX_Bytes_String (1 .. 4), Integer (Network.Ifnet.Rx_Stats.Bytes));
                  GUI.Print
                    ("Data:" &
                     Integer'Image (Integer (Network.Ifnet.Rx_Stats.Bytes)),
                     (280, 62));
               end if;

               GUI.Print
                 ("Pkts:" & Tail (Network.Ifnet.Rx_Stats.Packets'Image, 4),
                  (280, 75));

               if TX_Ms /= 0 then
                  GUI.Print
                    ("Data:" &
                     Integer'Image
                       (Integer (Network.Ifnet.Tx_Stats.Bytes) / 1_024 /
                        1_024) &
                     "MB",
                     (390, 62));
               elsif RX_Ks /= 0 then
                  GUI.Print
                    ("Data:" &
                     Integer'Image
                       (Integer (Network.Ifnet.Tx_Stats.Bytes) / 1_024) &
                     "KB",
                     (390, 62));
               else
                  --  Float_Text_IO.Put (RX_Bytes_String (1 .. 4), Integer (Network.Ifnet.Rx_Stats.Bytes));
                  GUI.Print
                    ("Data:" &
                     Integer'Image (Integer (Network.Ifnet.Tx_Stats.Bytes)),
                     (390, 62));
               end if;

               GUI.Print
                 ("Pkts:" & Tail (Network.Ifnet.Tx_Stats.Packets'Image, 4),
                  (390, 75));

            end if;

            GUI.Print ("Version: " & GUI.Build_Verson, (362, 105));

            --  GUI.Print ("Status: ", (80, 45));
            --  GUI.Print ("Waiting for enrollment request", (145, 45));

            --  GUI.Print
         --    ("Ignored Packets: " & Network.Ifnet.Rx_Stats.Ignored'Image, (80, 30));

            Debug.Render;
         else
            --  File selector
            GUI.Draw_Rounded_Rectangle
              ((15, 20), (140, 246), GUI.Foreground, 2, 1);

            declare
               Button_Offset  : Natural := 0;
               Selected_Image : Picture_Message;
            begin
               GUI.Print ("File List", (15, 7), GUI.Background);

               if Image_Files.Is_Empty then
                  GUI.Print ("No Files", (50, 132), GUI.Button_Color);
               else
                  --  Left side shows the list of downloaded image files
                  for E of Image_Files loop
                     GUI.Fill_Rounded_Rectangle
                       ((17, 20 + Button_Offset + 5), (136, 20),
                        GUI.Button_Color, 2);

                     GUI.Print
                       (To_String (E.Identifier), (17, 24 + Button_Offset + 5),
                        GUI.Button_Color);

                     if GUI.Has_Touch_Within_Area
                         (State => State, P => (17, 20 + Button_Offset + 5),
                          S     => (136, 20))
                     then
                        Draw_Image := True;
                     end if;

                     Button_Offset := Button_Offset + 20;
                  end loop;
               end if;
            end;

            --  Image renderer
            GUI.Draw_Rounded_Rectangle
              ((160, 10), (256, 256), GUI.Foreground, 2, 1);

            if Draw_Image then
               null;
               --Texture.Print ();
            end if;

            --  Exit button background
            GUI.Fill_Rounded_Rectangle
              ((425, 200), (48, 65), GUI.Button_Color, 2);

            --  Close icon
            Texture_Close.Close.Print (Texture_Close.Bitmap, (433, 210));

            --  Text "Exit"
            GUI.Print ("Exit", (432, 250), GUI.Button_Color);

            --  Handle pressing the "Exit" button
            if GUI.Has_Touch_Within_Area
                (State => State, P => (425, 200), S => (48, 65))
            then
               Show_File_Browser := False;
            end if;
         end if;

         GUI.Update;

         --  Set activation time of next render in loop
         Next_Render := Next_Render + Period;

         delay until Next_Render;
      end;
   end loop;
end Demo;
