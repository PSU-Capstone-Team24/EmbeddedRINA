with Bitmapped_Drawing;
with GUI;

package body Debug is
  
   procedure Print (Debug_Level : in Debug; Msg : in String) is
      M : Message;
      
      --  These line variables shouldn't be confused with the array of lines that are drawn to the screen
      --  This is specifically for strings that are too long to fit on a single line, so they can be broken up
      Lines_Count : constant Positive := (Msg'Length + (Max_Characters_Per_Line - 1)) / Max_Characters_Per_Line;
      Lines : array(1 .. Lines_Count) of String(1 .. Max_Characters_Per_Line);
      Line_Start : Positive := Lines'First;
   begin
      for I in 1 .. Lines_Count loop
         --  Calculate the start of the next line segment. If it's the last segment, it may be shorter.
         if Line_Start + Max_Characters_Per_Line - 1 <= Msg'Length then
            Lines(I) := Msg(Line_Start .. Line_Start + Max_Characters_Per_Line - 1);
         else
            --  Last segment, potentially shorter than Max_Characters_Per_Line
            declare
               Last_Segment : constant String := Msg(Line_Start .. Msg'Last);
               Padded_Last_Segment : constant String(1 .. Max_Characters_Per_Line) := (Last_Segment & (Last_Segment'Length + 1 .. Max_Characters_Per_Line => ' '));
            begin
               Lines(I) := Padded_Last_Segment;
            end;
            
            --  Damn why can't it be this simple
            --  Lines(I) := Msg(Line_Start .. Msg'Length) & (others => ' '); -- Fill the rest with spaces
         end if;
         --  Prepare for the next segment
         Line_Start := Line_Start + Max_Characters_Per_Line;
      end loop;
      
      for I in Lines'Range loop
         M.Msg := Lines(I);
         M.Level := Debug_Level;
         
         --  Any lines after the first don't have the debug heading
         if I > 1 then
            M.Cont := True;
         end if;
         
         if Messages.Full then
            Messages.Pop;
         end if;

         Messages.Push (M);
      end loop;
   end Print;

   procedure Render is
      Foreground : HAL.Bitmap.Bitmap_Color;
      Background : HAL.Bitmap.Bitmap_Color;
      Pos : HAL.Bitmap.Point := Starting_Point;
      Msg : Message;
   begin
      for I in 0 .. Messages.Size - 1 loop
         Msg := Messages.Peek (Queue_Max(I));
         Pos := (Starting_Point.X, Starting_Point.Y + I * (Font_Height + Line_Padding));

         Foreground :=
         (case Msg.Level is when Info | Error => HAL.Bitmap.White,
            when Warning => HAL.Bitmap.Black);

         Background :=
         (case Msg.Level is when Info => HAL.Bitmap.Blue,
            when Warning => HAL.Bitmap.Yellow, when Error => HAL.Bitmap.Red);
         
         -- Restart X position to start
         Pos.X := Starting_Point.X;

         if not Msg.Cont then
            --  Draw debug prefix
            Bitmapped_Drawing.Draw_String
            (Buffer => GUI.Screen_Buffer.all,
               Start  =>
               GUI.Scale
                  ((Pos.X, Pos.Y)),
               Msg        => Msg.Level'Image, Font => BMP_Fonts.Font8x8,
               Foreground => Foreground, Background => Background);
            
            --  Offset X position for debug level header
            Pos.X := Pos.X + GUI.MeasureText (Msg.Level'Image, BMP_Fonts.Font8x8).Width;
         end if;

         --  Draw actual message text
         Bitmapped_Drawing.Draw_String
         (Buffer => GUI.Screen_Buffer.all,
            Start  =>
            GUI.Scale
               ((Pos.X + Line_Padding,
                  Pos.Y)),
            Msg => Msg.Msg, Font => BMP_Fonts.Font8x8, Foreground => HAL.Bitmap.White,
            Background => HAL.Bitmap.Black);
      end loop;
   end Render;
end Debug;
