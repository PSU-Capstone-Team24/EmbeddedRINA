with Bitmapped_Drawing;
with GUI;

package body Debug is

   procedure Clear is
   begin
      while not Messages.Empty loop
         Messages.Pop;
      end loop;
   end Clear;

   procedure Print (Debug_Level : in Debug; Msg : in String) is
      M : Message;

      --  These line variables shouldn't be confused with the array of lines that are drawn to the screen
      --  This is specifically for strings that are too long to fit on a single line, so they can be broken up
      Lines_Count : constant Natural :=
        (Msg'Length + (Max_Characters_Per_Line - 1)) / Max_Characters_Per_Line;
      Lines :
        array (1 .. Lines_Count) of String (1 .. Max_Characters_Per_Line);
      Line_Start : Positive := Lines'First;
   begin
      for I in 1 .. Lines_Count loop
         --  Calculate the start of the next line segment. If it's the last segment, it may be shorter.
         if Line_Start + Max_Characters_Per_Line - 1 <= Msg'Length then
            Lines (I) :=
              Msg (Line_Start .. Line_Start + Max_Characters_Per_Line - 1);
         else
            --  Last segment, potentially shorter than Max_Characters_Per_Line
            declare
               Last_Segment : constant String := Msg (Line_Start .. Msg'Last);
               Padded_Last_Segment :
                 constant String (1 .. Max_Characters_Per_Line) :=
                 (Last_Segment &
                  (Last_Segment'Length + 1 .. Max_Characters_Per_Line => ' '));
            begin
               Lines (I) := Padded_Last_Segment;
            end;

            --  Damn why can't it be this simple
            --  Lines(I) := Msg(Line_Start .. Msg'Length) & (others => ' '); -- Fill the rest with spaces
         end if;
         --  Prepare for the next segment
         Line_Start := Line_Start + Max_Characters_Per_Line;
      end loop;

      for I in Lines'Range loop
         M.Msg   := Lines (I);
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
      Pos        : HAL.Bitmap.Point := Starting_Point;
      Msg        : Message;
   begin
      for I in 0 .. Messages.Size - 1 loop
         Msg := Messages.Peek (Queue_Max (I));
         Pos :=
           (Starting_Point.X,
            Starting_Point.Y + I * (Font_Height + Line_Padding));

         Foreground :=
           (case Msg.Level is when Info => HAL.Bitmap.Black,
              when Warning | Error => HAL.Bitmap.Black);

         Background :=
           (case Msg.Level is when Info => Info_Color,
              when Warning => Warning_Color, when Error => Error_Color);

         -- Restart X position to start
         Pos.X := Starting_Point.X;

         --  MT: TODO
         --  Draw over anything left over at this location
         --  I don't know why we need to do this since we're not copying in the old buffer?
         GUI.Draw_Rectangle
           ((Pos.X, Pos.Y), (GUI.Board_Resolution.Width - 20, Font_Height),
            HAL.Bitmap.White);

         if not Msg.Cont then
            --  Draw debug prefix
            Bitmapped_Drawing.Draw_String
              (Buffer     => GUI.Screen_Buffer.all,
               Start => GUI.Scale ((Pos.X, Pos.Y)), Msg => Msg.Level'Image,
               Font       => BMP_Fonts.Font8x8, Foreground => Foreground,
               Background => Background);

            --  Offset X position for debug level header
            Pos.X :=
              Pos.X +
              GUI.MeasureText (Msg.Level'Image, BMP_Fonts.Font8x8).Width +
              Line_Padding;
         end if;

         --  Draw actual message text
         Bitmapped_Drawing.Draw_String
           (Buffer     => GUI.Screen_Buffer.all,
            Start      => GUI.Scale ((Pos.X, Pos.Y)), Msg => Msg.Msg,
            Font       => BMP_Fonts.Font8x8, Foreground => HAL.Bitmap.Black,
            Background => HAL.Bitmap.White);
      end loop;
   end Render;
end Debug;
