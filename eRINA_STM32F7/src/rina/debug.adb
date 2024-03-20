with Bitmapped_Drawing;
with STM32.Board;
with GUI;

package body Debug is

   protected body Mutex is
      entry Seize when not Owned is
      begin
         Owned := True;
      end Seize;
      procedure Release is
      begin
         Owned := False;
      end Release;
   end Mutex;

   -- Probably a cleaner way to do this
   procedure Print (Debug_Level : in Debug; Msg : in String) is
      currentLine : constant Natural :=
        (CURRENT_CONSOLE_POSITION.Y - 100) / FONT_HEIGHT; -- zero indexed
      Foreground : HAL.Bitmap.Bitmap_Color;
      Background : HAL.Bitmap.Bitmap_Color;
   begin
      Print_Mutex.Seize;

      Foreground :=
        (case Debug_Level is when Info | Error => HAL.Bitmap.White,
           when Warning => HAL.Bitmap.Black);

      Background :=
        (case Debug_Level is when Info => HAL.Bitmap.Blue,
           when Warning => HAL.Bitmap.Yellow, when Error => HAL.Bitmap.Red);

      if currentLine > MAX_LINES then
         --  Shift console lines up
         for I in 1 .. MAX_LINES loop
            CopyLine (I, I - 1, I = MAX_LINES);
         end loop;

         -- Adjust Y position to overwrite the last line
         CURRENT_CONSOLE_POSITION.Y :=
           CURRENT_CONSOLE_POSITION.Y - FONT_HEIGHT - LINE_PADDING;
      end if;

      --  Draw debug prefix
      Bitmapped_Drawing.Draw_String
        (Buffer => GUI.Screen_Buffer.all,
         Start  =>
           GUI.Scale
             ((CURRENT_CONSOLE_POSITION.X, CURRENT_CONSOLE_POSITION.Y)),
         Msg        => Debug_Level'Image, Font => BMP_Fonts.Font8x8,
         Foreground => Foreground, Background => Background);

      --  Draw actual message text
      Bitmapped_Drawing.Draw_String
        (Buffer => GUI.Screen_Buffer.all,
         Start  =>
           GUI.Scale
             ((CURRENT_CONSOLE_POSITION.X +
               GUI.MeasureText (Debug_Level'Image, BMP_Fonts.Font8x8).Width +
               LINE_PADDING * 2,
               CURRENT_CONSOLE_POSITION.Y)),
         Msg => Msg, Font => BMP_Fonts.Font8x8, Foreground => HAL.Bitmap.White,
         Background => HAL.Bitmap.Black);

      -- Move the Y position down for the next message
      CURRENT_CONSOLE_POSITION.Y :=
        CURRENT_CONSOLE_POSITION.Y + FONT_HEIGHT + LINE_PADDING;

      STM32.Board.Display.Update_Layer (1, True);

      Print_Mutex.Release;
   end Print;

   procedure CopyLine
     (SrcLineNumber : in Natural; DstLineNumer : in Natural;
      DeleteSrc     : in Boolean)
   is
      --  TODO: Cleanup later...
      srcToPoint : constant HAL.Bitmap.Point :=
        (0,
         Natural'Min
           (CONSOLE_STARTING_POINT.Y +
            (FONT_HEIGHT + LINE_PADDING) * SrcLineNumber,
            GUI.Board_Resolution.Height - 2));
      dstToPoint : constant HAL.Bitmap.Point :=
        (0,
         Natural'Min
           (CONSOLE_STARTING_POINT.Y +
            (FONT_HEIGHT + LINE_PADDING) * DstLineNumer,
            GUI.Board_Resolution.Height - 2));
      srcRect : constant HAL.Bitmap.Rect :=
        (Position => srcToPoint, Width => GUI.Board_Resolution.Width,
         Height   => FONT_HEIGHT + LINE_PADDING);
   begin
      HAL.Bitmap.Copy_Rect
        (Src_Buffer => GUI.Screen_Buffer.all, Src_Pt => srcToPoint,
         Dst_Buffer => GUI.Screen_Buffer.all, Dst_Pt => dstToPoint,
         Width      => GUI.Board_Resolution.Width,
         Height     => FONT_HEIGHT + LINE_PADDING, Synchronous => True);
      if DeleteSrc then
         HAL.Bitmap.Fill_Rect
           (Buffer => GUI.Screen_Buffer.all, Area => srcRect);
      end if;
      STM32.Board.Display.Update_Layer (1, True);
   end CopyLine;
end Debug;
