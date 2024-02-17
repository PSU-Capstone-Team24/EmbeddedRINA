with Bitmapped_Drawing;
with STM32.Board;
with STM32.RNG.Interrupts;

package body GUI is

   function Screen_Buffer return Any_Bitmap_Buffer is
   begin
      return STM32.Board.Display.Hidden_Buffer (1);
   end Screen_Buffer;

   function Scale (Position : in Point) return Point is
   begin
      if STM32.Board.LCD_Natural_Width > Board_Resolution.Width then
         return
           (Position.X * 800 / Board_Resolution.Width,
            Position.Y * Board_Resolution.Width / Board_Resolution.Width);
      else
         return Position;
      end if;
   end Scale;

   procedure Update is
   begin
      STM32.Board.Display.Update_Layer (1);
   end Update;

   procedure Draw_Rectangle (P : Point; S : Size) is
   begin
      Set_Source (Buffer => Screen_Buffer.all, ARGB => Red);
      Fill_Rect (Buffer => Screen_Buffer.all, Area => (P, S.Width, S.Height));
   end Draw_Rectangle;

   procedure Print (Msg : in String; Pos : in Point) is
   begin
      Bitmapped_Drawing.Draw_String
        (Buffer     => Screen_Buffer.all, Start => Scale ((Pos.X, Pos.Y)),
         Msg => Msg, Font => BMP_Fonts.Font8x8, Foreground => Foreground,
         Background => Background);
   end Print;

   procedure Initialize is
   begin
      STM32.RNG.Interrupts.Initialize_RNG;
      STM32.Board.Display.Initialize;
      STM32.Board.Display.Initialize_Layer (1, ARGB_1555);
   end Initialize;

   function MeasureText
     (Text : in String; Font : in BMP_Fonts.BMP_Font) return Size
   is
   begin
      return
        (Text'Length * BMP_Fonts.Char_Width (Font),
         BMP_Fonts.Char_Height (Font));
   end MeasureText;
end GUI;
