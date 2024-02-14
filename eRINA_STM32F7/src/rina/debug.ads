with HAL.Bitmap;
with BMP_Fonts;

package Debug is
   type Debug is (Error, Warning, Info);

   FONT_HEIGHT              : constant Natural :=
     BMP_Fonts.Char_Height (Font => BMP_Fonts.Font8x8);
   MAX_LINES    : constant Natural := 20;
   LINE_PADDING : constant Natural := 2;

   protected Console is
      procedure Print (Debug_Level : in Debug; Msg : in String);
      procedure CopyLine
       (SrcLineNumber : in Natural; DstLineNumer : in Natural;
        DeleteSrc     : in Boolean);
   private
      CONSOLE_STARTING_POINT   : HAL.Bitmap.Point := (0, 100);
      CURRENT_CONSOLE_POSITION : HAL.Bitmap.Point := (0, 100);
   end Console;

end Debug;
