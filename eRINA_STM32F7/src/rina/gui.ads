with BMP_Fonts;
with HAL.Bitmap;

package GUI is
   pragma Warnings (Off);

   Large_Font : BMP_Fonts.BMP_Font      := BMP_Fonts.Font12x12;
   Foreground : HAL.Bitmap.Bitmap_Color := HAL.Bitmap.White;
   Background : HAL.Bitmap.Bitmap_Color := HAL.Bitmap.Black;

   type Size is record
      Width  : Natural;
      Height : Natural;
   end record;

   Board_Resolution : Size := (480, 272);
   Frame_Rate : Natural := 30;

   procedure Initialize;
   procedure Update;
   procedure Print (Msg : in String; Pos : in HAL.Bitmap.Point);
   function Screen_Buffer return HAL.Bitmap.Any_Bitmap_Buffer;

   function MeasureText
     (Text : in String; Font : in BMP_Fonts.BMP_Font) return Size;
   function Scale (Point : in HAL.Bitmap.Point) return HAL.Bitmap.Point;
end GUI;
