with BMP_Fonts;
with HAL.Bitmap; use HAL.Bitmap;

package GUI is
   pragma Warnings (Off);

   Large_Font : BMP_Fonts.BMP_Font := BMP_Fonts.Font12x12;
   Foreground : Bitmap_Color       := White;
   Background : Bitmap_Color       := Black;

   type Size is record
      Width  : Natural;
      Height : Natural;
   end record;

   type Button is record
      Position : Point;
      BSize    : Size;
   end record;

   Board_Resolution : Size    := (480, 272);
   Frame_Rate       : Natural := 30;

   procedure Initialize;
   procedure Update;
   procedure Print (Msg : in String; Pos : in Point);
   function Screen_Buffer return Any_Bitmap_Buffer;
   procedure Draw_Rectangle (P : Point; S : Size);

   function MeasureText
     (Text : in String; Font : in BMP_Fonts.BMP_Font) return Size;
   function Scale (Position : in Point) return Point;
end GUI;
