with BMP_Fonts;
with HAL.Bitmap; use HAL.Bitmap;
with Ada.Containers.Vectors;

package GUI is
   pragma Warnings (Off);

   Large_Font   : BMP_Fonts.BMP_Font := BMP_Fonts.Font12x12;
   Foreground   : Bitmap_Color       := Black;
   Background   : Bitmap_Color       := White;
   Button_Color : Bitmap_Color       := (255, 242, 243, 245);
   Build_Verson : String             := "0.0.1";

   type Size is record
      Width  : Natural;
      Height : Natural;
   end record;

   type Access_String is access all String;

   type Button is record
      Position : Point;
      BSize    : Size;
      Color    : Bitmap_Color;
      Text     : Access_String;
   end record;

   Board_Resolution : Size    := (480, 272);
   Frame_Rate       : Natural := 60;
   Max_Buttons      : Natural := 16;

   package Button_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Button);

   Buttons : Button_Vectors.Vector;

   procedure Initialize;
   procedure Update;
   procedure Print (Msg : in String; Pos : in Point);
   procedure Print_Large (Msg : in String; Pos : in Point);
   function Screen_Buffer return Any_Bitmap_Buffer;
   procedure Draw_Rectangle (P : Point; S : Size; C : Bitmap_Color);
   procedure Draw_Rounded_Rectangle
     (P : Point; S : Size; C : Bitmap_Color; R : Natural; T : Natural);
   procedure Fill_Rounded_Rectangle
     (P : Point; S : Size; C : Bitmap_Color; R : Natural);
   procedure Add_Button
     (P : Point; S : Size; C : Bitmap_Color; T : Access_String);
   function MeasureText
     (Text : in String; Font : in BMP_Fonts.BMP_Font) return Size;
   function Scale (Position : in Point) return Point;
end GUI;
