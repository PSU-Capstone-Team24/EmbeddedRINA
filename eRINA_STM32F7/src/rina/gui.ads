with BMP_Fonts;
with HAL.Bitmap; use HAL.Bitmap;
with Ada.Containers.Vectors;
with HAL.Touch_Panel; use HAL.Touch_Panel;
package GUI is
   pragma Warnings (Off);

   Large_Font   : BMP_Fonts.BMP_Font := BMP_Fonts.Font12x12;
   Foreground   : Bitmap_Color       := Black;
   Background   : Bitmap_Color       := White;
   Button_Color : Bitmap_Color       := (255, 242, 243, 245);
   Button_Selected_Color : Bitmap_Color := (255, 255, 255, 255);
   TX_Active    : Bitmap_Color       := (255, 111, 255, 109);
   TX_Inactive  : Bitmap_Color       := (255, 56, 128, 55);
   RX_Active    : Bitmap_Color       := (255, 255, 109, 109);
   RX_Inactive  : Bitmap_Color       := (255, 128, 55, 55);
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
   function Get_TX_Status_Color return Bitmap_Color;
   function Get_RX_Status_Color return Bitmap_Color;
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
   function Has_Touch_Within_Area (State : HAL.Touch_Panel.TP_State; P : Point; S : Size) return Boolean;
   function Has_Touch (State : HAL.Touch_Panel.TP_State) return Boolean;
end GUI;
