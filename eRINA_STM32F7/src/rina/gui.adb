with Bitmapped_Drawing;
with STM32.Board;
with STM32.RNG.Interrupts; use STM32.RNG.Interrupts;
with Net.Interfaces.STM32;

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

   function Get_TX_Status_Color return Bitmap_Color is
   begin
      return
        (if Net.Interfaces.STM32.TX_Active then GUI.TX_Active
         else GUI.TX_Inactive);
   end Get_TX_Status_Color;

   function Get_RX_Status_Color return Bitmap_Color is
   begin
      return
        (if Net.Interfaces.STM32.RX_Active then GUI.RX_Active
         else GUI.RX_Inactive);
   end Get_RX_Status_Color;

   procedure Draw_Rectangle (P : Point; S : Size; C : Bitmap_Color) is
   begin
      Set_Source (Buffer => Screen_Buffer.all, ARGB => C);
      Fill_Rect (Buffer => Screen_Buffer.all, Area => (P, S.Width, S.Height));
   end Draw_Rectangle;

   procedure Draw_Rounded_Rectangle
     (P : Point; S : Size; C : Bitmap_Color; R : Natural; T : Natural)
   is
   begin
      Set_Source (Buffer => Screen_Buffer.all, ARGB => C);
      Draw_Rounded_Rect
        (Buffer => Screen_Buffer.all, Area => (P, S.Width, S.Height),
         Radius => R, Thickness => T);
   end Draw_Rounded_Rectangle;

   procedure Fill_Rounded_Rectangle
     (P : Point; S : Size; C : Bitmap_Color; R : Natural)
   is
   begin
      Set_Source (Buffer => Screen_Buffer.all, ARGB => C);
      Fill_Rounded_Rect
        (Buffer => Screen_Buffer.all, Area => (P, S.Width, S.Height),
         Radius => R);
   end Fill_Rounded_Rectangle;

   procedure Add_Button
     (P : Point; S : Size; C : Bitmap_Color; T : Access_String)
   is
      Btn : Button := (P, S, C, T);
   begin
      Buttons.Append (Btn);
   end Add_Button;

   procedure Print (Msg : in String; Pos : in Point) is
   begin
      Bitmapped_Drawing.Draw_String
        (Buffer     => Screen_Buffer.all, Start => Scale ((Pos.X, Pos.Y)),
         Msg => Msg, Font => BMP_Fonts.Font8x8, Foreground => Foreground,
         Background => Background);
   end Print;

   procedure Print_Large (Msg : in String; Pos : in Point) is
   begin
      Bitmapped_Drawing.Draw_String
        (Buffer     => Screen_Buffer.all, Start => Scale ((Pos.X, Pos.Y)),
         Msg => Msg, Font => BMP_Fonts.Font12x12, Foreground => Foreground,
         Background => Background);
   end Print_Large;

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

   function Has_Touch_Within_Area(State : HAL.Touch_Panel.TP_State; P : Point; S : Size) return Boolean is
   begin
      if State'Length > 0 then
            if State (1).X >= P.X and State (1).X <= P.X + S.Width and
               State (1).Y >= P.Y and State (1).Y <= P.Y + S.Height then
               return True;
            else 
               return False;
            end if;
      end if;
   end Has_Touch_Within_Area;

   function Has_Touch(State : HAL.Touch_Panel.TP_State) return Boolean is
   begin
      if State'Length > 0 then
         return True;
      else
         return False;
      end if;
   end Has_Touch;

end GUI;
