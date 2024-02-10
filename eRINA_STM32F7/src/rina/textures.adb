with Bitmap_Color_Conversion;
with GUI;

package body Textures is
   procedure Print (T : in Texture; Pos : in HAL.Bitmap.Point) is
   begin
      for Row in 0 .. Textures.Texture_Height - 1 loop
         for Column in 0 .. Textures.Texture_Width - 1 loop
            GUI.ScreenBuffer.Set_Source
              (Bitmap_Color_Conversion.Word_To_Bitmap_Color
                 (HAL.Bitmap.ARGB_1555, HAL.UInt32 (T (Row) (Column))));
            GUI.ScreenBuffer.Set_Pixel
              ((Pos.X + Row, Pos.Y + Column));
         end loop;
      end loop;
   end Print;
end Textures;
