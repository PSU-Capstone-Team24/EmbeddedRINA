with Bitmap_Color_Conversion;
with GUI;

package body Textures is
   procedure Print (T : in Texture; Pos : in HAL.Bitmap.Point) is
   begin
      for Row in Texture_Width_Index loop
         for Column in Texture_Height_Index loop
            GUI.Screen_Buffer.Set_Source
              (Bitmap_Color_Conversion.Word_To_Bitmap_Color
                 (HAL.Bitmap.ARGB_1555, HAL.UInt32 (T (Row) (Column))));

            --  If this byte is supposed to be transparent, don't even draw it
            if T (Row) (Column) > 16#0FFF# then
               GUI.Screen_Buffer.Set_Pixel ((Pos.X + Row, Pos.Y + Column));
            end if;
         end loop;
      end loop;
   end Print;
end Textures;
