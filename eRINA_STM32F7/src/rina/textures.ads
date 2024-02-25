with HAL; use HAL;
with HAL.Bitmap;

generic
  Width : Natural;
  Height : Natural;
package Textures is
   subtype Texture_Width_Index is Natural range 0 .. Width - 1;
   subtype Texture_Height_Index is Natural range 0 .. Height - 1;
   --  Dynamic size textures
   type Texture_Column is array (Texture_Height_Index) of UInt16 with
     Component_Size => 16;

   type Texture is
     array (Texture_Width_Index) of aliased Texture_Column with
     Pack;

   type Texture_Column_Access is access constant Texture_Column;
   type Texture_Access is access constant Texture;

   procedure Print (T : in Texture; Pos : in HAL.Bitmap.Point);
end Textures;