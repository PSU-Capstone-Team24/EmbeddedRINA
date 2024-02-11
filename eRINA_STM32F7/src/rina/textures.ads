with HAL; use HAL;
with HAL.Bitmap;

package Textures is

   Texture_Width  : constant := 64;
   Texture_Height : constant := 64;

   type Texture_Column is array (0 .. Texture_Width - 1) of UInt16 with
     Component_Size => 16;

   type Texture is
     array (0 .. Texture_Height - 1) of aliased Texture_Column with
     Pack;

   type Texture_Column_Access is access constant Texture_Column;

   type Texture_Access is access constant Texture;

   procedure Print (T : in Texture; Pos : in HAL.Bitmap.Point);
end Textures;
