with BMP_Fonts;
with HAL.Bitmap;
package GUI is
    pragma Warnings(Off);

    Large_Font : BMP_Fonts.BMP_Font := BMP_Fonts.Font12x12;
    Foreground : HAL.Bitmap.Bitmap_Color := HAL.Bitmap.White;
    Background : HAL.Bitmap.Bitmap_Color := HAL.Bitmap.Black;

    procedure Print (X : in Natural; Y : in Natural; Msg : in String);

    procedure Initialize (Title : in String);

    procedure PrintToConsole(Msg : in String);

    procedure CopyLine(SrcLineNumber : in Natural; DstLineNumer : in Natural; DeleteSrc : in Boolean);
end GUI;