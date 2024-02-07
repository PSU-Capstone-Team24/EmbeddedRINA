with Bitmapped_Drawing;
with HAL.Bitmap;
with STM32.Board;
with STM32.RNG.Interrupts;


package body GUI is
    pragma Warnings (Off);

    CONSOLE_STARTING_POINT   : HAL.Bitmap.Point := (0, 100);
    CURRENT_CONSOLE_POSITION : HAL.Bitmap.Point := (0, 100);


    function Scale (Point : in HAL.Bitmap.Point) return HAL.Bitmap.Point;

    function Scale (Point : in HAL.Bitmap.Point) return HAL.Bitmap.Point is
    begin
        if STM32.Board.LCD_Natural_Width > 480 then
            return (Point.X * 800 / 480, Point.Y * 480 / 272);
        else
            return Point;
      end if;
    end Scale;

    procedure Print (X : in Natural; Y : in Natural; Msg : in String) is
    begin
        Bitmapped_Drawing.Draw_String (Buffer     => STM32.Board.Display.Hidden_Buffer (1).all,
                                       Start      => Scale ((X,Y)),
                                       Msg        => Msg,
                                       Font       => Large_Font,
                                       Foreground => Foreground,
                                       Background => Background);
    end Print;

    procedure Initialize (Title : in String) is
    begin
        STM32.RNG.Interrupts.Initialize_RNG;
        STM32.Board.Display.Initialize;
        STM32.Board.Display.Initialize_Layer (1, HAL.Bitmap.ARGB_1555);
        Print(0, 0, Title);
    end Initialize;

    procedure PrintToConsole(Msg : in String) is
    begin
        Print(CURRENT_CONSOLE_POSITION.X, CURRENT_CONSOLE_POSITION.Y, Msg);
        CURRENT_CONSOLE_POSITION := (0, CURRENT_CONSOLE_POSITION.Y + 12);
        STM32.Board.Display.Update_Layer(1, True);
    end PrintToConsole;

    procedure CopyLine(SrcLineNumber : in Natural; DstLineNumer : in Natural; DeleteSrc : in Boolean) is
        fontHeight : Natural := BMP_Fonts.Char_Height(Font => Large_Font);
        srcToPoint : HAL.Bitmap.Point := (0, (CONSOLE_STARTING_POINT.Y + (fontHeight * SrcLineNumber)));
        dstToPoint : HAL.Bitmap.Point := (0, (CONSOLE_STARTING_POINT.Y + (fontHeight * DstLineNumer)));
        srcRect : HAL.Bitmap.Rect := (Position => srcToPoint, Width => 480, Height => fontHeight);
    begin
        HAL.Bitmap.Copy_Rect(Src_Buffer => STM32.Board.Display.Hidden_Buffer (1).all, Src_Pt => srcToPoint, Dst_Buffer => STM32.Board.Display.Hidden_Buffer (1).all, Dst_Pt => dstToPoint, Width => 480, Height => fontHeight, Synchronous => True);
        if DeleteSrc then
            HAL.Bitmap.Fill_Rect(Buffer => STM32.Board.Display.Hidden_Buffer (1).all, Area => srcRect);
        end if;
        STM32.Board.Display.Update_Layer(1, True);
    end CopyLine;


end GUI;