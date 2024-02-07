with Bitmapped_Drawing;
with HAL.Bitmap;
with STM32.Board;
with STM32.RNG.Interrupts;


package body GUI is
    pragma Warnings (Off);

    CONSOLE_STARTING_POINT   : HAL.Bitmap.Point := (0, 100);
    CURRENT_CONSOLE_POSITION : HAL.Bitmap.Point := (0, 100);
    FONT_HEIGHT : Natural := BMP_Fonts.Char_Height(Font => Large_Font);


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

    -- Probably a cleaner way to do this
    procedure PrintToConsole(Msg : in String) is
        currentLine : Natural := (CURRENT_CONSOLE_POSITION.Y - 100) / FONT_HEIGHT; -- zero indexed
    begin
        if currentLine = 14 then --make dynamic
            declare
                iterator : Integer := 1;
            begin
                while iterator < 13 loop --make dynamic
                    CopyLine(iterator, iterator - 1, false);
                    iterator := iterator + 1;
                end loop;
                CopyLine(13, 12, True); --make dynamic
            end;
            CURRENT_CONSOLE_POSITION.Y := CURRENT_CONSOLE_POSITION.Y - FONT_HEIGHT;
            Print(CURRENT_CONSOLE_POSITION.X, CURRENT_CONSOLE_POSITION.Y, Msg);
            CURRENT_CONSOLE_POSITION.Y := CURRENT_CONSOLE_POSITION.Y + FONT_HEIGHT;

        else
            Print(CURRENT_CONSOLE_POSITION.X, CURRENT_CONSOLE_POSITION.Y, Msg);
            CURRENT_CONSOLE_POSITION.Y := CURRENT_CONSOLE_POSITION.Y + FONT_HEIGHT;
        end if;    

        STM32.Board.Display.Update_Layer(1, True);
    end PrintToConsole;


    procedure CopyLine(SrcLineNumber : in Natural; DstLineNumer : in Natural; DeleteSrc : in Boolean) is
        srcToPoint : HAL.Bitmap.Point := (0, (CONSOLE_STARTING_POINT.Y + (FONT_HEIGHT * SrcLineNumber)));
        dstToPoint : HAL.Bitmap.Point := (0, (CONSOLE_STARTING_POINT.Y + (FONT_HEIGHT * DstLineNumer)));
        srcRect : HAL.Bitmap.Rect := (Position => srcToPoint, Width => 480, Height => FONT_HEIGHT); -- dont hardcode 480
    begin
        HAL.Bitmap.Copy_Rect(Src_Buffer => STM32.Board.Display.Hidden_Buffer (1).all, Src_Pt => srcToPoint, Dst_Buffer => STM32.Board.Display.Hidden_Buffer (1).all, Dst_Pt => dstToPoint, Width => 480, Height => FONT_HEIGHT, Synchronous => True);
        if DeleteSrc then
            HAL.Bitmap.Fill_Rect(Buffer => STM32.Board.Display.Hidden_Buffer (1).all, Area => srcRect);
        end if;
        STM32.Board.Display.Update_Layer(1, True);
    end CopyLine;


end GUI;