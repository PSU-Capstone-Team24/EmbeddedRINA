with HAL.Bitmap;
with BMP_Fonts;
with Queues;

package Debug is
   type Debug is (Error, Warning, Info);

   Font_Height : constant Natural :=
     BMP_Fonts.Char_Height (Font => BMP_Fonts.Font8x8);
   Max_Characters_Per_Line : constant Positive         := 52;
   Line_Padding            : constant Natural          := 4;
   Starting_Point          : constant HAL.Bitmap.Point := (10, 110);

   --  Debug bitmap colors
   Info_Color    : constant HAL.Bitmap.Bitmap_Color := (255, 109, 177, 255);
   Warning_Color : constant HAL.Bitmap.Bitmap_Color := (255, 255, 227, 066);
   Error_Color   : constant HAL.Bitmap.Bitmap_Color := (255, 254, 112, 112);

   --  Maximum number of lines of messages in the console output
   Max_Messages : constant := 13;

   type Message is record
      Msg   : String (1 .. Max_Characters_Per_Line);
      Level : Debug;
      Cont  : Boolean := False;
   end record;

   type Queue_Max is mod Max_Messages;
   package Message_Queue is new Queues (Queue_Max, Message);
   Messages : Message_Queue.Queue;

   procedure Print (Debug_Level : in Debug; Msg : in String);
   procedure Clear;
   procedure Render;
end Debug;
