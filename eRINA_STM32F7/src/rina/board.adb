with Debug;

package body Board is
   protected body Button_Handler is
      procedure Pressed is
      begin
         STM32.Board.All_LEDs_On;
         Debug.Print(Debug.Info, "Button pressed!");
      end Pressed;
   end Button_Handler;
end Board;