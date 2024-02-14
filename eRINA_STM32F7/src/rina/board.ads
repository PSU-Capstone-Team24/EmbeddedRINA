with STM32;
with STM32.Board;

package Board is
   protected type Button_Handler is
      procedure Pressed;
      pragma Attach_Handler (Pressed, STM32.Board.User_Button_Interrupt);
   end Button_Handler;
end Board;