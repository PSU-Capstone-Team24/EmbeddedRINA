with STM32;
with STM32.Board;
with Ada.Real_Time; use Ada.Real_Time;

package Board is

   procedure Initialize (Use_Rising_Edge : Boolean := True) with
     Pre => not Initialized, Post => Initialized;

   function Has_Been_Pressed return Boolean with
     Pre => Initialized;
   --  Returns whether the user button has been pressed since the last time
   --  this subprogram was called.

   function Initialized return Boolean;

   protected Button_Handler is
      pragma Interrupt_Priority;
      function Current_State return Boolean;
      procedure Clear_State;
   private
      procedure Interrupt;
      pragma Attach_Handler (Interrupt, STM32.Board.User_Button_Interrupt);

      Pressed    : Boolean := False;
      Start_Time : Time    := Clock;
   end Button_Handler;
end Board;
