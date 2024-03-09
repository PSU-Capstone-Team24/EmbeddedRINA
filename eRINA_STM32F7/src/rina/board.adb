with Debug;
with STM32.Board;  use STM32.Board;
with STM32.Device; use STM32.Device;
with STM32.GPIO;   use STM32.GPIO;
with STM32.EXTI;   use STM32.EXTI;

package body Board is

   Button_High : Boolean := True;

   Debounce_Time : constant Time_Span := Milliseconds (250);

   EXTI_Line : constant External_Line_Number :=
     User_Button_Point.Interrupt_Line_Number;

   Initialization_Complete : Boolean := False;

   protected body Button_Handler is
      procedure Interrupt is
      begin
         Clear_External_Interrupt (EXTI_Line);
         Debug.Print (Debug.Info, "Button pressed!");
         if (Button_High and then User_Button_Point.Set)
           or else (not Button_High and then not User_Button_Point.Set)
         then
            if Clock - Start_Time > Debounce_Time then
               Pressed := True;
            end if;
         end if;
      end Interrupt;

      function Current_State return Boolean is
      begin
         return Pressed;
      end Current_State;

      procedure Clear_State is
      begin
         if Pressed then
            Start_Time := Clock;
            Pressed    := False;
         end if;
      end Clear_State;
   end Button_Handler;

   procedure Initialize (Use_Rising_Edge : Boolean := True) is
      Edge : constant External_Triggers :=
        (if Use_Rising_Edge then Interrupt_Rising_Edge
         else Interrupt_Falling_Edge);
   begin
      Enable_Clock (User_Button_Point);

      User_Button_Point.Configure_IO
        ((Mode      => Mode_In,
          Resistors => (if Use_Rising_Edge then Pull_Down else Pull_Up)));

      --  Connect the button's pin to the External Interrupt Handler
      User_Button_Point.Configure_Trigger (Edge, 0);

      Button_High             := Use_Rising_Edge;
      Initialization_Complete := True;
   end Initialize;

   function Has_Been_Pressed return Boolean is
      State : Boolean;
   begin
      State := Button_Handler.Current_State;
      Button_Handler.Clear_State;

      return State;
   end Has_Been_Pressed;

   function Initialized return Boolean is (Initialization_Complete);
end Board;
