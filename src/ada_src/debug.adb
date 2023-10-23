--  Temp disabling
pragma Style_Checks (Off);

with Ada.Text_IO;
with Ada.Characters.Latin_1;

package body Debug is
   procedure Print (Caller : String; Msg : String; Level : Debug_Level) is
      DEBUG : constant Boolean := true;
   begin
      if DEBUG then
         --  Trick the terminal into printing in color
         Ada.Text_IO.Put (Ada.Characters.Latin_1.ESC & "[104m");
         Ada.Text_IO.Put ("[" & Caller & "]");
         Ada.Text_IO.Put (Ada.Characters.Latin_1.ESC & "[0m ");

         case Level is
            when Info => Ada.Text_IO.Put (Ada.Characters.Latin_1.ESC & "[34m");
            when Warning => Ada.Text_IO.Put (Ada.Characters.Latin_1.ESC & "[33m");
            when Error => Ada.Text_IO.Put (Ada.Characters.Latin_1.ESC & "[31m");
         end case;

         Ada.Text_IO.Put (Msg);
         Ada.Text_IO.Put_Line (Ada.Characters.Latin_1.ESC & "[0m");
      end if;

   end Print;

end Debug;