--  Temp disabling
pragma Style_Checks (Off);

--  This is just a wrapper around Ada.Text_IO that only
--  prints in development for specific debug conditions
package Debug is
   type Debug_Level is (Info, Warning, Error);

   procedure Print (Caller : String; Msg : String; Level : Debug_Level);
end Debug;