--  Temp disabling
pragma Style_Checks (Off);

--  This is just a wrapper around Ada.Text_IO that only
--  prints in development for specific debug conditions
package Debug is
   procedure Print (Msg : String);
end Debug;