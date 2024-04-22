--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Test_RINA_Unregister is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Unregister_AppName_to_DIF (Object : in out Test);
   procedure Test_Unregister_DIF_Name_Too_Long (Object : in out Test);
   procedure Test_Unregister_DIF_Name_Empty (Object : in out Test);
   procedure Test_Unregister_DIF_Name_Not_Registered (Object : in out Test);
   procedure Test_Unregister_App_Name_Too_Long (Object : in out Test);
   procedure Test_Unregister_App_Name_Empty (Object : in out Test);
   procedure Test_Unregister_Invalid_File_Descriptor (Object : in out Test);
   procedure Test_Unregister_Invalid_App_Descriptor (Object : in out Test);
   procedure Test_Unregister_Writes_DeReg (Object : in out Test);

end Test_RINA_Unregister;
