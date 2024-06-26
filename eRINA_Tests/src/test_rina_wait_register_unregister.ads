pragma Style_Checks (Off);

with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Test_RINA_Wait_Register_Unregister is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Register_Wait_Invalid_Wait_FD (Object : in out Test);
   procedure Test_Register_Wait_Invalid_FD (Object : in out Test);
   procedure Test_Register_Wait_Valid_FD (Object : in out Test);
   procedure Test_Register_Wait_App_Name_Correspondence (Object : in out Test);
   

end Test_RINA_Wait_Register_Unregister;