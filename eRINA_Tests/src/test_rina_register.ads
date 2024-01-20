--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Test_RINA_Register is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Register_DIF_Length (Object : in out Test);
   procedure Test_Register_DIF_Empty (Object : in out Test);
   procedure Test_Register_Not_Exist_IPCP (Object : in out Test);
   procedure Test_Register_Application_Length (Object : in out Test);
   procedure Test_Register_Application_Empty (Object : in out Test);

end Test_RINA_Register;
