--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Test_Suites;
with AUnit.Test_Fixtures;
with Bindings.Rlite.Ctrl;
with Bindings.Rlite.Msg.Flow;
with Names; use Names;
with Interfaces; use Interfaces;


package Test_RINA_Flow_Alloc is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Valid_Flow_Allocation (Object : in out Test);
   procedure Test_Invalid_Flags (Object : in out Test);
   --procedure Test_Incorrect_Flowspec_Version (Object : in out Test);
   --procedure Test_RINA_Open_Failure (Object : in out Test);
end Test_RINA_Flow_Alloc;
