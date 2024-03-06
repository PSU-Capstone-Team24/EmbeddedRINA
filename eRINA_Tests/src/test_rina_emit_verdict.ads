--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Test_RINA_Emit_Verdict is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;
    procedure Test_Rina_Flow_Handle_Valid (Object : in out Test);
    procedure Test_Rina_Flow_Handle_Invalid (Object : in out Test);
    procedure Test_Rina_Flow_Handle_Invalid_FD (Object : in out Test);

end Test_RINA_Emit_Verdict;