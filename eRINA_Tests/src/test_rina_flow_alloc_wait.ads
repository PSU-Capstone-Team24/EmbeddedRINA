--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Test_Suites;
with AUnit.Test_Fixtures;
with Bindings.Rlite.Ctrl;
with Interfaces; use Interfaces;


package Test_RINA_Flow_Alloc_Wait is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Positive_Flow_Alloc_Response (Object : in out Test);
   --procedure Test_Negative_Flow_Alloc_Response (Object : in out Test);
   --procedure Test_No_Message_Available (Object : in out Test);
   --procedure Test_Unexpected_Error_During_Message_Reading (Object : in out Test);
end Test_RINA_Flow_Alloc_Wait;
