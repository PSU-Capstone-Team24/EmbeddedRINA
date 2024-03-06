--  Temp disabling
pragma Style_Checks (Off);

--  AUnit
with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;

--  Test suites
with Test_RINA_Open; -- TS-001
with Test_RINA_Register; -- TS-002
with Test_RINA_Unregister; -- TS-003
with Test_RINA_ACCEPT_FLOW_REQ; -- TS-005
with Test_RINA_Emit_Verdict; -- TS-006
with Test_RINA_Flow_Alloc; -- TS-007
with Test_RINA_Flow_Alloc_Wait; -- TS-008

procedure eRINA_Tests is
   
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite := AUnit.Test_Suites.New_Suite;
   begin
      Result.Add_Test (Test_RINA_Open.Suite); -- TS-001
      Result.Add_Test (Test_RINA_Register.Suite); -- TS-002
      Result.Add_Test (Test_RINA_Unregister.Suite); -- TS-003
      Result.Add_Test (Test_RINA_Flow_Alloc.Suite); -- TS-00?
      Result.Add_Test (Test_RINA_ACCEPT_FLOW_REQ.Suite); -- TS-005
      Result.Add_Test (Test_RINA_Emit_Verdict.Suite); -- TS-006
      Result.Add_Test (Test_RINA_Flow_Alloc_Wait.Suite); -- TS-008
      return Result;
   end Suite;

   procedure Runner is new AUnit.Run.Test_Runner (Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Reporter.Set_Use_ANSI_Colors (True);
   Runner (Reporter);
end eRINA_Tests;