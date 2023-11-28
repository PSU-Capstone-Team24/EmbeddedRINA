--  Temp disabling
pragma Style_Checks (Off);

--  AUnit
with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;

--  Test suites
with Test_RINA_Open;
with Test_RINA_Flow_Alloc_Wait;
with Test_RINA_Flow_Alloc;

procedure eRINA_Tests is
   
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite := AUnit.Test_Suites.New_Suite;
   begin
      Result.Add_Test (Test_RINA_Open.Suite);
      Result.Add_Test (Test_RINA_Flow_Alloc_Wait.Suite);
      Result.Add_Test (Test_RINA_Flow_Alloc.Suite);
      return Result;
   end Suite;

   procedure Runner is new AUnit.Run.Test_Runner (Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Reporter.Set_Use_ANSI_Colors (True);
   Runner (Reporter);
end eRINA_Tests;