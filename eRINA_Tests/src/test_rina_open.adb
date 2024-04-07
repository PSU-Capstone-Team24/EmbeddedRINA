--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Assertions;
with AUnit.Test_Caller;
with GNAT.OS_Lib;
with Bindings.Rlite.API;

package body Test_RINA_Open is
   use AUnit.Assertions;
   use GNAT.OS_Lib;
   use Bindings.Rlite.API;

   package OS renames GNAT.OS_Lib;

   package Caller is new AUnit.Test_Caller (Test);
   
   -----------------------TS-001-----------------------
   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name_001 : constant String := "TC-001";
      Name_002 : constant String := "TC-002";
   begin
      Test_Suite.Add_Test (Caller.Create
         (Name_001 & " rLite FD can be opened", Test_Open'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_002 & " Cannot open invalid FD", Test_Open_Failure'Access));

      return Test_Suite'Access;
   end Suite;

   -- TC 001
   procedure Test_Open (Object : in out Test) is
      Rlite_Fd : File_Descriptor := Invalid_FD;
   begin
      Rlite_Fd := RINA_Open;
      Assert(Rlite_Fd /= Invalid_FD, "Invalid file descriptor returned");
   end Test_Open;

   -- TC 002
   procedure Test_Open_Failure (Object : in out Test) is
      Fd : File_Descriptor := Invalid_FD;
   begin
      Fd := OS.Open_Read_Write ("/dev/this_should_fail", OS.Binary);
      Assert(Fd = Invalid_FD, "Device opened that should not be reachable");
   end Test_Open_Failure;

end Test_RINA_Open;
