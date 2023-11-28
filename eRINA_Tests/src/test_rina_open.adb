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
   
   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "TC-001 ";
   begin
      Test_Suite.Add_Test (Caller.Create
         (Name & "Valid rLite File Descriptor", Test_Open'Access));
      
      return Test_Suite'Access;
   end Suite;

   procedure Test_Open (Object : in out Test) is
      Fd : File_Descriptor := Invalid_FD;
   begin
      Fd := RINA_Open;
      Assert(Fd /= Invalid_FD, "Invalid file descriptor returned");
   end Test_Open;

end Test_RINA_Open;
