--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Assertions;
with AUnit.Test_Caller;
with GNAT.OS_Lib;
with Bindings.Rlite.API;
with Names;

package body Test_RINA_Register is
   use AUnit.Assertions;
   use GNAT.OS_Lib;
   use Bindings.Rlite.API;
   use Names;

   package OS renames GNAT.OS_Lib;

   package Caller is new AUnit.Test_Caller (Test);
   
   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "RINA_Register";
   begin
      Test_Suite.Add_Test (Caller.Create
         (Name & " - Fails when DIF name too long", Test_Register'Access));
      
      return Test_Suite'Access;
   end Suite;

   procedure Test_Register (Object : in out Test) is
      Fd : File_Descriptor := Invalid_FD;
      Dif_Name : Bounded_String := "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn";
   begin
      Fd := Create_Temp_File(Fd, "Temp_Test_File");
      Fd := RINA_Register(Fd, Dif_Name, "Local_App_Name", 1);
      Assert(Fd = -1, "Valid file descriptor returned");
   end Test_Register;

end Test_RINA_Register;
