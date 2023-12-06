--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Assertions;
with AUnit.Test_Caller;
with GNAT.OS_Lib;
with Bindings.Rlite.API;
with Names;
   use Names.Name_String;

with Ada.Strings;

package body Test_RINA_Register is
   use AUnit.Assertions;
   use GNAT.OS_Lib;
   use Bindings.Rlite.API;

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
      Dif_Name : Bounded_String;
      Caused_Error : Boolean := False;
   begin
      Dif_Name := To_Bounded_String ("nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn");
      exception
         when Ada.Strings.Length_Error =>
            Caused_Error := True;

      Assert(Caused_Error, "DIF_Name length > 128 characters");
   end Test_Register;

end Test_RINA_Register;
