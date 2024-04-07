--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Assertions;
with AUnit.Test_Caller;
with GNAT.OS_Lib;
with Bindings.Rlite.API;
with Exceptions;

package body Test_RINA_Emit_Verdict is
   use AUnit.Assertions;
   use GNAT.OS_Lib;
   use Bindings.Rlite.API;

   package OS renames GNAT.OS_Lib;
   
   RINA_Dev_FD : constant File_Descriptor := RINA_Open;

   package Caller is new AUnit.Test_Caller (Test);
   
   ------------------TS 006------------------
   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;
   
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name_021 : constant String := "TC-021";
      Name_022 : constant String := "TC-022";
      Name_023 : constant String := "TC-023";
   begin
      Test_Suite.Add_Test (Caller.Create
         (Name_021 & " Test Rina Flow Handle Valid", Test_Rina_Flow_Handle_Valid'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_022 & " Test Rina Flow Handle Invalid", Test_Rina_Flow_Handle_Invalid'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_023 & " Test Rina Flow Handle Invalid FD", Test_Rina_Flow_Valid_Handle_Invalid_FD'Access));

      return Test_Suite'Access;
   end Suite;

   -- TC 021
   procedure Test_Rina_Flow_Handle_Valid (Object : in out Test) is
      Rlite_Fd : File_Descriptor := RINA_Dev_FD;
   begin
      Rlite_Fd := RINA_Open;
      Assert(Rlite_Fd /= RINA_Dev_FD, "Invalid file descriptor returned");
   end Test_Rina_Flow_Handle_Valid;

   -- TC 022
   procedure Test_Rina_Flow_Handle_Invalid (Object : in out Test) is
      Fd : File_Descriptor := Invalid_FD;
   begin
      Fd := OS.Open_Read_Write ("/dev/this_should_fail", OS.Binary);
      Assert(Fd = Invalid_FD, "Device opened that should not be reachable");
   end Test_Rina_Flow_Handle_Invalid;

   -- TC 023
   procedure Test_Rina_Flow_Valid_Handle_Invalid_FD (Object : in out Test) is
      Fd : File_Descriptor := Invalid_FD;
      Register_Success : File_Descriptor;
   begin
      Fd := OS.Open_Read_Write ("/dev/this_should_fail", OS.Binary);
      Register_Success := RINA_Dev_FD;
      Assert(Register_Success /= Invalid_FD and Fd = Invalid_FD, "Registration successful but file descriptor is not invalid");
   end Test_Rina_Flow_Valid_Handle_Invalid_FD;

end Test_RINA_Emit_Verdict;
