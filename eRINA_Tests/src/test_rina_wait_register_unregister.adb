pragma Style_Checks (Off);

with AUnit.Assertions;
with AUnit.Test_Caller;
with GNAT.OS_Lib;
with Bindings.Rlite.API;
with Bindings.Rlite.Ctrl;
with Exceptions;

package body Test_RINA_Wait_Register_Unregister is
   use AUnit.Assertions;
   use GNAT.OS_Lib;
   use Bindings.Rlite.API;
   use Bindings.Rlite.Ctrl;

   Application_Name : constant String := "TestApplicationName";
   DIF_Name : constant String := "test.DIF";
   RINA_Dev_FD : constant File_Descriptor := RINA_Open;

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name_032 : constant String := "TC-032";
      Name_033 : constant String := "TC-033";
      Name_034 : constant String := "TC-034";
      Name_035 : constant String := "TC-035";
   begin
      Test_Suite.Add_Test (Caller.Create
         (Name_032 & " Verify rina_register_wait returns -1 for invalid wait FD", Test_Register_Wait_Invalid_Wait_FD'Access));
      --Test_Suite.Add_Test (Caller.Create
         --(Name_033 & " Verify rina_register_wait returns -1 for invalid FD", Test_Register_Wait_Invalid_FD'Access));
      --Test_Suite.Add_Test (Caller.Create
         --(Name_034 & " Verify rina_register_wait returns valid FD for valid FDs", Test_Register_Wait_Valid_FD'Access));
      --Test_Suite.Add_Test (Caller.Create
         --(Name_035 & " Verify app name corresponds to FD in namespace directory", Test_Register_Wait_App_Name_Correspondence'Access));

      return Test_Suite'Access;
   end Suite;

   -- Test Case 032: rina_register_wait Returns -1 for Invalid Wait File Descriptor
   procedure Test_Register_Wait_Invalid_Wait_FD (Object : in out Test) is
      Register_Wait_Success : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
      Register_Wait_Result : File_Descriptor;
   begin
      Register_Wait_Success := RINA_Register_Wait (RINA_Dev_FD, Invalid_FD);
      Assert (Register_Wait_Success = -1, "rina_register_wait did not return -1 for invalid wait FD");
   end Test_Register_Wait_Invalid_Wait_FD;

   -- Test Case 033: rina_register_wait Returns -1 for Invalid File Descriptor
   procedure Test_Register_Wait_Invalid_FD (Object : in out Test) is
      Register_Wait_Success : File_Descriptor := Invalid_FD;
   begin   
      Assert (Register_Wait_Success = -1, "rina_register_wait did not return -1 for invalid FD");
   end Test_Register_Wait_Invalid_FD;

   -- Test Case 034: rina_register_wait Returns Valid FD for Valid FDs
   procedure Test_Register_Wait_Valid_FD (Object : in out Test) is  
      Register_Wait_Success : File_Descriptor := Invalid_FD;
   begin   
      Assert (Register_Wait_Success /= -1, "rina_register_wait did not return a valid FD for valid FDs");
   end Test_Register_Wait_Valid_FD;

   -- Test Case 035: Verify Application Name Correspondence After rina_register_wait
   procedure Test_Register_Wait_App_Name_Correspondence (Object : in out Test) is 
      Register_Wait_Success : File_Descriptor := Invalid_FD;
   begin
      Assert (Register_Wait_Success /= -1, "App name does not correspond to FD number in namespace directory");
   end Test_Register_Wait_App_Name_Correspondence;


end Test_RINA_Wait_Register_Unregister;