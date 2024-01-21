--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Assertions;
with AUnit.Test_Caller;
with GNAT.OS_Lib;
with Bindings.Rlite.API;
with Names;
   use Names.Name_String;

with Ada.Strings;
with Debug;

package body Test_RINA_Register is
   use AUnit.Assertions;
   use GNAT.OS_Lib;
   use Bindings.Rlite.API;

   package Caller is new AUnit.Test_Caller (Test);
   
   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name_003 : constant String := "TC-003";
      Name_004 : constant String := "TC-004";
      Name_005 : constant String := "TC-005";
      Name_006 : constant String := "TC-006";
      Name_007 : constant String := "TC-007";
      Name_008 : constant String := "TC-008";
      Name_009 : constant String := "TC-009";
      Name_010 : constant String := "TC-010";
   begin
      Test_Suite.Add_Test (Caller.Create
         (Name_003 & " Fails when DIF name too long", Test_Register_DIF_Length'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_004 & " Fails when DIF name is empty", Test_Register_DIF_Empty'Access));
      --Test_Suite.Add_Test (Caller.Create
         --(Name_005 & " Fails when DIF name does not exist in IPCP", Test_Register_Not_Exist_IPCP'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_006 & " Fails when Application name too long", Test_Register_Application_Length'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_007 & " Fails when Application name is empty", Test_Register_Application_Empty'Access));
      --Test_Suite.Add_Test (Caller.Create
         --(Name_008 & " Fails when Application name has already been registered", Test_Register_Not_Exist_IPCP'Access));
      --Test_Suite.Add_Test (Caller.Create
         --(Name_009 & " Verify rina_register successfully reg. an App name to DIF", Test_Register_AppName_to_DIF'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_010 & " Verify rina_register returns -1 when inval. file descriptor is passed", Test_Register_Application_Empty'Access));

      return Test_Suite'Access;
   end Suite;

   --Test Case 003
   procedure Test_Register_DIF_Length (Object : in out Test) is
      Fd : File_Descriptor := Invalid_FD;
      Dif_Name : Bounded_String;
      Caused_Error : Boolean := False;
   begin
      Dif_Name := To_Bounded_String ("nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn");
      exception
         when Ada.Strings.Length_Error =>
            Caused_Error := True;

      Assert(Caused_Error, "DIF_Name length > 128 characters");
   end Test_Register_DIF_Length;

   --Test Case 004
   procedure Test_Register_DIF_Empty (Object : in out Test) is
      DIF_Name : Bounded_String := To_Bounded_String ("");
      Register_Success : File_Descriptor := Invalid_FD;
      RINA_Dev_FD : File_Descriptor := RINA_Open;
   begin
      Register_Success := RINA_Register (RINA_Dev_FD, DIF_Name, To_Bounded_String ("TestApplicationName"), 0);
      
      Assert(Register_Success = Invalid_FD, "DIF_Name blank");
   end Test_Register_DIF_Empty;

   --Test Case 005
   procedure Test_Register_Not_Exist_IPCP (Object : in out Test) is
      DIF_Name : Bounded_String := To_Bounded_String ("TestDIF"); 
      Register_Success : File_Descriptor := Invalid_FD;
      RINA_Dev_FD : File_Descriptor := RINA_Open;
   begin
      Register_Success := RINA_Register (RINA_Dev_FD, DIF_Name, To_Bounded_String ("TestApplicationName"), 0);
      --if fails, assert prints
      Assert(Register_Success = Invalid_FD, "IPCP already exist");
   end Test_Register_Not_Exist_IPCP;

   --Test Case 006
   procedure Test_Register_Application_Length (Object : in out Test) is
      Fd : File_Descriptor := Invalid_FD;
      App_Name : Bounded_String;
      Caused_Error : Boolean := False;
   begin
      App_Name := To_Bounded_String ("nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn");
      exception
         when Ada.Strings.Length_Error =>
            Caused_Error := True;

      Assert(Caused_Error, "Application_Name length > 128 characters");
   end Test_Register_Application_Length;

   --Test Case 007
   procedure Test_Register_Application_Empty (Object : in out Test) is
      App_Name : Bounded_String := To_Bounded_String ("");
      Register_Success : File_Descriptor := Invalid_FD;
      RINA_Dev_FD : File_Descriptor := RINA_Open;
   begin
      Register_Success := RINA_Register (RINA_Dev_FD, App_Name, To_Bounded_String ("TestApplicationName"), 0);
      
      Assert(Register_Success = Invalid_FD, "Application_Name blank");
   end Test_Register_Application_Empty;

-- Test Case 008
   procedure Test_Register_App_Already_Reg (Object : in out Test) is
      App_Name : Bounded_String := To_Bounded_String ("TestApplicationName");
      RINA_Dev_FD : File_Descriptor := RINA_Open;
      Register_First : File_Descriptor := RINA_Register (RINA_Dev_FD, App_Name, To_Bounded_String ("TestDIF"), 0);
      Register_Second : File_Descriptor := RINA_Register (RINA_Dev_FD, App_Name, To_Bounded_String ("TestDIF"), 0);
   begin
   -- The first registration should succeed, the second should fail
      Assert (Register_First /= Invalid_FD and Register_Second = Invalid_FD, "Duplicate application name registration did not fail");
   end Test_Register_App_Already_Reg;

   -- Test Case 009
   procedure Test_Register_AppName_to_DIF (Object : in out Test) is
      App_Name : Bounded_String := To_Bounded_String ("TestApplicationName");
      RINA_Dev_FD : File_Descriptor := RINA_Open;
      Register_Success : File_Descriptor := RINA_Register (RINA_Dev_FD, App_Name, To_Bounded_String ("TestDIF"), 0);
   begin
      Assert (Register_Success /= Invalid_FD, "Failed to register a unique application name");
   end Test_Register_AppName_to_DIF;

   -- Test Case 010
   procedure Test_Register_Invalid_File_Descriptor (Object : in out Test) is
      App_Name : Bounded_String := To_Bounded_String ("TestApplicationName");
      Invalid_FD : File_Descriptor := -1; -- Assuming -1 is an invalid file descriptor
      Register_Result : File_Descriptor := RINA_Register (Invalid_FD, App_Name, To_Bounded_String ("TestDIF"), 0);
   begin
      Assert (Register_Result = Invalid_FD, "rina_register did not return -1 for invalid file descriptor");
   end Test_Register_Invalid_File_Descriptor;

end Test_RINA_Register;
