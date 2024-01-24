--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Assertions;
with AUnit.Test_Caller;
with GNAT.OS_Lib;
with Bindings.Rlite.API;
with Exceptions;

package body Test_RINA_Register is
   use AUnit.Assertions;
   use GNAT.OS_Lib;
   use Bindings.Rlite.API;

   package Caller is new AUnit.Test_Caller (Test);
   
   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name_003 : constant String := "TC-003";
      Name_004 : constant String := "TC-004";
      Name_008 : constant String := "TC-008";
      Name_009 : constant String := "TC-009";
      Name_010 : constant String := "TC-010";
   begin
      Test_Suite.Add_Test (Caller.Create
         (Name_003 & " Fails when DIF name too long", Test_Register_DIF_Length'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_004 & " Fails when DIF name is empty", Test_Register_DIF_Empty'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_008 & " Fails when DIF name is already registered", Test_Register_DIF_Already_Registered'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_009 & " Succeeds when DIF name is not already registered", Test_Register_DIF_Register_New'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_010 & " Fails when invalid file descriptor passed", Test_Register_DIF_Invalid_File_Descriptor'Access));

      return Test_Suite'Access;
   end Suite;

   procedure Test_Register_DIF_Length (Object : in out Test) is
      RINA_Dev_FD : constant File_Descriptor := RINA_Open;
      Dif_Name : constant String := "this___is___a___really___long___dif___name___that___should___fail___because___it___is___over___128___characters___long___________";
      Register_Success : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
   begin
      
      Register_Success := RINA_Register (RINA_Dev_FD, DIF_Name, Dif_Name, 0);
      exception
         when Exceptions.Bounded_Length_Expcetion =>
            Caused_Error := True;

      Assert(Caused_Error and Register_Success = Invalid_FD, "DIF_Name allowed to register with length > 128 characters");
   end Test_Register_DIF_Length;

   procedure Test_Register_DIF_Empty (Object : in out Test) is
      RINA_Dev_FD : constant File_Descriptor := RINA_Open;
      DIF_Name : constant String := "";
      Register_Success : File_Descriptor := Invalid_FD;
   begin
      Register_Success := RINA_Register (RINA_Dev_FD, DIF_Name, "TestApplicationName", 0);
      
      Assert(Register_Success = Invalid_FD, "DIF_Name blank");
   end Test_Register_DIF_Empty;

   procedure Test_Register_DIF_Already_Registered (Object : in out Test) is
      DIF_Name : String := "Test DIF";
      App_Name : String := "Test App";
      Register_Result : File_Descriptor := Invalid_FD;
      RINA_Dev_FD : File_Descriptor := RINA_Open;
      Caused_Error : Boolean := False;
   begin
      -- First success registration
      Register_Result := RINA_Register (RINA_Dev_FD, DIF_Name, App_Name, 0);
      -- Second registration failure
      Register_Result := RINA_Register (RINA_Dev_FD, DIF_Name, App_Name, 0);
      exception
         when Exceptions.DIF_Registration_Failure =>
            Caused_Error := True;   
      Assert(Caused_Error, "App_Name already registered");
   end Test_Register_DIF_Already_Registered;

   procedure Test_Register_DIF_Register_New (Object : in out Test) is
      DIF_Name : String := "Test DIF";
      App_Name : String := "Test App";
      Register_Result : File_Descriptor := Invalid_FD;
      RINA_Dev_FD : File_Descriptor := RINA_Open;
      Caused_Error : Boolean := False;
   begin
      Register_Result := RINA_Register (RINA_Dev_FD, DIF_Name, App_Name, 0);
      exception
         when Exceptions.DIF_Registration_Failure =>
            Caused_Error := True;
      Assert(Caused_Error, "Issue registering application name to DIF");
   end Test_Register_DIF_Register_New;
      
   procedure Test_Register_DIF_Invalid_File_Descriptor (Object : in out Test) is
   DIF_Name : String := "Test DIF";
      App_Name : String := "Test App";
      Register_Result : File_Descriptor := Invalid_FD;
      RINA_Dev_FD : File_Descriptor := -1;
      Caused_Error : Boolean := False;
   begin
      Register_Result := RINA_Register (RINA_Dev_FD, DIF_Name, App_Name, 0);
      exception
         when Exceptions.DIF_Registration_Failure =>
            Caused_Error := True;
      Assert(Caused_Error, "Invalid file descriptor passed to RINA_Register");
   end Test_Register_DIF_Invalid_File_Descriptor;

end Test_RINA_Register;
