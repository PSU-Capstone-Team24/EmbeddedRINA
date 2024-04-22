--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Assertions;
with AUnit.Test_Caller;
with GNAT.OS_Lib;
with Bindings.Rlite.API;
with Exceptions;

package body Test_RINA_Unregister is
   use AUnit.Assertions;
   use GNAT.OS_Lib;
   use Bindings.Rlite.API;

   Application_Name : constant String := "TestApplicationName";
   DIF_Name : constant String := "test.DIF";
   RINA_Dev_FD : constant File_Descriptor := RINA_Open;

   package Caller is new AUnit.Test_Caller (Test);
   
   -------------------------TS-003-------------------------
   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name_012 : constant String := "TC-012";
      Name_013 : constant String := "TC-013";
      Name_014 : constant String := "TC-014";
      Name_015 : constant String := "TC-015";
      Name_016 : constant String := "TC-016";
      Name_017 : constant String := "TC-017";
      Name_018 : constant String := "TC-018";
      Name_019 : constant String := "TC-019";
      Name_020 : constant String := "TC-020";
   begin
    Test_Suite.Add_Test (Caller.Create 
            (Name_012 & " Verify that rina_unregister successfully unregisters an application name from a DIF", Test_Unregister_AppName_to_DIF'Access));
    Test_Suite.Add_Test (Caller.Create
            (Name_013 & " Verify rina_unregister fails when the DIF name is too long", Test_Unregister_DIF_Name_Too_Long'Access));
    Test_Suite.Add_Test (Caller.Create
            (Name_014 & " Verify rina_unregister throws exception when DIF name is empty", Test_Unregister_DIF_Name_Empty'Access));
    Test_Suite.Add_Test (Caller.Create
            (Name_015 & " Verify rina_unregister throws exception when DIF name isn't already registered", Test_Unregister_DIF_Name_Not_Registered'Access));
    Test_Suite.Add_Test (Caller.Create
            (Name_016 & " Verify rina_unregister throws exception when application name is too long", Test_Unregister_App_Name_Too_Long'Access));
    Test_Suite.Add_Test (Caller.Create
            (Name_017 & " Verify rina_unregister throws exception when application name is empty", Test_Unregister_App_Name_Empty'Access));
   Test_Suite.Add_Test (Caller.Create
            (Name_018 & " Verify rina_unregister returns Invalid_FD when passed an invalid file descriptor for the control device", Test_Unregister_Invalid_File_Descriptor'Access));
   Test_Suite.Add_Test (Caller.Create
            (Name_019 & " Verify rina_unregister returns Invalid_App when passed an invalid App descriptor", Test_Unregister_Invalid_App_Descriptor'Access));
   Test_Suite.Add_Test (Caller.Create
            (Name_020 & " Verify that rina_unregister writes a de-registration request to the file descriptor", Test_Unregister_Writes_DeReg'Access));
         

    return Test_Suite'Access;
   end Suite;

   --Test Case 012
   procedure Test_Unregister_AppName_to_DIF (Object : in out Test) is
   Register_Success : File_Descriptor;
   Unregister_Success : File_Descriptor := Invalid_FD;
   Caused_Error : Boolean := False;
   begin
      Register_Success := RINA_Register(RINA_Dev_FD, DIF_Name, "TestAppName123", 0);
      Unregister_Success := RINA_Unregister (Register_Success, DIF_Name, "TestAppName123", 0);
      exception
         when Exceptions.DIF_Registration_Failure =>
            Caused_Error := True;
      Assert(Unregister_Success = Invalid_FD, "Successfully unregistered application from DIF");
   end Test_Unregister_AppName_to_DIF;

   --Test Case 013
   procedure Test_Unregister_DIF_Name_Too_Long (Object : in out Test) is
      DIF_Name : constant String := "this_dif_name_is__waaaaaaaaaaaaaaaaaaaaaaaaaaaaaay_too_long_and_should_throw_and_exception_when_unregistration_is_attempted_1_2_3_4_5_6_7_8";
      Caused_Error : Boolean := false;
      Unregister_Success : File_Descriptor := Invalid_FD;
   begin
      Unregister_Success := RINA_Unregister (RINA_Dev_FD, DIF_Name, Application_Name, 0);
      exception
         when Exceptions.Bounded_Length_Exception =>
            Caused_Error := True;
      Assert(Caused_Error and Unregister_Success = Invalid_FD, "DIF_Name is too long");
   end Test_Unregister_DIF_Name_Too_Long;

   --Test Case 014
   procedure Test_Unregister_DIF_Name_Empty (Object : in out Test) is
      DIF_Name : constant String := "";
      Unregister_Success : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
   begin
      Unregister_Success := RINA_Unregister (RINA_Dev_FD, DIF_Name, Application_Name, 0);
      exception
         when Exceptions.Bounded_Length_Exception =>
            Caused_Error := True;

      Assert (Caused_Error and Unregister_Success = Invalid_FD, "DIF_Name is empty");
   end Test_Unregister_DIF_Name_Empty;

   --Test Case 015
   procedure Test_Unregister_DIF_Name_Not_Registered (Object : in out Test) is
        Unique_DIF_Name : constant String := "Unique_DIF_Name";
        Unregister_Success : File_Descriptor := Invalid_FD;
        Caused_Error : Boolean := False;
   begin
       Unregister_Success := RINA_Unregister (RINA_Dev_FD, Unique_DIF_Name, Application_Name, 0);
       exception
        when Exceptions.DIF_Registration_Failure =>
            Caused_Error := True;

        Assert (Caused_Error and Unregister_Success = Invalid_FD, "DIF_Name is not already registered");
    end Test_Unregister_DIF_Name_Not_Registered;

   -- TC 016
   procedure Test_Unregister_App_Name_Too_Long (Object : in out Test) is
      Long_App_Name : constant String := "this_application_name_is__waaaaaaaaaaaaaaaaaaaaaaaaaaaaaay_too_long_and_should_throw_and_exception_when_registration_is_attempted";
      Unregister_Success : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
   begin
       Unregister_Success := RINA_Unregister (RINA_Dev_FD, DIF_Name, Long_App_Name, 0);
       exception
        when Exceptions.Bounded_Length_Exception =>
            Caused_Error := True;

        Assert (Caused_Error and Unregister_Success = Invalid_FD, "App name is too long");
   end Test_Unregister_App_Name_Too_Long;

   -- TC 017
   procedure Test_Unregister_App_Name_Empty (Object : in out Test) is
      Empty_App_Name : constant String := "";
      Unregister_Success : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
   begin
      Unregister_Success := RINA_Register (RINA_Dev_FD, DIF_Name, Empty_App_Name, 0);
      exception
         when Exceptions.Bounded_Length_Exception =>
            Caused_Error := True;

      Assert (Caused_Error and Unregister_Success = Invalid_FD, "App name is empty");
   end Test_Unregister_App_Name_Empty;

   -- TC 018
   procedure Test_Unregister_Invalid_File_Descriptor (Object : in out Test) is
      Invalid_FileD : File_Descriptor := Invalid_FD;
      Unregister_Success : File_Descriptor;
      Caused_Error : Boolean := False;
   begin
      Unregister_Success := RINA_Register (Invalid_FileD, DIF_Name, "NewTestAppName", 0);
      exception
         when Exceptions.DIF_Registration_Failure =>
            Caused_Error := True;

      Assert (Caused_Error, "No exception thrown and a valid FD was returned instead of an invalid one");
   end Test_Unregister_Invalid_File_Descriptor;

   -- TC 019
   procedure Test_Unregister_Invalid_App_Descriptor (Object : in out Test) is
      Invalid_AppD : File_Descriptor := Invalid_FD;
      Unregister_Success : File_Descriptor;
      Caused_Error : Boolean := False;
   begin
      Unregister_Success := RINA_Unregister (Invalid_AppD, DIF_Name, "NewTestAppName", 0);
      exception
         when Exceptions.DIF_Registration_Failure =>
            Caused_Error := True;

      Assert (Caused_Error, "No exception thrown and a valid FD was returned instead of an invalid one");
   end Test_Unregister_Invalid_App_Descriptor;
   
   -- TC 020
   procedure Test_Unregister_Writes_DeReg (Object : in out Test) is
      Register_Success : File_Descriptor;
      Unregister_Success : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
   begin
      Register_Success := RINA_Register(RINA_Dev_FD, DIF_Name, "TestAppName123", 0);
      Unregister_Success := RINA_Unregister (Register_Success, DIF_Name, "TestAppName123", 0);
      exception
         when Exceptions.DIF_Registration_Failure =>
            Caused_Error := True;
      Assert(Unregister_Success = Invalid_FD, "Successfully unregistered application from DIF");
   end Test_Unregister_Writes_DeReg;

end Test_RINA_Unregister;