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
   
   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name_014 : constant String := "TC-014";
      Name_015 : constant String := "TC-015";
      Name_016 : constant String := "TC-016";
      Name_017 : constant String := "TC-017";
   begin
    Test_Suite.Add_Test (Caller.Create
            (Name_014 & " Verify rina_unregister throws exception when DIF name is empty", Test_Unregister_DIF_Name_Empty'Access));
    Test_Suite.Add_Test (Caller.Create
            (Name_015 & " Verify rina_unregister throws exception when DIF name isn't already registered", Test_Unregister_DIF_Name_Not_Registered'Access));
    Test_Suite.Add_Test (Caller.Create
            (Name_016 & " Verify rina_unregister throws exception when application name is too long", Test_Unregister_App_Name_Too_Long'Access));
    Test_Suite.Add_Test (Caller.Create
            (Name_017 & " Verify rina_unregister throws exception when application name is empty", Test_Unregister_App_Name_Empty'Access));

    return Test_Suite'Access;
   end Suite;

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

end Test_RINA_Unregister;