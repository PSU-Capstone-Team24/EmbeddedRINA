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
   begin
    Test_Suite.Add_Test (Caller.Create
            (Name_014 & " Verify rina_unregister throws exception when DIF name empty", Test_Unregister_DIF_Name_Empty'Access));
    Test_Suite.Add_Test (Caller.Create
            (Name_015 & " Verify rina_unregister throws exception when DIF name isn't already registered", Test_Unregister_DIF_Name_Not_Registered'Access));

    return Test_Suite'Access;
   end Suite;

   --Test Case 014
   procedure Test_Unregister_DIF_Name_Empty (Object : in out Test) is
      DIF_Name : constant String := "";
      App_Name : constant String := "Test App";
      Unregister_Success : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
   begin
      
      Unregister_Success := RINA_Unregister (RINA_Dev_FD, DIF_Name, App_Name, 0);
      exception
         when Exceptions.Bounded_Length_Exception =>
            Caused_Error := True;

      Assert (Caused_Error and Unregister_Success = Invalid_FD, "DIF_Name is empty");
   end Test_Unregister_DIF_Name_Empty;

   procedure Test_Unregister_DIF_Name_Not_Registered (Object : in out Test) is
        DIF_Name : constant String := "Unique_DIF_Name";
        App_Name : constant String := "Test App";
        Unregister_Success : File_Descriptor := Invalid_FD;
        Caused_Error : Boolean := False;
   begin
       Unregister_Success := RINA_Unregister (RINA_Dev_FD, DIF_Name, App_Name, 0);
       exception
        when Exceptions.DIF_Registration_Failure =>
            Caused_Error := True;

        Assert (Caused_Error and Unregister_Success = Invalid_FD, "DIF_Name is not already registered");
    end Test_Unregister_DIF_Name_Not_Registered;

end Test_RINA_Unregister;