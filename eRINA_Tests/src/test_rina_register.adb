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
   begin
      Test_Suite.Add_Test (Caller.Create
         (Name_003 & " Fails when DIF name too long", Test_Register_DIF_Length'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_004 & " Fails when DIF name is empty", Test_Register_DIF_Empty'Access));

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

end Test_RINA_Register;
