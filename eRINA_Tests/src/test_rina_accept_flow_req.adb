--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Assertions;
with AUnit.Test_Caller;
with GNAT.OS_Lib;
with Bindings.Rlite.API;
with Exceptions;

package body Test_RINA_ACCEPT_FLOW_REQ is
   use AUnit.Assertions;
   use GNAT.OS_Lib;
   use Bindings.Rlite.API;

   Application_Name : constant String := "TestApplicationName";
   DIF_Name : constant String := "test.DIF";
   RINA_Dev_FD : constant File_Descriptor := RINA_Open;
   Rlite_Fd : File_Descriptor := Invalid_FD;


   
   package Caller is new AUnit.Test_Caller (Test);
   
   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   ------------------TS 005------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name_036 : constant String := "TC-036";
      Name_037 : constant String := "TC-037";
      Name_038 : constant String := "TC-038";
      Name_039 : constant String := "TC-039";
      Name_040 : constant String := "TC-040";
      Name_041 : constant String := "TC-041";
      Name_042 : constant String := "TC-042";
   begin
      Test_Suite.Add_Test (Caller.Create
         (Name_036 & " Test RINA ACCEPT FLOW REQ", Test_RINA_ACCEPT_FLOW_REQ'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_037 & " Test RINA ACCEPT FLOW REQ Invalid Flag", Test_RINA_ACCEPT_FLOW_REQ_Invalid_Flag'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_038 & " Test RINA ACCEPT FLOW REQ Invalid FD", Test_RINA_ACCEPT_FLOW_REQ_Invalid_FD'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_039 & " Test RINA ACCEPT FLOW REQ No Memory", Test_RINA_ACCEPT_FLOW_REQ_No_Memory'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_040 & " Test RINA ACCEPT FLOW REQ Flag 2048", Test_RINA_ACCEPT_FLOW_REQ_Flag_2048'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_041 & " Test RINA ACCEPT FLOW REQ Valid FD", Test_RINA_ACCEPT_FLOW_REQ_Valid_FD'Access));
      Test_Suite.Add_Test (Caller.Create
         (Name_042 & " Test RINA ACCEPT FLOW REQ Valid Pointer", Test_RINA_ACCEPT_FLOW_REQ_Valid_Pointer'Access));

      return Test_Suite'Access;
   end Suite;

   -- TC 036
   procedure Test_RINA_ACCEPT_FLOW_REQ (Object : in out Test) is
      Rlite_Fd : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
   begin
      Rlite_Fd  := RINA_Register (RINA_Dev_FD, "", Application_Name, 0);
      exception
         when Exceptions.Bounded_Length_Exception =>
            Caused_Error := True;

      Assert(Caused_Error and Rlite_Fd = Invalid_FD, "Invalid file descriptor returned");
   end Test_RINA_ACCEPT_FLOW_REQ;

   -- TC 037
   procedure Test_RINA_ACCEPT_FLOW_REQ_Invalid_Flag (Object : in out Test) is
       Rlite_Fd  : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
   begin
      Rlite_Fd  := RINA_Register (RINA_Dev_FD, "", Application_Name, 0);
      exception
         when Exceptions.Bounded_Length_Exception =>
            Caused_Error := True;

      Assert(Caused_Error and Rlite_Fd = Invalid_FD, "Device opened that should not be reachable");
   end Test_RINA_ACCEPT_FLOW_REQ_Invalid_Flag;

   -- TC 038
   procedure Test_RINA_ACCEPT_FLOW_REQ_Invalid_FD (Object : in out Test) is
      Rlite_Fd  : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
   begin
      Rlite_Fd  := RINA_Register (RINA_Dev_FD, "", Application_Name, 0);
      exception
         when Exceptions.Bounded_Length_Exception =>
            Caused_Error := True;

      Assert(Caused_Error and Rlite_Fd = Invalid_FD, "Device opened that should not be reachable");
   end Test_RINA_ACCEPT_FLOW_REQ_Invalid_FD;

   -- TC 039
   procedure Test_RINA_ACCEPT_FLOW_REQ_No_Memory (Object : in out Test) is
      Rlite_Fd  : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
   begin
      Rlite_Fd  := RINA_Register (RINA_Dev_FD, "", Application_Name, 0);
      exception
         when Exceptions.Bounded_Length_Exception =>
            Caused_Error := True;

      Assert(Caused_Error and Rlite_Fd = Invalid_FD, "Device opened that should not be reachable");
   end Test_RINA_ACCEPT_FLOW_REQ_No_Memory;

   -- TC 040
   procedure Test_RINA_ACCEPT_FLOW_REQ_Flag_2048 (Object : in out Test) is
      Rlite_Fd  : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
   begin
      Rlite_Fd  := RINA_Register (RINA_Dev_FD, "", Application_Name, 0);
      exception
         when Exceptions.Bounded_Length_Exception =>
            Caused_Error := True;

      Assert(Caused_Error and Rlite_Fd = Invalid_FD, "Invalid file descriptor returned");
   end Test_RINA_ACCEPT_FLOW_REQ_Flag_2048;

   -- TC 041
   procedure Test_RINA_ACCEPT_FLOW_REQ_Valid_FD (Object : in out Test) is
      Rlite_Fd  : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
   begin
      Rlite_Fd  := RINA_Register (RINA_Dev_FD, "", Application_Name, 0);
      exception
         when Exceptions.Bounded_Length_Exception =>
            Caused_Error := True;

      Assert(Caused_Error and Rlite_Fd = Invalid_FD, "Invalid file descriptor returned");
    end Test_RINA_ACCEPT_FLOW_REQ_Valid_FD;

   -- TC 042
   procedure Test_RINA_ACCEPT_FLOW_REQ_Valid_Pointer (Object : in out Test) is
      Rlite_Fd  : File_Descriptor := Invalid_FD;
      Caused_Error : Boolean := False;
   begin
      Rlite_Fd  := RINA_Register (RINA_Dev_FD, "", Application_Name, 0);
      exception
         when Exceptions.Bounded_Length_Exception =>
            Caused_Error := True;

      Assert(Caused_Error and Rlite_Fd = Invalid_FD, "Invalid file descriptor returned");
   end Test_RINA_ACCEPT_FLOW_REQ_Valid_Pointer;

end Test_RINA_ACCEPT_FLOW_REQ;