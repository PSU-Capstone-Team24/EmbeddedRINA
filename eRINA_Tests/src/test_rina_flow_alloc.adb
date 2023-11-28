--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Assertions;
with AUnit.Test_Caller;
with GNAT.OS_Lib;
with Bindings.Rlite.Ctrl;
with Bindings.Rlite.Msg.Flow;
with Names; use Names.Name_String;


package body Test_RINA_Flow_Alloc is
   use AUnit.Assertions;
   use GNAT.OS_Lib;
   use Bindings.Rlite.Ctrl;
   use Bindings.Rlite.Msg.Flow;

   package OS renames GNAT.OS_Lib;

   package Caller is new AUnit.Test_Caller (Test);
   
   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "RINA_Flow_Alloc_Wait";
    begin
      Test_Suite.Add_Test (Caller.Create("TC-025 Valid Flow Allocation", Test_Valid_Flow_Allocation'Access));
      Test_Suite.Add_Test (Caller.Create("TC-026 Invalid Flags", Test_Invalid_Flags'Access));
      --Test_Suite.Add_Test (Caller.Create("TC-027 Incorrect Flowspec Version", Test_Incorrect_Flowspec_Version'Access));
      --Test_Suite.Add_Test (Caller.Create("TC-028 RINA Open Failure", Test_RINA_Open_Failure'Access));
      return Test_Suite'Access;
    end Suite;

   procedure Test_Valid_Flow_Allocation (Object : in out Test) is
      dif_name : Bounded_String := To_Bounded_String("TestDIF");
      local_appl : Bounded_String := To_Bounded_String("TestLocalAppl");
      remote_appl : Bounded_String := To_Bounded_String("TestRemoteAppl");
      flow_spec : Bindings.Rlite.Msg.Flow.RINA_Flow_Spec;
      result : File_Descriptor;
   begin
    -- Initialize flow_spec as needed, for example:
      flow_spec.Version := Bindings.Rlite.Msg.Flow.RINA_FLOW_SPEC_VERSION;
      flow_spec.Max_Delay := 0; -- and so on for other fields

      result := RINA_Flow_Alloc(dif_name, local_appl, remote_appl, flow_spec, 0, 0);
      Assert(result /= Invalid_FD, "Invalid file descriptor returned");
   end Test_Valid_Flow_Allocation;

   procedure Test_Invalid_Flags (Object : in out Test) is
      dif_name : Bounded_String := To_Bounded_String("TestDIF");
      local_appl : Bounded_String := To_Bounded_String("TestLocalAppl");
      remote_appl : Bounded_String := To_Bounded_String("TestRemoteAppl");
      flow_spec : Bindings.Rlite.Msg.Flow.RINA_Flow_Spec;
      result : File_Descriptor;
   begin
      result := RINA_Flow_Alloc(dif_name, local_appl, remote_appl, flow_spec, 2, 0);
      Assert(result /= Invalid_FD, "Invalid file descriptor returned");

   end Test_Invalid_Flags;


end Test_RINA_Flow_Alloc;
