--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Assertions;
with AUnit.Test_Caller;
with GNAT.OS_Lib;
with Bindings.Rlite.Ctrl;

package body Test_RINA_Flow_Alloc_Wait is
   use AUnit.Assertions;
   use GNAT.OS_Lib;
   use Bindings.Rlite.Ctrl;

   package OS renames GNAT.OS_Lib;

   package Caller is new AUnit.Test_Caller (Test);
   
   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "RINA_Flow_Alloc_Wait";
    begin
      Test_Suite.Add_Test (Caller.Create
         ("TC-029 Positive Flow Alloc Response", Test_Positive_Flow_Alloc_Response'Access));
      return Test_Suite'Access;
    end Suite;

    -- TC 029
    procedure Test_Positive_Flow_Alloc_Response (Object : in out Test) is
        wfd : File_Descriptor := 1;
        port_id : Unsigned_16 := 2020;
        result : File_Descriptor;
    begin
        result := RINA_Flow_Alloc_Wait(wfd, port_id);
        Assert(result /= Invalid_FD, "Invalid file descriptor returned");
    end Test_Positive_Flow_Alloc_Response;

end Test_RINA_Flow_Alloc_Wait;
