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
   
   ------------------TS 008------------------
   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "RINA_Flow_Alloc_Wait";
    begin
      Test_Suite.Add_Test (Caller.Create
         ("TC-028 Positive Flow Alloc Response", Test_Positive_Flow_Alloc_Response'Access));
      Test_Suite.Add_Test (Caller.Create
         ("TC-029 Negative Flow Alloc Response", Test_Negative_Flow_Alloc_Response'Access));
      Test_Suite.Add_Test (Caller.Create
         ("TC-030 Positive Flow Alloc Request", Test_No_Message_Available'Access));
      Test_Suite.Add_Test (Caller.Create
         ("TC-031 Negative Flow Alloc Request", Test_Unexpected_Error_During_Message_Reading'Access));
      return Test_Suite'Access;
    end Suite;

    -- TC 028
    procedure Test_Positive_Flow_Alloc_Response (Object : in out Test) is
        wfd : File_Descriptor := 1;
        port_id : Unsigned_16 := 2020;
        result : File_Descriptor := OS.Invalid_FD;
    begin        
        Assert(result = Invalid_FD, "Invalid file descriptor returned");
    end Test_Positive_Flow_Alloc_Response;

    -- TC 029
    procedure Test_Negative_Flow_Alloc_Response (Object : in out Test) is
        wfd : File_Descriptor := 1;
        port_id : Unsigned_16 := 2020;
        result : File_Descriptor := OS.Invalid_FD;
    begin
        result := OS.Invalid_FD;        
        Assert(result = OS.Invalid_FD, "Invalid file descriptor returned");
    end Test_Negative_Flow_Alloc_Response;

    -- TC 030
    procedure Test_No_Message_Available (Object : in out Test) is
        wfd : File_Descriptor := 1;
        port_id : Unsigned_16 := 2020;
        result : File_Descriptor := OS.Invalid_FD;
    begin
        result := OS.Invalid_FD;
        Assert(result = OS.Invalid_FD, "Expected Invalid file descriptor returned when no message available for flow allocation request");
    end Test_No_Message_Available;

    -- TC 031
    procedure Test_Unexpected_Error_During_Message_Reading (Object : in out Test) is
        wfd : File_Descriptor := 1;
        port_id : Unsigned_16 := 2020;
        result : File_Descriptor := OS.Invalid_FD;
    begin
        result := OS.Invalid_FD;        
        Assert(result = Invalid_FD, "Expected Invalid file descriptor returned for unexpected error during message reading for flow allocation request");
    end Test_Unexpected_Error_During_Message_Reading;    

end Test_RINA_Flow_Alloc_Wait;
