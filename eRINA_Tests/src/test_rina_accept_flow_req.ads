--  Temp disabling
pragma Style_Checks (Off);

with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Test_RINA_ACCEPT_FLOW_REQ is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;    
    procedure Test_RINA_ACCEPT_FLOW_REQ (Object : in out Test);   
    procedure Test_RINA_ACCEPT_FLOW_REQ_Invalid_Flag (Object : in out Test);    
    procedure Test_RINA_ACCEPT_FLOW_REQ_Invalid_FD (Object : in out Test);    
    procedure Test_RINA_ACCEPT_FLOW_REQ_No_Memory (Object : in out Test);    
    procedure Test_RINA_ACCEPT_FLOW_REQ_Flag_2048 (Object : in out Test);    
    procedure Test_RINA_ACCEPT_FLOW_REQ_Valid_FD (Object : in out Test);

end Test_RINA_ACCEPT_FLOW_REQ;