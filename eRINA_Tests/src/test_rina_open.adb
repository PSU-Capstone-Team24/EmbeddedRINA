package body Test_RINA_Open is
   use AUnit.Test_Suites;

   -- Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   --  Test_1 : aliased Test_Case_1.Test_Case;
   --  Test_2 : aliased Test_Case_2.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      --  Add_Test (Result'Access, Test_Case_1'Access);
      --  Add_Test (Result'Access, Test_Case_2'Access);
      return Result'Access;
   end Suite;
end Test_RINA_Open;
