with Demos;
-- with CDAP;
-- with EFCP;
with IPCP_Manager; use IPCP_Manager;

procedure Demo is

   procedure Header is
   begin
      Demos.Put (0, 0, "eRINA Debug");
   end Header;

   procedure Initialize is new Demos.Initialize_Blank (Header);
begin
   Initialize;

   --  Keep board from immediately terminating
   loop
      null;
   end loop;
end Demo;