with Demos;
with Net;
with Ada.Strings.Hash;
with Ada.Strings.Bounded;
with Ada.Containers.Indefinite_Hashed_Maps;

procedure Demo is

   --  Mac address type
   use Net;

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