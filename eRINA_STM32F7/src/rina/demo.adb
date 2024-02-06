with Demos;
-- with CDAP;
-- with EFCP;
with IPCP_Manager; use IPCP_Manager;

procedure Demo is

   procedure Header is
   begin
      Demos.Put (0, 0, "eRINA_Debug");
   end Header;

   --  Debug only, remove later
   Demo_IPCP_Name : constant IPCP_Name.Bounded_String :=
     IPCP_Name.To_Bounded_String ("Demo.IPCP");

   procedure DoStuff is
   begin
      --  I am an "application process"
      --  I run concurrently with the rest of the system and communicate over IPC
      null;
   end DoStuff;

   Demo_IPCP : IPCP := (Name => Demo_IPCP_Name, Executable => DoStuff'Unrestricted_Access, IO_Buffer => <>);
begin
   Header;
end Demo;
