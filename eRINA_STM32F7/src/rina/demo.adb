with Demos;
with Net;
with CDAP;
with EFCP;
with IPCP_Manager; use IPCP_Manager;

procedure Demo is

   use Net;

   procedure Header is
   begin
      Demos.Put (0, 0, "eRINA_Debug");
   end Header;

   --  Debug only, remove later
   Demo_IPCP_Name : constant IPCP_Name.Bounded_String := IPCP_Name.To_Bounded_String ("Demo.IPCP");
   Demo_IPCP_MacAddr : constant Ether_Addr := (others => 16#ff#);
   Demo_IPCP : IPCP := IPCP_Manager.Create (Demo_IPCP_Name, Demo_IPCP_MacAddr);
begin
   Header;
end Demo;