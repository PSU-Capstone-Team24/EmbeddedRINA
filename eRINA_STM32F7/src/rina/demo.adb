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
      Demos.Put (0, 0, "eRINA_Debug");
   end Header;

   --  Max of 32 IPCs can be registered
   MAX_IPC_COUNT : constant Natural := 32;
   MAX_IPCP_NAME_LENGTH : constant Natural := 128;

   package IPCP_String is new Ada.Strings.Bounded.Generic_Bounded_Length (MAX_IPCP_NAME_LENGTH);
   use IPCP_String;

   function Hash(Name : in Bounded_String) return Ada.Containers.Hash_Type is begin
       return Ada.Strings.Hash(To_String (Name));
   end Hash;

   --  Eventually make this the protected object that holds the IPC database
   package IPCP_MAC_Map is new Ada.Containers.Indefinite_Hashed_Maps (
       Key_Type => Bounded_String,
       Element_Type => Ether_Addr,
       Hash => Hash,
       Equivalent_Keys => "="
   );
   
   use IPCP_MAC_Map;

   --  Instantiate IPCP map
   IPCP_Map : Map;
  
   --  Debug only, remove later
   Demo_IPCP_Name : constant Bounded_String := To_Bounded_String ("Demo.IPCP");
   Demo_IPCP_MacAddr : constant Ether_Addr := (others => 16#ff#);
begin
   Header;
   IPCP_Map.Include (Demo_IPCP_Name, Demo_IPCP_MacAddr);
end Demo;