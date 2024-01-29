package body IPCP_Manager is

   function Hash (Name : in Bounded_String) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (To_String (Name));
   end Hash;

   function Create
     (Name : Bounded_String; MAC_Addr : Net.Ether_Addr) return IPCP
   is
      New_IPCP : IPCP;
   begin
      New_IPCP.Name := Name;
      IPCP_Map.Insert (Name, MAC_Addr);

      return New_IPCP;
   end Create;

end IPCP_Manager;
