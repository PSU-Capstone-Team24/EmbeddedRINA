with Ada.Strings.Hash;
with Ada.Strings.Bounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Net;

package IPCP_Manager is
   use type Net.Uint8;
   use type Net.Ether_Addr;

   --  Max of 32 IPCs can be registered
   MAX_IPC_COUNT : constant Natural := 32;
   MAX_IPCP_NAME_LENGTH : constant Natural := 128;

   package IPCP_Name is
      new Ada.Strings.Bounded.Generic_Bounded_Length (MAX_IPCP_NAME_LENGTH);

   use IPCP_Name;

   function Hash(Name : in Bounded_String) return Ada.Containers.Hash_Type;

   --  Eventually make this the protected object that holds the IPC database
   package IPCP_MAC_Map is new Ada.Containers.Indefinite_Hashed_Maps (
       Key_Type => Bounded_String,
       Element_Type => Net.Ether_Addr,
       Hash => Hash,
       Equivalent_Keys => "="
   );
   
   --  Instantiate IPCP map
   IPCP_Map : IPCP_MAC_Map.Map;
   
   type Data_Buffer is array(1 .. 128) of Net.Uint8;

   type IPCP is tagged record
      Name : Bounded_String;
      IO_Buffer : Data_Buffer := (others => 0);
   end record;

   function Create (Name : Bounded_String; MAC_Addr : Net.Ether_Addr) return IPCP;

end IPCP_Manager;