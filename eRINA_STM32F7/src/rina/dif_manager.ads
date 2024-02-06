with Ada.Strings.Hash;
with Ada.Strings.Bounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Net;
with IPCP_Manager; use IPCP_Manager;

package DIF_Manager is
   use type Net.Uint8;
   use type Net.Ether_Addr;

   --  Max of 32 IPCs can be registered
   MAX_IPC_COUNT        : constant Natural := 32;

   type DIF is record
      IPCPs : array(0 .. MAX_IPC_COUNT);
   end record;


   --  Enrollment is the procedure by which an IPCP joins an existing DIF and is initialized
   --  with enough information to become a fully operational DIF member. 
   procedure Enroll (IPCP : IPCP; Flow : Flow) is null;

end DIF_Manager;
