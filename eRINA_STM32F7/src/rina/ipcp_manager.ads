with Ada.Strings.Bounded;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Net;

package IPCP_Manager is

   MAX_IPCP_NAME_LENGTH : constant Natural := 128;

   package IPCP_Name is new Ada.Strings.Bounded.Generic_Bounded_Length
     (MAX_IPCP_NAME_LENGTH);

   use IPCP_Name;

   type Data_Buffer is array (1 .. 128) of Net.Uint8;

   type Task_Procedure is access procedure;

   type IPCP is tagged record
      Name       : Bounded_String;
      Executable : Task_Procedure;
      IO_Buffer  : Data_Buffer := (others => 0);
   end record;

   --  TBD ::
   --  Port Id exists here so that we can have the same application names
   --  The port identifier allows us to address individual instances of the same application
   type Flow is record
      Source_Application : Unbounded_String;
      Source_Port_Id : Net.Uint8;
      Destination_Application : Unbounded_String;
      Destination_Port_Id : Net.Uint8;
   end record;

end IPCP_Manager;
