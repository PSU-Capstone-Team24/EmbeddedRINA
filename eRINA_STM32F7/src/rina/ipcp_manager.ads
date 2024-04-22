with Ada.Strings.Bounded;
with Ada.Containers.Vectors;
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

   function Create (Name : String) return IPCP;

   --  TBD ::
   --  Port Id exists here so that we can have the same application names
   --  The port identifier allows us to address individual instances of the same application
   type Flow is record
      Source_Application      : Bounded_String;
      Source_Port_Id          : Net.Uint8;
      Destination_Application : Bounded_String;
      Destination_Port_Id     : Net.Uint8;
   end record;

   --  Stores all DIFs in the system
   package IPCP_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => IPCP);

   subtype IPCP_Vector is IPCP_Vectors.Vector;

end IPCP_Manager;
