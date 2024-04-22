with Names;                      use Names;
with Ada.Strings.Hash;
with Ada.Containers.Hashed_Maps; use Ada.Containers;

package Namespace_Manager is
    -- Possible errors
    Application_Name_Non_Existent     : exception;
    Application_Name_Already_Assigned : exception;

    -- IPCP address maps to Application name
    type Application_Name is
       new Names_String; -- The endpoint to send and be sent from

    type IPCP_Address is
       new Integer range 0 .. 255; -- IPCP address range is 1 byte

    -- Map of app names to IPCP addrs
    package IPCP_To_App is new Ada.Containers.Hashed_Maps
       (Key_Type => IPCP_Address, Element_Type => Application_Name,
        Hash     => Integer.Hash, Equivalent_Keys => "=");

    subtype IPCP_Map is
       IPCP_To_App.Map; -- so we don't have to .Map everytime created

    -- Allow getting assigned names
    -- and addresses given one or the other
    function Get_IPCP_Address
       (Self : Namespace_Manager; App_Name : Application_Name)
        return IPCP_Address;

    function Get_Application_Name
       (Self : Namespace_Manager; IPCP_Add : IPCP_Address)
        return Application_Name;

    -- Allow assigning names to IPCP's
    procedure Assign_Application_Name (App_Name : Application_Name);

private
    -- Pseudo-randomly choose IPCP address
    function Allocate_IPCP_Address return IPCP_Address;

end Namespace_Manager;
