with Ada.Numerics.Discrete_Random;

package body Namespace_Manager is
    -- instantiate IPCP to App Names map when NSM created inside a DIF
    IPCP_To_App_Names : IPCP_Map;

    -- Get IPCP address from app name
    function Get_IPCP_Address
       (Self : Namespace_Manager; App_Name : Application_Name)
        return IPCP_Address
    is
        Curr_Name : Application_Name;
    begin
        -- for now, linear O(n)
        for IPCP in IPCP_To_App_Names.Iterate loop
            Curr_Name := IPCP_To_App_Names (IPCP);
            if Curr_Name = App_Name then
                return IPCP;
            end if;
        end loop;
        -- Make user handle error
        raise Application_Name_Non_Existent;
    end Get_IPCP_Address;

    -- Get the app name from IPCP Address
    function Get_Application_Name
       (IPCP_Addr : IPCP_Address) return Application_Name
    is
    begin
        -- desired route
        if IPCP_To_App_Names.Contains (IPCP_Addr) then
            return IPCP_To_App_Names (IPCP_Addr);
        end if;
        -- Make user handle error
        raise Application_Name_Non_Existent;
    end Get_Application_Name;

    -- store application name with corresponding ipcp address
    procedure Assign_Application_Name (App_Name : Application_Name) is
        IPCP_Addr              : IPCP_Address;
        IPCP_Assigned          : Boolean;
        Pos                    : Cursor;
        IPCP_Alloc_Retry_Count : Integer := IPCP_Address'Last + 1;
    begin
        -- allocate an unassigned IPCP Address
        IPCP_Addr := Allocate_IPCP_Address;
        -- Insert IPCP address into map
        IPCP_To_App_Names.Insert (IPCP_Addr, App_Name, Pos, IPCP_Assigned);

        -- double check that app name was actually inserted
        while IPCP_Assigned /= True and IPCP_Alloc_Retry_Count > 0 loop
            -- retry only as many times as there are addresses
            IPCP_Addr := Allocate_IPCP_Address;
            IPCP_To_App_Names.Insert (IPCP_Addr, App_Name, Pos, IPCP_Assigned);
            -- decrement counter
            IPCP_Alloc_Retry_Count := IPCP_Alloc_Retry_Count - 1;
        end loop;
    end Assign_Application_Name;

    -- helper to allocate new IPCP address
    function Allocate_IPCP_Address return IPCP_Address is
        -- Instantiate Random number with range of IPCP Addrs
        package Random_Addr is new Ada.Numerics.Discrete_Random (IPCP_Address);
        use Random_Addr;
        Gen                   : Generator;
        Addr                  : IPCP_Address;
        Addr_Already_Assigned : Boolean;
    begin
        Reset
           (Gen); -- seed random number generator with new seed every invocation
        Addr := Random (Gen); -- choose pseudo random IPCP addr number
        -- check if IPCP Addr already allocated
        Addr_Already_Assigned := IPCP_To_App_Names.Contains (Addr);
        while Addr_Already_Assigned loop
            Addr := Random (Gen); -- choose pseudo random IPCP addr number
            Addr_Already_Assigned :=
               IPCP_To_App_Names.Contains (Addr); -- check again
        end loop;
        return Addr;
    end Allocate_IPCP_Address;

end Namespace_Manager;
