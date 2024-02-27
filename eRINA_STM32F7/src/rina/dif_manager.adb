with Debug;

package body DIF_Manager is

    Element_Not_Found : exception;

    function Get (Name : String; DIF_Type : DIF_Types) return DIF is
    begin
        for E of DIF_List loop
            if E.Name = To_Bounded_String (Name) and E.DIF_Type = DIF_Type then
                return E.all;
            end if;
        end loop;

        raise Element_Not_Found;
    end Get;

    function Get_IPCP (Name : String) return IPCP is
    begin
        for DIF of DIF_List loop
            Debug.Print (Debug.Info, "Checking local DIF: " & To_String (DIF.Name));
            for IPC_Process of DIF.IPCPs loop
                Debug.Print (Debug.Info, "Checking local IPCP: " & To_String (IPC_Process.Name));
                if To_Bounded_String (Name) = IPC_Process.Name then
                    return IPC_Process;
                end if;
            end loop; 
        end loop;

        raise Element_Not_Found;
    end Get_IPCP;
    
    function IPCP_Exists (Name : String) return Boolean is
        IPC_Process : IPCP;
    begin
        IPC_Process := Get_IPCP (Name);
        return true;
        exception
            when Element_Not_Found =>
                return false;
    end IPCP_Exists;

    function Create (Name : String; DIF_Type : DIF_Types) return DIF_Access is
        New_DIF : constant DIF_Access := new DIF;
    begin
        Debug.Print(Debug.Info, "Created local DIF: " & Name);
        New_DIF.Name := To_Bounded_String (Name);
        New_DIF.DIF_Type := DIF_Type;
        
        DIF_List.Append (New_DIF);
        return New_DIF;
    end Create;

    procedure Enroll (Self : in out DIF; IPC_Process : IPCP) is -- Flow_Req : Flow) is
    begin
        Debug.Print(Debug.Info, "Enrolling IPCP: " & To_String(IPC_Process.Name) & " into " & To_String(Self.Name));
        Self.IPCPs.Append (IPC_Process);
        Debug.Print(Debug.Info, "Enrolled IPCP: " & To_String(IPC_Process.Name) & " into " & To_String(Self.Name));
    end Enroll;

end DIF_Manager;