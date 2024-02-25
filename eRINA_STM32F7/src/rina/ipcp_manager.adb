package body IPCP_Manager is
    function Create (Name : String; Executable : Task_Procedure) return IPCP is
        New_IPCP : IPCP;
    begin
        New_IPCP.Name := To_Bounded_String (Name);
        New_IPCP.Executable := Executable;
        return New_IPCP;
    end Create;
end IPCP_Manager;