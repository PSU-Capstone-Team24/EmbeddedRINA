package body EFCP.Abstr.DTP is

    function Get_Dest_Addr (Self : in out PDU) return IPCP_Network_Address is
    begin
        return Decode (PDU.Dest_Addr);
    end Get_Dest_Addr;

    function Get_Send_Addr (Self : in out PDU) return IPCP_Network_Address is
    begin
        return Decode (PDU.Send_Addr);
    end Get_Send_Addr;

    -- QoS Cubes have corresponding IDs
    -- QoS Cubes are classes which support specific QoS metrics/standards and are
    -- offerred by a DIF
    function Get_QoS_ID (Self : in out PDU) return QoS_ID is
    begin
        return Decode (PDU.QoS_ID);
    end Get_QoS_ID;

    function Get_Version (Self : in out PDU) return EFCP_Version is
    begin
        return Decode (PDU.Version);
    end Get_Version;

    -- CEPID is the identifier of an EFCP instance
    -- EFCP is internal to an IPCP, with 1 instance per SDU flow
    function Get_Dest_CEPID (Self : in out PDU) return EFCP_CEPID is
    begin
        return Decode (PDU.Dest_CEPID);
    end Get_Dest_CEPID;

    function Get_Src_CEPID (Self : in out PDU) return EFCP_CEPID is
    begin
        return Decode (PDU.Src_CEPID);
    end Get_Src_CEPID;

    -- this can be DTP or DTCP
    function Get_PDU_Type (Self : in out PDU) return PDU_Type is
    begin
        return Decode (PDU.PDU_Type);
    end Get_PDU_Type;

    -- This changes how PDU is processed
    function Get_Flags (Self : in out PDU) return PDU_Flags is
    begin
        return Decode (PDU.Flags);
    end Get_Flags;

    function Get_Seq_Num (Self : in out PDU) return Seq_Num is
    begin
        return Decode (PDU.Seq_Num);
    end Get_Seq_Num;

    -- returns raw user data
    function Get_User_Data (Self : in out PDU) return User_Data is
    begin
        return Decode (PDU.User_Data);
    end Get_User_Data;

    -- returns length in bytes
    function Get_PDU_Length (Self : in out PDU) return PDU_Length is
    begin
        return Decode (PDU.Length);
    end Get_PDU_Length;
end EFCP.Abstr.DTP;
