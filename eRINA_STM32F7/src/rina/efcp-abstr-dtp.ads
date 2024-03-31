use Buffers;
with Buffers;

package EFCP.Abstr.DTP is
   -- derive from these, placing size constraints as your DIF requires
   type Network_Address_Size is new Natural;
   type QoS_Size is new Natural;
   type CEPID_Size is new Natural;
   type PDU_Length is new Natural;
   type Seq_Num_Size is new Natural;
   type User_Data_Size is new Natural;

   -- PDU abstract syntax definition per:
   -- ETSI GR NGP 009 V1.1.1 (2019-02)
   --  Next Generation Protocols (NGP);
   --  An example of a non-IP network protocol architecture
   --  based on RINA design principles
   -- https://www.etsi.org/deliver/etsi_gr/NGP/001_099/009/01.01.01_60/gr_ngp009v010101p.pdf
   type PDU is abstract tagged record
      -- all elements are stored and shown in raw format
      -- they are encoded as binary
      Version    : Byte; -- EFCP version
      Dest_Addr  : Network_Address_Size; -- IPC Process address
      Send_Addr  : Network_Address_Size; -- IPC Process address
      QoS_ID : QoS_Size; -- Id of QoS Cube (services supporting requested QoS params)
      Dest_CEPID : CEPID_Size; -- ID of the destination endpoint EFCP instance
      Src_CEPID  : CEPID_Size; -- ID of the source endpoint EFCP instance
      PDU_Type   : Byte; -- the type of PDU sent (DTP vs DTCP)
      Flags      : Byte; -- alters processing of PDU
      Length     : PDU_Length; -- number of total bytes in this PDU
      Seq_Num    : Seq_Num_Size; -- seq number of this PDU
      User_Data : User_Data_Size; -- SDU fragments or whole SDUs - only decoded by application itself
   end record;

   -- these are all generic types to provide flexibility
   -- for how the implementer decides to specify each data type.
   -- NOTE: these represent the decoded values (decoded from binary)
   -- e.g. alphanumeric, numeric, emojis, only punctuation symbols, etc.
   --
   -- decoder functions follow each generic.
   -- they take binary raw from PDU and return a decoded value
   -- you need to implement these yourself for your designed PDU implementation
   generic
      type IPCP_Network_Address is
        private; -- "is private" means you can map any type to it
      with function Decode
        (Self : in out PDU) return IPCP_Network_Address; -- use inside Getters
   -- the Address of an IPC Process (IPCP)
   function Get_Dest_Addr (Self : in out PDU) return IPCP_Network_Address;
   function Get_Send_Addr (Self : in out PDU) return IPCP_Network_Address;

   generic
      type QoS_ID is private;
   -- QoS Cubes have corresponding IDs
   -- QoS Cubes are classes which support specific QoS metrics/standards and are
   -- offerred by a DIF
   function Get_QoS_ID (Self : in out PDU) return QoS_ID;

   generic
      type EFCP_Version is private;
   function Get_Version (Self : in out PDU) return EFCP_Version;

   generic
      type EFCP_CEPID is private;
   -- CEPID is the identifier of an EFCP instance
   -- EFCP is internal to an IPCP, with 1 instance per SDU flow
   function Get_Dest_CEPID (Self : in out PDU) return EFCP_CEPID;
   function Get_Src_CEPID (Self : in out PDU) return EFCP_CEPID;

   generic
      type PDU_Type is private;
   -- this can be DTP or DTCP
   function Get_PDU_Type (Self : in out PDU) return PDU_Type;

   generic
      type PDU_Flags is private;
   -- This changes how PDU is processed
   function Get_Flags (Self : in out PDU) return PDU_Flags;

   generic
      type Seq_Num is private;
   function Get_Seq_Num (Self : in out PDU) return Seq_Num;

   generic
      type User_Data is private;
   -- returns raw user data
   function Get_User_Data (Self : in out PDU) return User_Data;

   -- returns length in bytes
   function Get_PDU_Length (Self : in out PDU) return PDU_Length;
end EFCP.Abstr.DTP;
