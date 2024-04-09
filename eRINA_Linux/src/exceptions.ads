package Exceptions is
   Unexpected_Flow_Failure   : exception;
   DIF_Registration_Failure  : exception;
   RINA_Control_Failure      : exception;
   Flow_Alloc_Failure        : exception;
   Flow_Acceptance_Failure   : exception;
   Not_Implemented_Exception : exception;
   IPCP_Creation_Exception   : exception;
   Bounded_Length_Exception  : exception;
   Deserialize_Failure       : exception;
end Exceptions;