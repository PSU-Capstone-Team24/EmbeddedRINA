--  Temp disabling
pragma Style_Checks (Off);

package Bindings.Rlite.Msg.Register is

   -- (Application --> Kernel) to register a name.
   type Request is new Rl_Msg_Base with record
      Reg       : Unsigned_8;
      Appl_Name : Bounded_String;
      Dif_Name  : Bounded_String;
   end record;
   pragma Pack (Request);

   overriding procedure Deserialize
     (Self : in out Request; fd : OS.File_Descriptor);

   overriding function Serialize (Self : in Request) return Byte_Buffer;

   --  (Application <-- Kernel) report the result of (un)registration.
   type Response is new Rl_Msg_Base with record
      Ipcp_Id   : Rl_Ipcp_Id_T;
      Reg       : Unsigned_8;
      Response  : Unsigned_8;
      Pad1      : Unsigned_32;
      Appl_Name : Bounded_String;
   end record;
   pragma Pack (Response);

   overriding procedure Deserialize
     (Self : in out Response; fd : OS.File_Descriptor);

   overriding function Serialize (Self : in Response) return Byte_Buffer;

   --  (Application --> Kernel) to finalize a registration operation.
   type Move is new Rl_Msg_Base with record
      Ipcp_Id : Rl_Ipcp_Id_T;
      Fd      : Integer_32;
   end record;
   pragma Pack (Move);

   overriding procedure Deserialize
     (Self : in out Move; fd : OS.File_Descriptor);

   overriding function Serialize (Self : in Move) return Byte_Buffer;

end Bindings.Rlite.Msg.Register;
