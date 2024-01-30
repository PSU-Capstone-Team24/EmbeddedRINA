--  Temp disabling
pragma Style_Checks (Off);

--  Bindings
with Bindings.Rlite.Ctrl;

--  Exceptions
with Exceptions;

package body Bindings.Rlite.API is

   --  Renames
   package Ctrl renames Bindings.Rlite.Ctrl;

   function RINA_Open return OS.File_Descriptor is
   begin
      return OS.Open_Read_Write ("/dev/rlite", OS.Binary);
   end RINA_Open;

   procedure RINA_Close (fd : OS.File_Descriptor) is
   begin
      OS.Close (fd);
   end RINA_Close;

   function RINA_Create_IPCP
     (Fd       : OS.File_Descriptor;
      Name     : String;
      DIF_Type : DIF_Types;
      DIF_Name : String) return Rl_Ipcp_Id_T
   is
   begin
      if Name'Length > Max_Length or Name'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      if DIF_Name'Length > Max_Length or DIF_Name'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      return Ctrl.RINA_Create_IPCP
          (Fd,
           To_Bounded_String (Name),
           DIF_Type,
           To_Bounded_String (DIF_Name));
   end RINA_Create_IPCP;

   -- Removes/destroys an existing IPCP by name
   procedure RINA_Destroy_IPCP (Fd : OS.File_Descriptor; Name : String) is
      Bounded_Name : constant Bounded_String := To_Bounded_String (Name);
   begin
      if Ctrl.IPCP_Map.Contains (Bounded_Name) then
         RINA_Destroy_IPCP (Fd, Ctrl.IPCP_Map (Bounded_Name));
      end if;
   end RINA_Destroy_IPCP;

   -- Removes/destroys an existing IPCP by its IPCP_Id
   -- (This can be looked up via the IPCP hashmap in Ctrl logic)
   procedure RINA_Destroy_IPCP (Fd : OS.File_Descriptor; Id : Rl_Ipcp_Id_T) is
   begin
      Ctrl.RINA_Destroy_IPCP (Fd, Id);
   end RINA_Destroy_IPCP;

   function RINA_Register
     (fd         : OS.File_Descriptor;
      dif_name   : String;
      local_appl : String;
      flags      : Integer) return OS.File_Descriptor
   is
   begin
      if dif_name'Length > Max_Length or dif_name'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      if local_appl'Length > Max_Length or local_appl'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      return Ctrl.RINA_Register_Common
          (fd,
           To_Bounded_String (dif_name),
           To_Bounded_String (local_appl),
           flags,
           1);
   end RINA_Register;

   function RINA_Unregister
     (Fd         : OS.File_Descriptor;
      Dif_Name   : String;
      Local_Appl : String;
      Flags      : Integer) return OS.File_Descriptor
   is
   begin
      if Dif_Name'Length > Max_Length or Dif_Name'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      if Local_Appl'Length > Max_Length or Local_Appl'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      return Ctrl.RINA_Register_Common
          (Fd,
           To_Bounded_String (Dif_Name),
           To_Bounded_String (Local_Appl),
           Flags,
           0);
   end RINA_Unregister;

   function RINA_Flow_Accept
     (Fd          :        OS.File_Descriptor;
      Remote_Appl : in out Bounded_String;
      Spec        :        Flow.RINA_Flow_Spec;
      Flags       :        Integer) return OS.File_Descriptor
   is
   begin
      return Ctrl.RINA_Flow_Accept (Fd, Remote_Appl, Spec, Flags);
   end RINA_Flow_Accept;

   function RINA_Flow_Alloc
     (Dif_Name    : String;
      Local_Appl  : String;
      Remote_Appl : String;
      Flowspec    : Flow.RINA_Flow_Spec;
      Flags       : Unsigned_32) return OS.File_Descriptor
   is
   begin
      if Dif_Name'Length > Max_Length or Dif_Name'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      if Local_Appl'Length > Max_Length or Local_Appl'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      if Remote_Appl'Length > Max_Length or Remote_Appl'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      return Ctrl.RINA_Flow_Alloc
          (To_Bounded_String (Dif_Name),
           To_Bounded_String (Local_Appl),
           To_Bounded_String (Remote_Appl),
           Flowspec,
           Flags,
           16#FFFF#);
   end RINA_Flow_Alloc;

   function RINA_Flow_Alloc_Wait
     (Wfd : OS.File_Descriptor) return OS.File_Descriptor
   is
   begin
      return Ctrl.RINA_Flow_Alloc_Wait (Wfd, 0);
   end RINA_Flow_Alloc_Wait;

   function RINA_Flow_Respond
     (Fd       : OS.File_Descriptor;
      Handle   : OS.File_Descriptor;
      Response : Integer) return OS.File_Descriptor
   is
   begin
      return Ctrl.RINA_Flow_Respond (Fd, Handle, Response);
   end RINA_Flow_Respond;

end Bindings.Rlite.API;
