--  Temp disabling
pragma Style_Checks (Off);

--  Bindings
with Bindings.Rlite.Ctrl;
with Exceptions;

package body Bindings.Rlite.API is

   --  Renames
   package Ctrl renames Bindings.Rlite.Ctrl;

   function RINA_Open return OS.File_Descriptor is
   begin
      return OS.Open_Read_Write ("/dev/rlite", OS.Binary);
   end RINA_Open;

   procedure RINA_Close(fd : OS.File_Descriptor) is
   begin
      OS.Close (fd);
   end RINA_Close;
   
   function RINA_Create_IPCP (
      Fd : OS.File_Descriptor;
      Name : String;
      DIF_Type : DIF_Types;
      DIF_Name : String) return Rl_IPCP_Id_T is
   begin
      if Name'Length > Max_Length then
         raise Exceptions.Bounded_Length_Expcetion;
      end if;

      if DIF_Name'Length > Max_Length then
         raise Exceptions.Bounded_Length_Expcetion;
      end if;

      return Ctrl.RINA_Create_IPCP (Fd, To_Bounded_String (Name), DIF_Type, To_Bounded_String (DIF_Name));
   end RINA_Create_IPCP;

   function RINA_Register (Fd : OS.File_Descriptor;
      DIF_Name : String;
      Local_Appl : String;
      Flags : Integer) return OS.File_Descriptor is
   begin
      if DIF_Name'Length > Max_Length then
         raise Exceptions.Bounded_Length_Expcetion;
      end if;

      if Local_Appl'Length > Max_Length then
         raise Exceptions.Bounded_Length_Expcetion;
      end if;

      return Ctrl.RINA_Register_Common (Fd, To_Bounded_String (DIF_Name), To_Bounded_String (Local_Appl), Flags, 1);
   end RINA_Register;

   function RINA_Unregister (Fd : OS.File_Descriptor;
      DIF_Name : String;
      Local_Appl : String;
      Flags : Integer
   ) return OS.File_Descriptor is
   begin
      if DIF_Name'Length > Max_Length then
         raise Exceptions.Bounded_Length_Expcetion;
      end if;

      if Local_Appl'Length > Max_Length then
         raise Exceptions.Bounded_Length_Expcetion;
      end if;

      return Ctrl.RINA_Register_Common (Fd, To_Bounded_String (DIF_Name), To_Bounded_String (Local_Appl), Flags, 0);
   end RINA_Unregister;

   function RINA_Flow_Accept (
      Fd           : OS.File_Descriptor;
      Remote_Appl : in out Bounded_String;
      Spec         : Flow.RINA_Flow_Spec;
      Flags        : Integer
   ) return OS.File_Descriptor is
   begin
      return Ctrl.RINA_Flow_Accept(Fd, Remote_Appl, Spec, Flags);
   end RINA_Flow_Accept;

   function RINA_Flow_Alloc(
      DIF_Name       : String;
      Local_Appl     : String;
      Remote_Appl    : String;
      Flowspec       : Flow.RINA_Flow_Spec;
      Flags          : Unsigned_32
   )  return OS.File_Descriptor is
   begin
      return Ctrl.RINA_Flow_Alloc(To_Bounded_String (DIF_Name), To_Bounded_String (Local_Appl), To_Bounded_String (Remote_Appl), Flowspec, Flags, 16#FFFF#);
   end RINA_Flow_Alloc;

   function RINA_Flow_Alloc_Wait(
      Wfd            : OS.File_Descriptor 
   )  return OS.File_Descriptor is
   begin
      return Ctrl.RINA_Flow_Alloc_Wait(Wfd, 0);
   end RINA_Flow_Alloc_Wait;

   function RINA_Flow_Respond(
      fd       : OS.File_Descriptor;
      handle   : OS.File_Descriptor;
      response : Integer
   ) return OS.File_Descriptor is
   begin
      return Ctrl.RINA_Flow_Respond(fd, handle, response);
   end RINA_Flow_Respond;
   
end Bindings.Rlite.API;