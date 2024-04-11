--  Temp disabling
pragma Style_Checks (Off);

--  Bindings
with Bindings.Rlite.Ctrl;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Streams;
use type Ada.Streams.Stream_Element_Count;

--  Exceptions
with Ada.Exceptions; use Ada.Exceptions;
with Buffers;        use Buffers;
with Exceptions;
with Debug;

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

   procedure RINA_Write
     (Fd : OS.File_Descriptor; Addr : System.Address; Bytes : Natural)
   is
      Bytes_Written : Integer;
   begin
      Bytes_Written := OS.Write (Fd, Addr, Bytes);

      if Bytes_Written < 0 then
         Debug.Print ("RINA_Write", "Error writing bytes", Debug.Error);
      end if;

   end RINA_Write;

   function RINA_Create_IPCP
     (Fd       : OS.File_Descriptor; Name : String; DIF_Type : DIF_Types;
      DIF_Name : String) return Rl_Ipcp_Id_T
   is
   begin
      if Name'Length > Max_Length or Name'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      if DIF_Name'Length > Max_Length or DIF_Name'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      return
        Ctrl.RINA_Create_IPCP
          (Fd, To_Bounded_String (Name), DIF_Type,
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

   procedure RINA_Config_IPCP
     (Fd    : OS.File_Descriptor; Id : Rl_Ipcp_Id_T; Name : String;
      Value : String)
   is
      Name_Str  : constant Bounded_String := To_Bounded_String (Name);
      Value_Str : constant Bounded_String := To_Bounded_String (Value);
   begin
      Ctrl.RINA_Config_IPCP (Fd, Id, Name_Str, Value_Str);
   end RINA_Config_IPCP;

   procedure RINA_Enroll_IPCP
     (Fd       : OS.File_Descriptor; IPCP_Name : String; Neigh_Name : String;
      DIF_Name : String; Supp_DIF_Name : String)
   is
   begin
      Ctrl.RINA_Enroll_IPCP
        (Fd, To_Bounded_String (IPCP_Name), To_Bounded_String (Neigh_Name),
         To_Bounded_String (DIF_Name), To_Bounded_String (Supp_DIF_Name));
   end RINA_Enroll_IPCP;

   function RINA_Register
     (fd    : OS.File_Descriptor; dif_name : String; local_appl : String;
      flags : Integer) return OS.File_Descriptor
   is
   begin
      if dif_name'Length > Max_Length or dif_name'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      if local_appl'Length > Max_Length or local_appl'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      return
        Ctrl.RINA_Register_Common
          (fd, To_Bounded_String (dif_name), To_Bounded_String (local_appl),
           flags, 1);
   end RINA_Register;

   function RINA_Unregister
     (Fd    : OS.File_Descriptor; Dif_Name : String; Local_Appl : String;
      Flags : Integer) return OS.File_Descriptor
   is
   begin
      if Dif_Name'Length > Max_Length or Dif_Name'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      if Local_Appl'Length > Max_Length or Local_Appl'Length = 0 then
         raise Exceptions.Bounded_Length_Exception;
      end if;

      return
        Ctrl.RINA_Register_Common
          (Fd, To_Bounded_String (Dif_Name), To_Bounded_String (Local_Appl),
           Flags, 0);
   end RINA_Unregister;

   function RINA_Flow_Accept
     (Fd   : OS.File_Descriptor; Remote_Appl : in out Bounded_String;
      Spec : Flow.RINA_Flow_Spec; Flags : Integer) return OS.File_Descriptor
   is
   begin
      return Ctrl.RINA_Flow_Accept (Fd, Remote_Appl, Spec, Flags);
   end RINA_Flow_Accept;

   function RINA_Flow_Alloc
     (Dif_Name : String; Local_Appl : String; Remote_Appl : String;
      Flowspec : Flow.RINA_Flow_Spec; Flags : Unsigned_32)
      return OS.File_Descriptor
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

      return
        Ctrl.RINA_Flow_Alloc
          (To_Bounded_String (Dif_Name), To_Bounded_String (Local_Appl),
           To_Bounded_String (Remote_Appl), Flowspec, Flags, 16#FFFF#);
   end RINA_Flow_Alloc;

   function RINA_Flow_Alloc_Wait
     (Wfd : OS.File_Descriptor) return OS.File_Descriptor
   is
   begin
      return Ctrl.RINA_Flow_Alloc_Wait (Wfd);
   end RINA_Flow_Alloc_Wait;

   function RINA_Flow_Respond
     (Fd : OS.File_Descriptor; Handle : OS.File_Descriptor; Response : Integer)
      return OS.File_Descriptor
   is
   begin
      return Ctrl.RINA_Flow_Respond (Fd, Handle, Response);
   end RINA_Flow_Respond;

   procedure Register_UIPCPS (DIF_Name : String; IPCP_Name : String) is
      Client  : Socket_Type;
      Address : Sock_Addr_Type;
      Channel : Stream_Access;
      Buffer  : Byte_Buffer :=
        (16#08#, 16#00#, 16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#01#, 16#43#, 16#48#, 16#17#, 16#e6#, 16#06#, 16#25#, 16#fb#,
         16#06#, 16#00#, 16#61#, 16#2E#, 16#49#, 16#50#, 16#43#, 16#50#,
         16#09#, 16#00#, 16#65#, 16#74#, 16#68#, 16#41#, 16#42#, 16#2E#,
         16#44#, 16#49#, 16#46#);
      Data : Ada.Streams.Stream_Element_Array (1 .. Buffer'Length) with
        Address => Buffer'Address;
   begin
      begin
         delay 1.0;
         Create_Socket (Client);
         Address.Addr := Inet_Addr ("127.0.0.1");
         Address.Port := 6_220;
         Connect_Socket (Client, Address);
         Channel := Stream (Client);
         Ada.Streams.Write (Channel.all, Data);
         delay 1.0;
      exception
         when E : others =>
            Debug.Print ("Connect_UIPCPS", Exception_Message (E), Debug.Error);
      end;
   end Register_UIPCPS;

end Bindings.Rlite.API;
