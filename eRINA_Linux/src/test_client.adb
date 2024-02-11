--  Temp disabling
pragma Style_Checks (Off);

--  Ada
with Ada.Text_IO;

--  Rlite Bindings
with Bindings.Rlite.API; use Bindings.Rlite.API;

with Bindings.Rlite.Msg.Flow; use Bindings.Rlite.Msg;

with Exceptions;
with Debug;

with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure Test_Client is
   package Text_IO renames Ada.Text_IO;

   --  Placeholders for file descriptor values, assume invalid (< 0)
   RINA_Dev_FD   : GNAT.OS_Lib.File_Descriptor;
   Alloc_Success : GNAT.OS_Lib.File_Descriptor;

   Server_Name      : String (1 .. 128) := (others => ASCII.NUL);
   Server_Name_Last : Integer;

   Client_Name      : String (1 .. 128) := (others => ASCII.NUL);
   Client_Name_Last : Integer;

   DIF_Name      : String (1 .. 128) := (others => ASCII.NUL);
   DIF_Name_Last : Integer;
begin
   Text_IO.Put_Line ("Starting RINA server application....");
   RINA_Dev_FD := RINA_Open;

   if RINA_Dev_FD = Invalid_FD then
      Debug.Print ("Client", "Error opening RINA control device", Debug.Error);
      raise Exceptions.RINA_Control_Failure;
   else
      Text_IO.Put_Line
        ("Successfully opened RINA control device (File Desc: " &
         GNAT.OS_Lib.File_Descriptor'Image (RINA_Dev_FD) & ")");
   end if;

   Ada.Text_IO.Put ("Enter name of client (this) application: ");
   Ada.Text_IO.Get_Line (Client_Name, Client_Name_Last);

   Ada.Text_IO.Put ("You typed in the string: ");
   Ada.Text_IO.Put_Line (Client_Name);

   Ada.Text_IO.Put ("Enter name of server application: ");
   Ada.Text_IO.Get_Line (Server_Name, Server_Name_Last);

   Ada.Text_IO.Put ("You typed in the string: ");
   Ada.Text_IO.Put_Line (Server_Name);

   Ada.Text_IO.Put
     ("Enter name of DIF to register '" & Client_Name & "' to: ");
   Ada.Text_IO.Get_Line (DIF_Name, DIF_Name_Last);

   Ada.Text_IO.Put ("You typed in the string: ");
   Ada.Text_IO.Put_Line (DIF_Name);

   --  Attempt a flow allocation request
   declare
      QoS_Parameters : Flow.RINA_Flow_Spec;
   begin
      Ada.Text_IO.Put_Line ("Attempting to allocate a flow...");
      Alloc_Success :=
        RINA_Flow_Alloc
          (DIF_Name, Client_Name, Server_Name, QoS_Parameters, 0);
   end;

   if Alloc_Success = Invalid_FD then
      Debug.Print ("Client", "Error allocating flow!", Debug.Error);
      RINA_Close (RINA_Dev_FD);
      raise Exceptions.Flow_Alloc_Failure;
   end if;

end Test_Client;
