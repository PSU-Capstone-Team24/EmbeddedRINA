--  Temp disabling
pragma Style_Checks (Off);

--  Ada
with Ada.Text_IO;
   
--  Rlite Bindings
with Bindings.Rlite.API;
   use Bindings.Rlite.API;

with Names;
   use Names.Name_String;

with Exceptions;

with GNAT.OS_Lib;
   use GNAT.OS_Lib;

procedure Test_Server is
   package Text_IO renames Ada.Text_IO;

   --  Placeholders for file descriptor values, assume invalid (< 0)
   RINA_Dev_FD : OS.File_Descriptor;
   Register_Success : OS.File_Descriptor;

   Application_Name : String(1 .. 128) := (others => ASCII.NUL);
   Application_Name_Last : Integer;

   DIF_Name : String(1 .. 128) := (others => ASCII.NUL);
   DIF_Name_Last : Integer;
begin
   Text_IO.Put_Line ("Starting RINA server application....");
   RINA_Dev_FD := RINA_Open;

   if RINA_Dev_FD = Invalid_FD then
      Text_IO.Put_Line ("Error opening RINA control device");
      raise Exceptions.RINA_Control_Failure;
   else
      Text_IO.Put_Line ("Successfully opened RINA control device (File Desc: " & OS.File_Descriptor'Image (RINA_Dev_FD) & ")");
   end if;

   Ada.Text_IO.Put ("Enter name of server application to register: ");
   Ada.Text_IO.Get_Line (Application_Name, Application_Name_Last);

   Ada.Text_IO.Put ("You typed in the string: ");
   Ada.Text_IO.Put_Line (Application_Name);

   Ada.Text_IO.Put ("Enter name of DIF to register '" & Application_Name & "' to: ");
   Ada.Text_IO.Get_Line (DIF_Name, DIF_Name_Last);

   Ada.Text_IO.Put ("You typed in the string: ");
   Ada.Text_IO.Put_Line (DIF_Name);

   Register_Success := RINA_Register (RINA_Dev_FD, To_Bounded_String (DIF_Name), To_Bounded_String (Application_Name), 0);
   
   if Register_Success = Invalid_FD then
      Text_IO.Put_Line ("Error registering application " & Application_Name & " to " & DIF_Name);
      raise Exceptions.DIF_Registration_Failure;
   else
      Text_IO.Put_Line ("Successfully registered application " & Application_Name & " to " & DIF_Name);
   end if;

   loop
      --  Do nothing for now so the server stays registered
      null;
   end loop;

end Test_Server;