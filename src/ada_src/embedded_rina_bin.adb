--  Temp disabling
pragma Style_Checks (Off);

with System;

--  Ada
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

--  Rlite Bindings
with Bindings.Rlite.API; use Bindings.Rlite.API;
with Exceptions;

procedure Embedded_Rina_Bin is
   package Text_IO renames Ada.Text_IO;

   --  Placeholders for file descriptor values, assume invalid (< 0)
   RINA_Dev_FD : OS.File_Descriptor;
   Register_Success : OS.File_Descriptor;
   
   -- TODO: Move me eventually
   -- Credit: Olivier Henley
   function To_Str (Addr : System.Address) return String is
        type C_String_T is access all String (1 .. Integer'Last);
        function To_C_String_T is new Ada.Unchecked_Conversion (System.Address, C_String_T);
        Ptr : constant C_String_T := To_C_String_T (Addr);
        C   : Integer := 0;
    begin
         --  Loop through each character in the string and stop once we've hit the null terminator
        for I in Ptr'Range loop
            C := I;
            exit when Ptr (I) = ASCII.NUL;
        end loop;
        --  Give Ptr the range of the string in memory
        return Ptr (1 .. C-1);
    end;

   --  Hardcoded for testing purposes
   Application_Name : String (1 .. 20);
   DIF_Name : String (1 .. 20);
   Application_Name_Length : Integer;
   DIF_Name_Length : Integer;
begin
   Text_IO.Put_Line ("Starting RINA server application....");
   RINA_Dev_FD := RINA_Open;

   if Integer(RINA_Dev_FD) < 0 then
      Text_IO.Put_Line ("Error opening RINA control device");
      raise Exceptions.RINA_Control_Failure;
   else
      Text_IO.Put_Line ("Successfully opened RINA control device (File Desc: " & OS.File_Descriptor'Image (RINA_Dev_FD) & ")");
   end if;


   Ada.Text_IO.Put ("Enter name of server application to register: ");
   Ada.Text_IO.Get_Line (Application_Name, Application_Name_Length);

   Ada.Text_IO.Put ("You typed in the string: ");
   Ada.Text_IO.Put_Line (Application_Name(1..Application_Name_Length));

   Ada.Text_IO.Put ("Enter name of DIF to register '" & Application_Name(1..Application_Name_Length) & "' to: ");
   Ada.Text_IO.Get_Line (DIF_Name, DIF_Name_Length);
   
   Ada.Text_IO.Put ("You typed in the string: ");
   Ada.Text_IO.Put_Line (DIF_Name(1..DIF_Name_Length));

   Register_Success := RINA_Register (RINA_Dev_FD, DIF_Name(1..DIF_Name_Length), Application_Name(1..Application_Name_Length), 0);

   if Integer(Register_Success) < 0 then
      Text_IO.Put_Line ("Error registering application to " & DIF_Name);
      raise Exceptions.DIF_Registration_Failure;
   else
      Text_IO.Put_Line ("Successfully registered application to " & DIF_Name);
   end if;

end Embedded_Rina_Bin;