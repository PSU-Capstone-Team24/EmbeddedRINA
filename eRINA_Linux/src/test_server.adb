--  Temp disabling
pragma Style_Checks (Off);

--  Ada
with Ada.Text_IO;
with Ada.Strings;

--  Rlite Bindings
with Bindings.Rlite.API; use Bindings.Rlite.API;

with Bindings.Rlite.Msg.IPCP; use Bindings.Rlite.Msg.IPCP;

with Bindings.Rlite.Msg.Flow; use Bindings.Rlite.Msg;

with Names; use Names.Name_String;

with Exceptions;

with Buffers; use Buffers;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Debug;

procedure Test_Server is
   package Text_IO renames Ada.Text_IO;

   --  Placeholders for file descriptor values, assume invalid (< 0)
   RINA_Dev_FD      : File_Descriptor;
   Register_Success : File_Descriptor := Invalid_FD;

   Application_Name      : String (1 .. 128) := (others => ASCII.NUL);
   Application_Name_Last : Integer;

   DIF_Name      : String (1 .. 128) := (others => ASCII.NUL);
   DIF_Name_Last : Integer;

   Spec         : Flow.RINA_Flow_Spec;
   Incoming_APN : Bounded_String := To_Bounded_String ("");

   Appl_Data_Msg : Bounded_String :=
     To_Bounded_String ("Application Data Packet #");
   Counter_Msg : Integer := 0;

   --  Strings used to create the initial IPCP
   IPCP_Name     : constant String := "test.IPCP";
   IPCP_DIF_Name : constant String := "test.DIF";

   Ipcp_Ret : Rl_Ipcp_Id_T;
begin
   Text_IO.Put_Line ("Starting RINA server application....");
   RINA_Dev_FD := RINA_Open;

   if RINA_Dev_FD = Invalid_FD then
      Text_IO.Put_Line ("Error opening RINA control device");
      raise Exceptions.RINA_Control_Failure;
   else
      Text_IO.Put_Line
        ("Successfully opened RINA control device (File Desc: " &
         File_Descriptor'Image (RINA_Dev_FD) & ")");
   end if;

   Ipcp_Ret :=
     RINA_Create_IPCP (RINA_Dev_FD, IPCP_Name, Normal, IPCP_DIF_Name);

   Ada.Text_IO.Put_Line
     ("Created IPCP '" & IPCP_Name & "' with DIF '" & IPCP_DIF_Name &
      "' of type Normal - IPCP_ID = " & Rl_Ipcp_Id_T'Image (Ipcp_Ret));

   Ada.Text_IO.Put ("Enter name of server application to register: ");
   Ada.Text_IO.Get_Line (Application_Name, Application_Name_Last);

   Ada.Text_IO.Put ("You typed in the string: ");
   Ada.Text_IO.Put_Line (Application_Name);

   while Register_Success = Invalid_FD loop
      Ada.Text_IO.Put
        ("Enter name of DIF to register '" & Application_Name & "' to: ");
      Ada.Text_IO.Get_Line (DIF_Name, DIF_Name_Last);

      Ada.Text_IO.Put ("You typed in the string: ");
      Ada.Text_IO.Put_Line (DIF_Name);

      begin
         Register_Success :=
           RINA_Register (RINA_Dev_FD, DIF_Name, Application_Name, 0);
      exception
         when Exceptions.DIF_Registration_Failure =>
            Register_Success := Invalid_FD;
      end;

      if Register_Success = Invalid_FD then
         Text_IO.Put_Line
           ("Error registering application " & Application_Name & " to " &
            DIF_Name);

         --  Reset DIF name so user has another chance to enter a valid one
         DIF_Name := (others => ASCII.NUL);
      else
         Text_IO.Put_Line
           ("Successfully registered application " & Application_Name &
            " to " & DIF_Name);
      end if;
   end loop;

   loop
      declare
         Flow_Incoming : File_Descriptor := Invalid_FD;
         Flow_Respond  : File_Descriptor := Invalid_FD;
      begin
         --  Block here and wait for an incoming flow...
         Flow_Incoming :=
           RINA_Flow_Accept (RINA_Dev_FD, Incoming_APN, Spec, RINA_F_NORESP);

         --  Check if flow has a valid IPC path through a file descriptor
         if Flow_Incoming /= Invalid_FD then
            Debug.Print
              ("Test_Server",
               "Received incoming flow request from: " &
               To_String (Incoming_APN),
               Debug.Info);
         end if;

         --  Accept incoming flow
         Flow_Respond := RINA_Flow_Respond (RINA_Dev_FD, Flow_Incoming, 0);
         Debug.Print
           ("Test_Server",
            "Accepting flow request from: " & To_String (Incoming_APN),
            Debug.Info);

         if Flow_Respond = Invalid_FD then
            Debug.Print ("Test_Server", "Invalid flow response?", Debug.Error);
         end if;

         loop
            Counter_Msg   := Counter_Msg + 1;
            Appl_Data_Msg :=
              Overwrite
                (Appl_Data_Msg, 26, Integer'Image (Counter_Msg),
                 Ada.Strings.Right);

            declare
               Buf     : Byte_Buffer := Names.To_Packed_Buffer (Appl_Data_Msg);
               Written : Integer     := 0;
            begin
               Written := Write (Flow_Respond, Buf'Address, Buf'Size / 8);
               Debug.Print
                 ("Test_Server",
                  "Sent message: " & To_String (Appl_Data_Msg) & " Bytes: " &
                  Integer'Image (Written),
                  Debug.Info);
            end;

            --  Wait 0.5 second before sending the next message
            delay 0.5;
         end loop;
      end;
   end loop;

   --  TODO: Some key combo to exit the above loop and close out any open FDs?
   --  RINA_Close(RINA_Dev_FD);
end Test_Server;
