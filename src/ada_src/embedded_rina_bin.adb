--  Temp disabling
pragma Style_Checks (Off);

with System;
with Ada.Text_IO;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Exceptions;

procedure Embedded_Rina_Bin is
   package Text_IO renames Ada.Text_IO;
   package C renames Interfaces.C;
   package Unbounded renames Ada.Strings.Unbounded;

   --  Flow specs for QoS
   --  version number to allow for extensions
   type rina_flow_spec is record
      version : aliased Integer;
      max_delay : aliased Integer;
      max_sdu_gap : aliased Integer;
      avg_bandwidth : aliased Integer;
      max_loss : aliased Integer;
      in_order_delivery : aliased Integer;
      msg_boundaries : aliased Integer;
      max_jitter : aliased Integer;
   end record
      with Convention => C_Pass_By_Copy;

   --  TODO: Eventually move all these bindings below to their own package --

   --  int rina_open (void)
   --  Open a file descriptor that can be used to register/unregister names,
   --  and to manage incoming flow allocation requests. On success, it
   --  returns a file descriptor that can be later provided to rina_register(),
   --  rina_unregister(), rina_flow_accept(), and rina_flow_respond().
   --  On error -1 is returned.
   function RINA_Open return Integer;
   pragma Import (C, RINA_Open, "rina_open");

   --  int rina_register(int fd,
   --                    const char *dif_name,
   --                    const char *local_appl
   --                    int flags)
  --  Register the application name local_appl to a DIF in the system.
  --  After a successful registration, flow allocation requests can be received
  --  on fd.
   function RINA_Register (fd : Integer;
      dif_name : C.Strings.chars_ptr;
      local_appl : C.Strings.chars_ptr;
      flags : Integer) return Integer;
   pragma Import (C, RINA_Register, "rina_register");


   --  TODO: Finish adding docs
   function RINA_Flow_Accept
     (fd : Integer;
      remote_appl : out System.Address;
      spec : access rina_flow_spec;
      flags : Natural) return Integer;
   pragma Import (C, RINA_Flow_Accept, "rina_flow_accept");

   --  TODO: Finish adding docs
   function RINA_Flow_Respond
     (fd : Integer;
      handle : Integer;
      response : Integer) return Integer;
   pragma Import (C, RINA_Flow_Respond, "rina_flow_respond");

   --  TODO: This needs to be moved entirely into Ada
   --  Figure out how to do I/O with file descriptor like in traditional read(), write() syscalls
   procedure Chat_DoFlow (fd : Integer);
   pragma Import (C, Chat_DoFlow, "doFlow");

   --  Errno global
   errno : Integer
      with
        Import      => True,
        Convention  => C;

   --  Placeholders for file descriptor values, assume invalid (< 0)
   RINA_Dev_FD : Integer := -1;
   Incoming_FD : Integer := -1;
   Flow_FD : Integer := -1;
   Flow_Response : Integer := -1;
   Register_Success : Integer := -1;
   
   type Rina_Flow_Spec_Access is access all rina_flow_spec;

   -- Assume no requirements during testing
   Default_Spec : constant Rina_Flow_Spec_Access := new rina_flow_spec'(
      version => 2,
      max_delay => 0,
      max_sdu_gap => 0,
      avg_bandwidth => 0,
      max_loss => 0,
      in_order_delivery => 0,
      msg_boundaries => 0,
      max_jitter => 0
   );

   -- TODO: Move me eventually
   -- Credit: Olivier Henley
   function To_Str (Addr : System.Address) return String is
        type C_String_T is access all String (1 .. Integer'Last);
        function To_C_String_T is new Ada.Unchecked_Conversion (System.Address, C_String_T);
        Ptr : constant C_String_T := To_C_String_T (Addr);
        C   : Integer := 0;
    begin
        for I in Ptr'Range loop
            C := I;
            exit when Ptr (I) = ASCII.NUL;
        end loop;
        return Ptr (1 .. C-1);
    end;

   --  Hardcoded for testing purposes
   Application_Name : constant C.Strings.chars_ptr := C.Strings.New_String ("TestServer");
   DIF_Name : constant C.Strings.chars_ptr := C.Strings.New_String ("cool.DIF");
   Incoming_APN : System.Address;
begin
   Text_IO.Put_Line ("Starting RINA server application....");
   RINA_Dev_FD := RINA_Open;

   if RINA_Dev_FD < 0 then
      Text_IO.Put_Line ("Error opening RINA control device");
      raise Exceptions.RINA_Control_Failure;
   else
      Text_IO.Put_Line ("Successfully opened RINA control device (File Desc: " & Integer'Image (RINA_Dev_FD) & ")");
   end if;

   Register_Success := RINA_Register (RINA_Dev_FD, DIF_Name, Application_Name, 0);

   if Register_Success < 0 then
      Text_IO.Put_Line ("Error registering application to cool.DIF");
      raise Exceptions.DIF_Registration_Failure;
   else
      Text_IO.Put_Line ("Successfully registered application to cool.DIF");
   end if;

   
   loop
      Incoming_FD := RINA_Flow_Accept (RINA_Dev_FD, Incoming_APN, Default_Spec, 2);

      if Incoming_FD < 0 then
         Text_IO.Put_Line ("WARNING: Unexpected flow accept failure, ignoring");
         raise Exceptions.Unexpected_Flow_Failure;
      end if;

   
      Text_IO.Put_Line (To_Str (Incoming_APN));

      if To_Str (Incoming_APN) /= "" then
         Text_IO.Put ("Received incoming flow requets from app ");
         Text_IO.Put_Line (To_Str (Incoming_APN));
	   else
			Text_IO.Put_Line ("Received incoming flow request, no apn supplied");
		end if;

      loop

         --  TODO: DEBUG ONLY --
         --  Text_IO.Put_Line ("with flowspec:");
         --  Text_IO.Put ("max_sdu_gap (in SDUs): ");
         --  Text_IO.Put_Line (Integer'Image(Default_Spec.max_sdu_gap));
         --  Text_IO.Put ("avg_bandwidth (in bits/second): ");
         --  Text_IO.Put_Line (Integer'Image(Default_Spec.avg_bandwidth));
         --  Text_IO.Put ("max_delay (in microseconds): ");
         --  Text_IO.Put_Line (Integer'Image(Default_Spec.max_delay));
         --  Text_IO.Put ("max_loss (percentage): ");
         --  Text_IO.Put_Line (Integer'Image(Default_Spec.max_loss / RINA_FLOW_SPEC_LOSS_MAX);
         --  Text_IO.Put ("max_jitter (in microseconds): ");
         --  Text_IO.Put_Line (Integer'Image(Default_Spec.max_jitter));
         --  Text_IO.Put ("in_order_delivery (boolean): ");
         --  Text_IO.Put_Line (Integer'Image(Default_Spec.in_order_delivery));
         --  Text_IO.Put ("msg_boundaries (boolean, 0 = stream): ");
         --  Text_IO.Put_Line (Integer'Image(Default_Spec.msg_boundaries));

         --  Accept the flow request
		   Flow_Response := RINA_Flow_Respond(RINA_Dev_FD, Incoming_FD, 0);
         
         if Flow_Response < 0 then
            Text_IO.Put_Line ("Error responding to flow request, is client still open?");
            raise Exceptions.Unexpected_Flow_Failure;
         else
            Text_IO.Put_Line ("Accepted flow request.");
         end if;
         
         loop
            -- TODO: This needs to be moved entirely to Ada
            -- Figure out the equivalent of read()/write() syscalls
            Chat_DoFlow (Flow_Response);
         end loop;
      end loop;
   end loop;
end Embedded_Rina_Bin;