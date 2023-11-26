pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__test_client.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__test_client.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E073 : Short_Integer; pragma Import (Ada, E073, "system__os_lib_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exception_table_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__io_exceptions_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "ada__strings_E");
   E038 : Short_Integer; pragma Import (Ada, E038, "ada__containers_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exceptions_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "ada__strings__maps_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "ada__strings__maps__constants_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "interfaces__c_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__soft_links__initialize_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "system__object_reader_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "system__dwarf_lines_E");
   E037 : Short_Integer; pragma Import (Ada, E037, "system__traceback__symbolic_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "ada__tags_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "ada__streams_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "gnat_E");
   E153 : Short_Integer; pragma Import (Ada, E153, "interfaces__c__strings_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "system__file_control_block_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "system__finalization_root_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "ada__finalization_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "system__file_io_E");
   E148 : Short_Integer; pragma Import (Ada, E148, "ada__streams__stream_io_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "ada__text_io_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "debug_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "exceptions_E");
   E126 : Short_Integer; pragma Import (Ada, E126, "buffers_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "names_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "bindings__rlite__msg_E");
   E156 : Short_Integer; pragma Import (Ada, E156, "bindings__rlite__msg__flow_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "bindings__rlite__msg__register_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "bindings__rlite__api_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "bindings__rlite__ctrl_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E144 := E144 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "bindings__rlite__msg__register__finalize_spec");
      begin
         F1;
      end;
      E156 := E156 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "bindings__rlite__msg__flow__finalize_spec");
      begin
         F2;
      end;
      E103 := E103 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "ada__text_io__finalize_spec");
      begin
         F3;
      end;
      E148 := E148 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "ada__streams__stream_io__finalize_spec");
      begin
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__file_io__finalize_body");
      begin
         E109 := E109 - 1;
         F5;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Exception_Tracebacks_Symbolic : Integer;
      pragma Import (C, Exception_Tracebacks_Symbolic, "__gl_exception_tracebacks_symbolic");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Exception_Tracebacks_Symbolic := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E023 := E023 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E068 := E068 + 1;
      Ada.Strings'Elab_Spec;
      E053 := E053 + 1;
      Ada.Containers'Elab_Spec;
      E038 := E038 + 1;
      System.Exceptions'Elab_Spec;
      E025 := E025 + 1;
      System.Os_Lib'Elab_Body;
      E073 := E073 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E059 := E059 + 1;
      Interfaces.C'Elab_Spec;
      System.Soft_Links.Initialize'Elab_Body;
      E019 := E019 + 1;
      E011 := E011 + 1;
      E055 := E055 + 1;
      E043 := E043 + 1;
      System.Object_Reader'Elab_Spec;
      System.Dwarf_Lines'Elab_Spec;
      E048 := E048 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E037 := E037 + 1;
      E079 := E079 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E097 := E097 + 1;
      Ada.Streams'Elab_Spec;
      E105 := E105 + 1;
      Gnat'Elab_Spec;
      E127 := E127 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E153 := E153 + 1;
      System.File_Control_Block'Elab_Spec;
      E113 := E113 + 1;
      System.Finalization_Root'Elab_Spec;
      E112 := E112 + 1;
      Ada.Finalization'Elab_Spec;
      E110 := E110 + 1;
      System.File_Io'Elab_Body;
      E109 := E109 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E148 := E148 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E103 := E103 + 1;
      E123 := E123 + 1;
      Exceptions'Elab_Spec;
      E124 := E124 + 1;
      E126 := E126 + 1;
      E130 := E130 + 1;
      Bindings.Rlite.Msg'Elab_Spec;
      E121 := E121 + 1;
      Bindings.Rlite.Msg.Flow'Elab_Spec;
      Bindings.Rlite.Msg.Flow'Elab_Body;
      E156 := E156 + 1;
      Bindings.Rlite.Msg.Register'Elab_Spec;
      Bindings.Rlite.Msg.Register'Elab_Body;
      E144 := E144 + 1;
      E119 := E119 + 1;
      E117 := E117 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_test_client");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/bindings.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/bindings-rlite.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/bindings-rlite-common.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/bindings-rlite-list.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/bindings-rlite-utils.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/debug.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/exceptions.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/buffers.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/names.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/bindings-rlite-msg.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/bindings-rlite-msg-flow.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/bindings-rlite-msg-register.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/bindings-rlite-ctrl.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/bindings-rlite-api.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/test_client.o
   --   -L/home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/
   --   -L/home/dev/Desktop/eRINA/EmbeddedRINA/eRINA/obj/development/
   --   -L/usr/lib/gcc/x86_64-linux-gnu/9/adalib/
   --   -shared
   --   -lgnat-9
   --   -ldl
--  END Object file/option list   

end ada_main;
