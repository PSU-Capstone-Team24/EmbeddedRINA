pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__erina_tests.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__erina_tests.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E079 : Short_Integer; pragma Import (Ada, E079, "system__os_lib_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__soft_links_E");
   E029 : Short_Integer; pragma Import (Ada, E029, "system__exception_table_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "ada__io_exceptions_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "ada__strings_E");
   E044 : Short_Integer; pragma Import (Ada, E044, "ada__containers_E");
   E031 : Short_Integer; pragma Import (Ada, E031, "system__exceptions_E");
   E061 : Short_Integer; pragma Import (Ada, E061, "ada__strings__maps_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "ada__strings__maps__constants_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "interfaces__c_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__soft_links__initialize_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "system__object_reader_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "system__dwarf_lines_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "system__traceback__symbolic_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "ada__tags_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "ada__streams_E");
   E176 : Short_Integer; pragma Import (Ada, E176, "gnat_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "system__file_control_block_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "system__finalization_root_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "ada__finalization_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "system__file_io_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "system__storage_pools_E");
   E167 : Short_Integer; pragma Import (Ada, E167, "system__finalization_masters_E");
   E162 : Short_Integer; pragma Import (Ada, E162, "ada__calendar_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "ada__text_io_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "system__pool_global_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "aunit_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "aunit__memory_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "aunit__memory__utils_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "ada_containers__aunit_lists_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "aunit__tests_E");
   E156 : Short_Integer; pragma Import (Ada, E156, "aunit__time_measure_E");
   E154 : Short_Integer; pragma Import (Ada, E154, "aunit__test_results_E");
   E147 : Short_Integer; pragma Import (Ada, E147, "aunit__assertions_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "aunit__test_filters_E");
   E145 : Short_Integer; pragma Import (Ada, E145, "aunit__simple_test_cases_E");
   E013 : Short_Integer; pragma Import (Ada, E013, "aunit__reporter_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "aunit__reporter__text_E");
   E190 : Short_Integer; pragma Import (Ada, E190, "aunit__test_suites_E");
   E188 : Short_Integer; pragma Import (Ada, E188, "aunit__run_E");
   E192 : Short_Integer; pragma Import (Ada, E192, "test_rina_open_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E190 := E190 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "aunit__test_suites__finalize_spec");
      begin
         F1;
      end;
      E184 := E184 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "aunit__reporter__text__finalize_spec");
      begin
         F2;
      end;
      E145 := E145 - 1;
      E143 := E143 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "aunit__simple_test_cases__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "aunit__test_filters__finalize_spec");
      begin
         F4;
      end;
      E147 := E147 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "aunit__assertions__finalize_spec");
      begin
         F5;
      end;
      E154 := E154 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "aunit__test_results__finalize_spec");
      begin
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "aunit__tests__finalize_spec");
      begin
         E165 := E165 - 1;
         F7;
      end;
      E173 := E173 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__pool_global__finalize_spec");
      begin
         F8;
      end;
      E114 := E114 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "ada__text_io__finalize_spec");
      begin
         F9;
      end;
      E167 := E167 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__finalization_masters__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "system__file_io__finalize_body");
      begin
         E118 := E118 - 1;
         F11;
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
      E029 := E029 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E074 := E074 + 1;
      Ada.Strings'Elab_Spec;
      E059 := E059 + 1;
      Ada.Containers'Elab_Spec;
      E044 := E044 + 1;
      System.Exceptions'Elab_Spec;
      E031 := E031 + 1;
      System.Os_Lib'Elab_Body;
      E079 := E079 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E065 := E065 + 1;
      Interfaces.C'Elab_Spec;
      System.Soft_Links.Initialize'Elab_Body;
      E025 := E025 + 1;
      E019 := E019 + 1;
      E061 := E061 + 1;
      E049 := E049 + 1;
      System.Object_Reader'Elab_Spec;
      System.Dwarf_Lines'Elab_Spec;
      E054 := E054 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E043 := E043 + 1;
      E085 := E085 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E105 := E105 + 1;
      Ada.Streams'Elab_Spec;
      E103 := E103 + 1;
      Gnat'Elab_Spec;
      E176 := E176 + 1;
      System.File_Control_Block'Elab_Spec;
      E122 := E122 + 1;
      System.Finalization_Root'Elab_Spec;
      E121 := E121 + 1;
      Ada.Finalization'Elab_Spec;
      E119 := E119 + 1;
      System.File_Io'Elab_Body;
      E118 := E118 + 1;
      System.Storage_Pools'Elab_Spec;
      E171 := E171 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E167 := E167 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E162 := E162 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E114 := E114 + 1;
      System.Pool_Global'Elab_Spec;
      E173 := E173 + 1;
      E008 := E008 + 1;
      E005 := E005 + 1;
      E152 := E152 + 1;
      E149 := E149 + 1;
      Aunit.Tests'Elab_Spec;
      E165 := E165 + 1;
      Aunit.Time_Measure'Elab_Spec;
      E156 := E156 + 1;
      Aunit.Test_Results'Elab_Spec;
      E154 := E154 + 1;
      Aunit.Assertions'Elab_Spec;
      Aunit.Assertions'Elab_Body;
      E147 := E147 + 1;
      Aunit.Test_Filters'Elab_Spec;
      Aunit.Simple_Test_Cases'Elab_Spec;
      E143 := E143 + 1;
      E145 := E145 + 1;
      Aunit.Reporter'Elab_Spec;
      E013 := E013 + 1;
      Aunit.Reporter.Text'Elab_Spec;
      E184 := E184 + 1;
      Aunit.Test_Suites'Elab_Spec;
      E190 := E190 + 1;
      E188 := E188 + 1;
      Test_Rina_Open'Elab_Body;
      E192 := E192 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_erina_tests");

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
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA_Tests/obj/development/test_rina_open.o
   --   /home/dev/Desktop/eRINA/EmbeddedRINA/eRINA_Tests/obj/development/erina_tests.o
   --   -L/home/dev/Desktop/eRINA/EmbeddedRINA/eRINA_Tests/obj/development/
   --   -L/home/dev/Desktop/eRINA/EmbeddedRINA/eRINA_Tests/obj/development/
   --   -L/home/dev/Desktop/eRINA/EmbeddedRINA/eRINA_Tests/alire/cache/dependencies/aunit_23.0.0_84ce7d0b/lib/aunit/native-full/
   --   -L/usr/lib/gcc/x86_64-linux-gnu/9/adalib/
   --   -shared
   --   -lgnat-9
   --   -ldl
--  END Object file/option list   

end ada_main;
