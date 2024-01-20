pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: 10.5.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_test_client" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#8d359d34#;
   pragma Export (C, u00001, "test_clientB");
   u00002 : constant Version_32 := 16#050ff2f0#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#4113f22b#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#185015e7#;
   pragma Export (C, u00005, "ada__exceptionsB");
   u00006 : constant Version_32 := 16#d6578bab#;
   pragma Export (C, u00006, "ada__exceptionsS");
   u00007 : constant Version_32 := 16#5726abed#;
   pragma Export (C, u00007, "ada__exceptions__last_chance_handlerB");
   u00008 : constant Version_32 := 16#cfec26ee#;
   pragma Export (C, u00008, "ada__exceptions__last_chance_handlerS");
   u00009 : constant Version_32 := 16#4635ec04#;
   pragma Export (C, u00009, "systemS");
   u00010 : constant Version_32 := 16#ae860117#;
   pragma Export (C, u00010, "system__soft_linksB");
   u00011 : constant Version_32 := 16#8d3f9472#;
   pragma Export (C, u00011, "system__soft_linksS");
   u00012 : constant Version_32 := 16#f32b4133#;
   pragma Export (C, u00012, "system__secondary_stackB");
   u00013 : constant Version_32 := 16#03a1141d#;
   pragma Export (C, u00013, "system__secondary_stackS");
   u00014 : constant Version_32 := 16#86dbf443#;
   pragma Export (C, u00014, "system__parametersB");
   u00015 : constant Version_32 := 16#0ed9b82f#;
   pragma Export (C, u00015, "system__parametersS");
   u00016 : constant Version_32 := 16#ced09590#;
   pragma Export (C, u00016, "system__storage_elementsB");
   u00017 : constant Version_32 := 16#6bf6a600#;
   pragma Export (C, u00017, "system__storage_elementsS");
   u00018 : constant Version_32 := 16#75bf515c#;
   pragma Export (C, u00018, "system__soft_links__initializeB");
   u00019 : constant Version_32 := 16#5697fc2b#;
   pragma Export (C, u00019, "system__soft_links__initializeS");
   u00020 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00020, "system__stack_checkingB");
   u00021 : constant Version_32 := 16#c88a87ec#;
   pragma Export (C, u00021, "system__stack_checkingS");
   u00022 : constant Version_32 := 16#34742901#;
   pragma Export (C, u00022, "system__exception_tableB");
   u00023 : constant Version_32 := 16#1b9b8546#;
   pragma Export (C, u00023, "system__exception_tableS");
   u00024 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00024, "system__exceptionsB");
   u00025 : constant Version_32 := 16#2e5681f2#;
   pragma Export (C, u00025, "system__exceptionsS");
   u00026 : constant Version_32 := 16#69416224#;
   pragma Export (C, u00026, "system__exceptions__machineB");
   u00027 : constant Version_32 := 16#5c74e542#;
   pragma Export (C, u00027, "system__exceptions__machineS");
   u00028 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00028, "system__exceptions_debugB");
   u00029 : constant Version_32 := 16#38bf15c0#;
   pragma Export (C, u00029, "system__exceptions_debugS");
   u00030 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00030, "system__img_intB");
   u00031 : constant Version_32 := 16#44ee0cc6#;
   pragma Export (C, u00031, "system__img_intS");
   u00032 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00032, "system__tracebackB");
   u00033 : constant Version_32 := 16#181732c0#;
   pragma Export (C, u00033, "system__tracebackS");
   u00034 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00034, "system__traceback_entriesB");
   u00035 : constant Version_32 := 16#466e1a74#;
   pragma Export (C, u00035, "system__traceback_entriesS");
   u00036 : constant Version_32 := 16#448e9548#;
   pragma Export (C, u00036, "system__traceback__symbolicB");
   u00037 : constant Version_32 := 16#46491211#;
   pragma Export (C, u00037, "system__traceback__symbolicS");
   u00038 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00038, "ada__containersS");
   u00039 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00039, "ada__exceptions__tracebackB");
   u00040 : constant Version_32 := 16#ae2d2db5#;
   pragma Export (C, u00040, "ada__exceptions__tracebackS");
   u00041 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00041, "interfacesS");
   u00042 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00042, "interfaces__cB");
   u00043 : constant Version_32 := 16#467817d8#;
   pragma Export (C, u00043, "interfaces__cS");
   u00044 : constant Version_32 := 16#e865e681#;
   pragma Export (C, u00044, "system__bounded_stringsB");
   u00045 : constant Version_32 := 16#31c8cd1d#;
   pragma Export (C, u00045, "system__bounded_stringsS");
   u00046 : constant Version_32 := 16#0062635e#;
   pragma Export (C, u00046, "system__crtlS");
   u00047 : constant Version_32 := 16#bba79bcb#;
   pragma Export (C, u00047, "system__dwarf_linesB");
   u00048 : constant Version_32 := 16#9a78d181#;
   pragma Export (C, u00048, "system__dwarf_linesS");
   u00049 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00049, "ada__charactersS");
   u00050 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00050, "ada__characters__handlingB");
   u00051 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00051, "ada__characters__handlingS");
   u00052 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00052, "ada__characters__latin_1S");
   u00053 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00053, "ada__stringsS");
   u00054 : constant Version_32 := 16#96df1a3f#;
   pragma Export (C, u00054, "ada__strings__mapsB");
   u00055 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00055, "ada__strings__mapsS");
   u00056 : constant Version_32 := 16#5886cb31#;
   pragma Export (C, u00056, "system__bit_opsB");
   u00057 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00057, "system__bit_opsS");
   u00058 : constant Version_32 := 16#72b39087#;
   pragma Export (C, u00058, "system__unsigned_typesS");
   u00059 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00059, "ada__strings__maps__constantsS");
   u00060 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00060, "system__address_imageB");
   u00061 : constant Version_32 := 16#e7d9713e#;
   pragma Export (C, u00061, "system__address_imageS");
   u00062 : constant Version_32 := 16#ec78c2bf#;
   pragma Export (C, u00062, "system__img_unsB");
   u00063 : constant Version_32 := 16#ed47ac70#;
   pragma Export (C, u00063, "system__img_unsS");
   u00064 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00064, "system__ioB");
   u00065 : constant Version_32 := 16#d8771b4b#;
   pragma Export (C, u00065, "system__ioS");
   u00066 : constant Version_32 := 16#f790d1ef#;
   pragma Export (C, u00066, "system__mmapB");
   u00067 : constant Version_32 := 16#7c445363#;
   pragma Export (C, u00067, "system__mmapS");
   u00068 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00068, "ada__io_exceptionsS");
   u00069 : constant Version_32 := 16#91eaca2e#;
   pragma Export (C, u00069, "system__mmap__os_interfaceB");
   u00070 : constant Version_32 := 16#1fc2f713#;
   pragma Export (C, u00070, "system__mmap__os_interfaceS");
   u00071 : constant Version_32 := 16#1e7d913a#;
   pragma Export (C, u00071, "system__mmap__unixS");
   u00072 : constant Version_32 := 16#54420b60#;
   pragma Export (C, u00072, "system__os_libB");
   u00073 : constant Version_32 := 16#d872da39#;
   pragma Export (C, u00073, "system__os_libS");
   u00074 : constant Version_32 := 16#ec4d5631#;
   pragma Export (C, u00074, "system__case_utilB");
   u00075 : constant Version_32 := 16#79e05a50#;
   pragma Export (C, u00075, "system__case_utilS");
   u00076 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00076, "system__stringsB");
   u00077 : constant Version_32 := 16#2623c091#;
   pragma Export (C, u00077, "system__stringsS");
   u00078 : constant Version_32 := 16#5a3f5337#;
   pragma Export (C, u00078, "system__object_readerB");
   u00079 : constant Version_32 := 16#82413105#;
   pragma Export (C, u00079, "system__object_readerS");
   u00080 : constant Version_32 := 16#fb020d94#;
   pragma Export (C, u00080, "system__val_lliB");
   u00081 : constant Version_32 := 16#2a5b7ef4#;
   pragma Export (C, u00081, "system__val_lliS");
   u00082 : constant Version_32 := 16#b8e72903#;
   pragma Export (C, u00082, "system__val_lluB");
   u00083 : constant Version_32 := 16#1f7d1d65#;
   pragma Export (C, u00083, "system__val_lluS");
   u00084 : constant Version_32 := 16#269742a9#;
   pragma Export (C, u00084, "system__val_utilB");
   u00085 : constant Version_32 := 16#ea955afa#;
   pragma Export (C, u00085, "system__val_utilS");
   u00086 : constant Version_32 := 16#d7bf3f29#;
   pragma Export (C, u00086, "system__exception_tracesB");
   u00087 : constant Version_32 := 16#62eacc9e#;
   pragma Export (C, u00087, "system__exception_tracesS");
   u00088 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00088, "system__wch_conB");
   u00089 : constant Version_32 := 16#5d48ced6#;
   pragma Export (C, u00089, "system__wch_conS");
   u00090 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00090, "system__wch_stwB");
   u00091 : constant Version_32 := 16#7059e2d7#;
   pragma Export (C, u00091, "system__wch_stwS");
   u00092 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00092, "system__wch_cnvB");
   u00093 : constant Version_32 := 16#52ff7425#;
   pragma Export (C, u00093, "system__wch_cnvS");
   u00094 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00094, "system__wch_jisB");
   u00095 : constant Version_32 := 16#d28f6d04#;
   pragma Export (C, u00095, "system__wch_jisS");
   u00096 : constant Version_32 := 16#5d91da9f#;
   pragma Export (C, u00096, "ada__tagsB");
   u00097 : constant Version_32 := 16#12a0afb8#;
   pragma Export (C, u00097, "ada__tagsS");
   u00098 : constant Version_32 := 16#796f31f1#;
   pragma Export (C, u00098, "system__htableB");
   u00099 : constant Version_32 := 16#c2f75fee#;
   pragma Export (C, u00099, "system__htableS");
   u00100 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00100, "system__string_hashB");
   u00101 : constant Version_32 := 16#60a93490#;
   pragma Export (C, u00101, "system__string_hashS");
   u00102 : constant Version_32 := 16#f4e097a7#;
   pragma Export (C, u00102, "ada__text_ioB");
   u00103 : constant Version_32 := 16#777d5329#;
   pragma Export (C, u00103, "ada__text_ioS");
   u00104 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00104, "ada__streamsB");
   u00105 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00105, "ada__streamsS");
   u00106 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00106, "interfaces__c_streamsB");
   u00107 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00107, "interfaces__c_streamsS");
   u00108 : constant Version_32 := 16#ec9c64c3#;
   pragma Export (C, u00108, "system__file_ioB");
   u00109 : constant Version_32 := 16#e1440d61#;
   pragma Export (C, u00109, "system__file_ioS");
   u00110 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00110, "ada__finalizationS");
   u00111 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00111, "system__finalization_rootB");
   u00112 : constant Version_32 := 16#09c79f94#;
   pragma Export (C, u00112, "system__finalization_rootS");
   u00113 : constant Version_32 := 16#bbaa76ac#;
   pragma Export (C, u00113, "system__file_control_blockS");
   u00114 : constant Version_32 := 16#6c899da8#;
   pragma Export (C, u00114, "bindingsS");
   u00115 : constant Version_32 := 16#bc34ac54#;
   pragma Export (C, u00115, "bindings__rliteS");
   u00116 : constant Version_32 := 16#0d731a7d#;
   pragma Export (C, u00116, "bindings__rlite__apiB");
   u00117 : constant Version_32 := 16#4bcea6a0#;
   pragma Export (C, u00117, "bindings__rlite__apiS");
   u00118 : constant Version_32 := 16#c6e7753a#;
   pragma Export (C, u00118, "bindings__rlite__ctrlB");
   u00119 : constant Version_32 := 16#2a25a427#;
   pragma Export (C, u00119, "bindings__rlite__ctrlS");
   u00120 : constant Version_32 := 16#43511b1f#;
   pragma Export (C, u00120, "bindings__rlite__msgB");
   u00121 : constant Version_32 := 16#2013b2e0#;
   pragma Export (C, u00121, "bindings__rlite__msgS");
   u00122 : constant Version_32 := 16#e7cbf0cd#;
   pragma Export (C, u00122, "debugB");
   u00123 : constant Version_32 := 16#3eb29bba#;
   pragma Export (C, u00123, "debugS");
   u00124 : constant Version_32 := 16#303292cc#;
   pragma Export (C, u00124, "exceptionsS");
   u00125 : constant Version_32 := 16#c5e5bdb1#;
   pragma Export (C, u00125, "buffersB");
   u00126 : constant Version_32 := 16#426c2c7a#;
   pragma Export (C, u00126, "buffersS");
   u00127 : constant Version_32 := 16#b5988c27#;
   pragma Export (C, u00127, "gnatS");
   u00128 : constant Version_32 := 16#efb85c8a#;
   pragma Export (C, u00128, "gnat__os_libS");
   u00129 : constant Version_32 := 16#883ea5ad#;
   pragma Export (C, u00129, "namesB");
   u00130 : constant Version_32 := 16#a45bc0c4#;
   pragma Export (C, u00130, "namesS");
   u00131 : constant Version_32 := 16#2977d08d#;
   pragma Export (C, u00131, "ada__strings__boundedB");
   u00132 : constant Version_32 := 16#5ea9a713#;
   pragma Export (C, u00132, "ada__strings__boundedS");
   u00133 : constant Version_32 := 16#7ec26662#;
   pragma Export (C, u00133, "ada__strings__superboundedB");
   u00134 : constant Version_32 := 16#da6addee#;
   pragma Export (C, u00134, "ada__strings__superboundedS");
   u00135 : constant Version_32 := 16#60da0992#;
   pragma Export (C, u00135, "ada__strings__searchB");
   u00136 : constant Version_32 := 16#c1ab8667#;
   pragma Export (C, u00136, "ada__strings__searchS");
   u00137 : constant Version_32 := 16#acee74ad#;
   pragma Export (C, u00137, "system__compare_array_unsigned_8B");
   u00138 : constant Version_32 := 16#ef369d89#;
   pragma Export (C, u00138, "system__compare_array_unsigned_8S");
   u00139 : constant Version_32 := 16#a8025f3c#;
   pragma Export (C, u00139, "system__address_operationsB");
   u00140 : constant Version_32 := 16#55395237#;
   pragma Export (C, u00140, "system__address_operationsS");
   u00141 : constant Version_32 := 16#039168f8#;
   pragma Export (C, u00141, "system__stream_attributesB");
   u00142 : constant Version_32 := 16#8bc30a4e#;
   pragma Export (C, u00142, "system__stream_attributesS");
   u00143 : constant Version_32 := 16#0ae8ef7f#;
   pragma Export (C, u00143, "bindings__rlite__msg__registerB");
   u00144 : constant Version_32 := 16#cfd13f10#;
   pragma Export (C, u00144, "bindings__rlite__msg__registerS");
   u00145 : constant Version_32 := 16#203b8c49#;
   pragma Export (C, u00145, "system__strings__stream_opsB");
   u00146 : constant Version_32 := 16#ec029138#;
   pragma Export (C, u00146, "system__strings__stream_opsS");
   u00147 : constant Version_32 := 16#416d4c4d#;
   pragma Export (C, u00147, "bindings__rlite__commonS");
   u00148 : constant Version_32 := 16#69f6ee6b#;
   pragma Export (C, u00148, "interfaces__c__stringsB");
   u00149 : constant Version_32 := 16#603c1c44#;
   pragma Export (C, u00149, "interfaces__c__stringsS");
   u00150 : constant Version_32 := 16#302de9ab#;
   pragma Export (C, u00150, "bindings__rlite__listS");
   u00151 : constant Version_32 := 16#d22db9ed#;
   pragma Export (C, u00151, "bindings__rlite__msg__flowB");
   u00152 : constant Version_32 := 16#442fa920#;
   pragma Export (C, u00152, "bindings__rlite__msg__flowS");
   u00153 : constant Version_32 := 16#f2bb62e5#;
   pragma Export (C, u00153, "bindings__rlite__utilsS");
   u00154 : constant Version_32 := 16#e31b7c4e#;
   pragma Export (C, u00154, "system__memoryB");
   u00155 : constant Version_32 := 16#1f488a30#;
   pragma Export (C, u00155, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.img_uns%s
   --  system.img_uns%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.traceback%s
   --  system.traceback%b
   --  ada.characters.handling%s
   --  system.case_util%s
   --  system.os_lib%s
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  ada.exceptions%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.soft_links%s
   --  system.val_lli%s
   --  system.val_llu%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.address_image%s
   --  system.address_image%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.bounded_strings%s
   --  system.bounded_strings%b
   --  system.case_util%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.maps.constants%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  ada.characters.handling%b
   --  system.exception_traces%s
   --  system.exception_traces%b
   --  system.memory%s
   --  system.memory%b
   --  system.mmap%s
   --  system.mmap.os_interface%s
   --  system.mmap%b
   --  system.mmap.unix%s
   --  system.mmap.os_interface%b
   --  system.object_reader%s
   --  system.object_reader%b
   --  system.dwarf_lines%s
   --  system.dwarf_lines%b
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.standard_library%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  system.val_lli%b
   --  system.val_llu%b
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.strings.superbounded%s
   --  ada.strings.superbounded%b
   --  ada.strings.bounded%s
   --  ada.strings.bounded%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.streams%s
   --  ada.streams%b
   --  gnat%s
   --  gnat.os_lib%s
   --  interfaces.c.strings%s
   --  interfaces.c.strings%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  bindings%s
   --  bindings.rlite%s
   --  bindings.rlite.common%s
   --  bindings.rlite.list%s
   --  bindings.rlite.utils%s
   --  debug%s
   --  debug%b
   --  exceptions%s
   --  buffers%s
   --  buffers%b
   --  names%s
   --  names%b
   --  bindings.rlite.msg%s
   --  bindings.rlite.msg%b
   --  bindings.rlite.msg.flow%s
   --  bindings.rlite.msg.flow%b
   --  bindings.rlite.msg.register%s
   --  bindings.rlite.msg.register%b
   --  bindings.rlite.api%s
   --  bindings.rlite.ctrl%s
   --  bindings.rlite.ctrl%b
   --  bindings.rlite.api%b
   --  test_client%b
   --  END ELABORATION ORDER

end ada_main;
