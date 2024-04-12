pragma Style_Checks (Off);

with Interfaces;          use Interfaces;
with Gtk.GEntry;          use Gtk.GEntry;
with Builder;             use Builder;
with Gtkada.File_Selection;
with Gtkada.Dialogs;
with Gtk;                 use Gtk;
with Gtk.Main;
with Gtk.Builder;         use Gtk.Builder;
with Gtk.Label;
with Gtk.Image;           use Gtk.Image;
with Ada.Directories;
with Gtk.Info_Bar;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Bindings.Rlite.API; use Bindings.Rlite.API;
with Bindings.Rlite.Msg.Flow; use Bindings.Rlite.Msg;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Hello_Package is

   function Remove_Leading_Spaces (Str : String) return String is
      First_Non_Space : Positive :=
        Str'Length + 1;
   begin
      -- Find the first non-space character
      for I in Str'Range loop
         if Str (I) /= ' ' then
            First_Non_Space := I;
            exit;
         end if;
      end loop;

      -- Return the substring from the first non-space character to the end
      -- or return an empty string if only spaces were found
      if First_Non_Space <= Str'Length then
         return Str (First_Non_Space .. Str'Last);
      else
         return "";
      end if;
   end Remove_Leading_Spaces;

   function Float_To_String
     (Item : Float; Decimal_Places : Positive) return String
   is
      Temp_Float : Float;
      Factor     : constant Float   := Float (10**Decimal_Places);
      Temp_Int   : Integer;
      S          : String (1 .. 64) := (others => ASCII.NUL);
   begin
      -- Truncate the float to the specified number of decimal places
      Temp_Float := Float (Integer (Item * Factor)) / Factor;

      -- Convert the truncated float to string with the specified decimal places
      Put (To => S, Item => Temp_Float, Aft => Decimal_Places, Exp => 0);

      -- Return the trimmed string
      return Remove_Leading_Spaces (S);
   end Float_To_String;

   -----------------------------------
   -- Browse_Clicked_Event_Callback --
   -----------------------------------
   procedure Browse_Clicked_Event_Callback
     (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Text_Entry : constant GEntry.Gtk_Entry :=
        GEntry.Gtk_Entry (Get_Object (UI_Builder, "FilePath"));
      Tx_Button : constant Gtk.Button.Gtk_Button :=
        Gtk.Button.Gtk_Button (Get_Object (UI_Builder, "TransmitButton"));
      Info_Bar : constant Gtk.Info_Bar.Gtk_Info_Bar :=
        Gtk.Info_Bar.Gtk_Info_Bar (Get_Object (UI_Builder, "InfoBar"));
      Info_Label : constant Gtk.Label.Gtk_Label :=
        Gtk.Label.Gtk_Label (Get_Object (UI_Builder, "InfoLabel"));
      Size_Bytes : Ada.Directories.File_Size;
      Size_KB    : Float;
      Size_MB    : Float;
   begin
      declare
         Response : constant String :=
           Gtkada.File_Selection.File_Selection_Dialog
             (Title    => "Select your file", Default_Dir => "",
              Dir_Only => False, Must_Exist => True);
      begin
         Text_Entry.Set_Text (Response);

         if Ada.Directories.Exists (Text_Entry.Get_Text) then
            Gtk.Image.Gtk_Image (Get_Object (UI_Builder, "ImageRender")).Set
              (Response);
            Tx_Button.Set_Sensitive (True);
            Size_Bytes := Ada.Directories.Size (Response);
            Size_KB    := Float (Size_Bytes) / 1_024.0;

            if Size_KB > 1_000.0 then
               Size_MB := Size_KB / 1_024.0;
               Info_Label.Set_Text
                 ("Size: " & Float_To_String (Size_MB, 2) & " MB (" &
                  Remove_Leading_Spaces (Size_Bytes'Image) & " Bytes)");
            else
               Info_Label.Set_Text
                 ("Size: " & Float_To_String (Size_KB, 2) & " KB (" &
                  Remove_Leading_Spaces (Size_Bytes'Image) & " Bytes)");
            end if;

            Info_Bar.Show;
         end if;
      end;

   end Browse_Clicked_Event_Callback;

   procedure RINA_Send_Image (File_Path : String) is
      Input_File        : Ada.Streams.Stream_IO.File_Type;
      Input_Stream      : Ada.Streams.Stream_IO.Stream_Access;
      Binary_File_Size  : constant Ada.Directories.File_Size := Ada.Directories.Size (File_Path);
      File_Data         : Byte_Buffer_Access := new Byte_Buffer (1 .. Positive (Binary_File_Size));
      Packet            : Picture_Message;
      Spec              : Flow.RINA_Flow_Spec;
      Flow_Fd           : File_Descriptor;
   begin
      Ada.Streams.Stream_IO.Open (Input_File, Ada.Streams.Stream_IO.In_File, File_Path);
      Input_Stream := Ada.Streams.Stream_IO.Stream(Input_File);
      Byte_Buffer'Read (Input_Stream, File_Data.all);
      Ada.Streams.Stream_IO.Close(Input_File);

      Ada.Text_IO.Put_Line ("Read in:" & File_Data'Length'Image & " Bytes");

      Packet.File_Type  := FILETYPE_IMAGE;
      Packet.Identifier  := To_Unbounded_String (Ada.Directories.Base_Name (File_Path));
      Packet.Frame_Total := Uint16(Integer'Max ((Natural (Binary_File_Size) / 1000), 1));
      Packet.Payload_Size := Uint32 (Binary_File_Size);

      Flow_Fd := RINA_Flow_Alloc ("ethAB.DIF", "TestClient", "TestServer", Spec, 0);

      for I in 1 .. Packet.Frame_Total loop
         Packet.Frame_Number := Uint16 (I);
         Packet.Payload_Data := new Byte_Buffer' (File_Data (Integer (I - 1) * Integer'Min(File_Data'Length, 1000) + 1 .. Integer (I) * Integer'Min(File_Data'Length, 1000)));
         
         declare
            Buf : Byte_Buffer := Packet.Serialize;
         begin
            Ada.Text_IO.Put_Line ("Sending frame: " & I'Image & " with size:" & Buf'Length'Image);

            RINA_Write (Flow_Fd, Buf'Address, Buf'Length);
         end;

         --  MT: TODO: Remove, for debugging only
         --  for J in ((I - 1) * 1000 + 1) .. (I * 1000) loop
         --     Ada.Integer_Text_IO.Put (Item => Integer(File_Data (Natural (J))), Base => 16, Width => 2);
         --     Ada.Text_IO.Put (" ");
         --  end loop;
      end loop;

   end RINA_Send_Image;

   procedure Transmit_Clicked_Event_Callback
     (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      File_Path : constant String :=
        GEntry.Gtk_Entry (Get_Object (UI_Builder, "FilePath")).Get_Text;
      use type Ada.Directories.File_Size;
   begin
      if Ada.Directories.Exists (File_Path) and
        Ada.Directories.Size (File_Path) > 1_024
      then
         declare
            Dialog_Result : Gtkada.Dialogs.Message_Dialog_Buttons :=
              Gtkada.Dialogs.Message_Dialog
                (Msg =>
                   "This image is too large to be contained within a single Ethernet frame." &
                   ASCII.LF & "This data will be split into " &
                   Natural
                     (Float'Ceiling
                        (Float (Ada.Directories.Size (File_Path)) / 1_024.0))'
                     Image &
                   " individual Ethernet packets.",
                 Dialog_Type => Gtkada.Dialogs.Information,
                 Buttons     => Gtkada.Dialogs.Button_OK,
                 Title       => "Information",
                 Parent      => Main_Window);

            use type Gtkada.Dialogs.Message_Dialog_Buttons;
         begin
            null;
            --  if Dialog_Result = Gtkada.Dialogs.Button_OK then
            --     RINA_Send_Image (File_Path);
            --  end if;
         end;
      end if;

      RINA_Send_Image (File_Path);
   end Transmit_Clicked_Event_Callback;

   procedure Window_Terminate (Window : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Window);
   begin
      Gtk.Main.Main_Quit;
   exception
      when others =>
         null;
   end Window_Terminate;

end Hello_Package;
