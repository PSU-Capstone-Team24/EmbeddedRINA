pragma Style_Checks (Off);

with Gtk;           use Gtk;
with Gtk.Main;
with Gtk.Builder;   use Gtk.Builder;
with Builder;       use Builder;
with Glib.Error;    use Glib.Error;
with Ada.Text_IO;
with Gtk.Window;
with Gtk.Button;
with Gtk.Frame;
with Gtk.Enums;
with Gtk.Box;
with Gdk.Color;
with Gtk.Info_Bar;
with Glib;          use Glib;
with Hello_Package; use Hello_Package;

procedure RINA_UI is
   Glade_Filename : constant String := "glade.glade";
   Error          : aliased GError;
   Browse_Btn     : Gtk.Button.Gtk_Button;
   Image_Frame    : Gtk.Frame.Gtk_Frame;
   Transmit_Btn   : Gtk.Button.Gtk_Button;
   Info_Bar       : Gtk.Info_Bar.Gtk_Info_Bar;
begin
   Gtk.Main.Init;
   Gtk_New (UI_Builder);

   --  ----------------------------------
   --  Load glade file using UI_Builder
   --  ----------------------------------
   if Add_From_File (UI_Builder, Glade_Filename, Error'Access) = 0 then
      Ada.Text_IO.Put_Line ("Error : " & Get_Message (Error));
      Error_Free (Error);
      return;
   end if;

   --  ----------------------------------
   --  Parent Window
   --  ----------------------------------
   Main_Window := Gtk.Window.Gtk_Window (Get_Object (UI_Builder, "window1"));
   Main_Window.Set_Resizable (False);
   Main_Window.Set_Title ("eRINA UI Demo");
   Main_Window.On_Destroy (Window_Terminate'Access);

   --  ----------------------------------
   --  Browse (...) button
   --  ----------------------------------
   Browse_Btn :=
     Gtk.Button.Gtk_Button (Get_Object (UI_Builder, "BrowseButton"));
   Browse_Btn.On_Clicked (Browse_Clicked_Event_Callback'Access);

   --  ----------------------------------
   --  Image Frame
   --  ----------------------------------
   Image_Frame := Gtk.Frame.Gtk_Frame (Get_Object (UI_Builder, "ImageFrame"));
   Image_Frame.Set_Shadow_Type (Gtk.Enums.Shadow_In);

   --  ----------------------------------
   --  Transmit Button
   --  ----------------------------------
   Transmit_Btn :=
     Gtk.Button.Gtk_Button (Get_Object (UI_Builder, "TransmitButton"));
   Transmit_Btn.On_Clicked (Transmit_Clicked_Event_Callback'Access);
   Transmit_Btn.Set_Sensitive (False);

   Main_Window.Show_All;

   Info_Bar := Gtk.Info_Bar.Gtk_Info_Bar (Get_Object (UI_Builder, "InfoBar"));
   Info_Bar.Hide;

   Gtk.Main.Main;
end RINA_UI;
