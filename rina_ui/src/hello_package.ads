with Gtk.Widget, Gtk.Handlers;
use Gtk.Widget, Gtk.Handlers;
with Gtk.Button;

package Hello_Package is

   package Handlers is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_Widget_Record);

   package Return_Handlers is new Gtk.Handlers.Return_Callback
     (Widget_Type => Gtk_Widget_Record, Return_Type => Boolean);

   -----------------------------------
   -- Browse_Clicked_Event_Callback --
   -----------------------------------
   procedure Browse_Clicked_Event_Callback
     (Button : access Gtk.Button.Gtk_Button_Record'Class);

   -------------------------------------
   -- Transmit_Clicked_Event_Callback --
   -------------------------------------
   procedure Transmit_Clicked_Event_Callback
     (Button : access Gtk.Button.Gtk_Button_Record'Class);

   ----------------------------
   --    Window_Terminate    --
   ----------------------------
   procedure Window_Terminate
     (Window : access Gtk.Widget.Gtk_Widget_Record'Class);

end Hello_Package;
