pragma Style_Checks (Off);

with Interfaces; use Interfaces;
with System.Storage_Elements;
with Gtk.Builder;
with Gtk.Window;
with Ada.Sequential_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Builder is
   UI_Builder  : Gtk.Builder.Gtk_Builder;
   Main_Window : Gtk.Window.Gtk_Window;

   subtype Uint8 is Unsigned_8;
   subtype Uint16 is Unsigned_16;
   subtype Uint32 is Unsigned_32;
   subtype Uint64 is Unsigned_64;

   type Byte is new System.Storage_Elements.Storage_Element;
   type Byte_Buffer is array (Natural range <>) of Byte;
   type Byte_Buffer_Access is access all Byte_Buffer;
   
   type String_Access is access all String;

   package Byte_IO is new Ada.Sequential_IO (Element_Type => Byte);
   
   type File_Types is (FILETYPE_IMAGE, FILETYPE_OTHER_BINARY);
   for File_Types use (FILETYPE_IMAGE => 0, FILETYPE_OTHER_BINARY => 1);
   
   type File is abstract tagged record
      Magic_Number    : Uint32 := 16#00ABCDEF#;
      File_Type       : File_Types;
   end record;
   pragma Pack (File);

   for File use record
      Magic_Number at  8 range 0 ..  31;
      File_Type    at  12 range 0 ..  7;
   end record;

   type Picture_Message is new File with record
      Frame_Number    : Uint16 := 0;
      Frame_Total     : Uint16 := 0;
      Payload_Size    : Uint32 := 0;
      Identifier      : Unbounded_String;
      Payload_Data    : Byte_Vector;
   end record;
   pragma Pack (Picture_Message);

   for Picture_Message use record
      Frame_Number at 16 range 0 ..  15;
      Frame_Total  at 18 range 0 ..  15;
      Payload_Size at 20 range 0 .. 31;
   end record;


   function Serialize (Self : in Picture_Message) return Byte_Buffer;

end Builder;
