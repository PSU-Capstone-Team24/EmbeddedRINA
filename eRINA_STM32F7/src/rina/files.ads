with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Buffers;               use Buffers;

package Files is

   type File_Types is (FILETYPE_IMAGE, FILETYPE_OTHER_BINARY);
   for File_Types use (FILETYPE_IMAGE => 0, FILETYPE_OTHER_BINARY => 1);

   type File is abstract tagged record
      Magic_Number : Uint32 := 16#00AB_CDEF#;
      File_Type    : File_Types;
   end record;

   for File use record
      Magic_Number at 4 range 0 .. 31;
      File_Type    at 8 range 0 ..  7;
   end record;
   pragma Pack (File);

   type Picture_Message is new File with record
      Frame_Number : Uint16 := 0;
      Frame_Total  : Uint16 := 0;
      Payload_Size : Uint32 := 0;
      Identifier   : Unbounded_String;
      Payload_Data : Byte_Vector;
   end record;
   pragma Pack (Picture_Message);

   for Picture_Message use record
      Frame_Number at 12 range 0 .. 15;
      Frame_Total  at 14 range 0 .. 15;
      Payload_Size at 16 range 0 .. 31;
   end record;

   procedure Decode (Self : in out Picture_Message; Buffer : Byte_Buffer);

   --   type Bitmap is record
   --       Signature : Uint8;
   --       FileSize : Uint16;
   --       Reserved : Uint16;
   --       DataOffset : Uint16;
   --       Size : Uint16;
   --       Width : Uint16;
   --       Height : Uint16;
   --       Planes : Uint8;
   --       Compression : Uint16;
   --       ImageSize : Uint16;
   --       XPixelsPerM : Uint16;
   --       YPixelsPerM : Uint16;
   --       ColorsUsed : Uint16;
   --       ColorsImportant : Uint16;
   --       ColorTable : Uint16;
   --       RasterData : Byte_Vector;
   --   end record;
   --   pragma Pack(Bitmap);

   type Bitmap is record
      Identifier : Unbounded_String;
      Data       : Byte_Vector;
   end record;

   package Bitmap_Vectors is new Ada.Containers.Vectors (Positive, Bitmap);
   subtype Bitmap_Vector is Bitmap_Vectors.Vector;

   Image_Files : Bitmap_Vector;

   function Is_File (Buf : in Byte_Buffer) return Boolean;
   function Get_File_Type (Buf : in Byte_Buffer) return File_Types;
end Files;
