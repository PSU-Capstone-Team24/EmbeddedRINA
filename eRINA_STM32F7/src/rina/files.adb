with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Debug;

package body Files is

   function Buffer_To_Unsigned_16 is new Ada.Unchecked_Conversion
     (Source => Byte_Buffer, Target => Unsigned_16);

   function Buffer_To_Unsigned_32 is new Ada.Unchecked_Conversion
     (Source => Byte_Buffer, Target => Unsigned_32);

   function Buffer_To_Unsigned_64 is new Ada.Unchecked_Conversion
     (Source => Byte_Buffer, Target => Unsigned_64);

   function Is_File (Buf : in Byte_Buffer) return Boolean is
      Magic_Number : constant Uint32 := Buffer_To_Unsigned_32 (Buf);
      use type Interfaces.Unsigned_32;
   begin
      if Magic_Number = 16#00AB_CDEF# then
         return True;
      end if;

      return False;
   end Is_File;

   function Get_File_Type (Buf : in Byte_Buffer) return File_Types is
   begin
      if Buf (Buf'First + 4) = 16#00# then
         return FILETYPE_IMAGE;
      end if;

      return FILETYPE_OTHER_BINARY;
   end Get_File_Type;

   function To_Host (Val : in Uint32) return Uint32 is
      Byte0 : constant Uint32 := (Val and 16#FF#) * 2**24;
      Byte1 : constant Uint32 := (Val and 16#FF00#) * 2**8;
      Byte2 : constant Uint32 := (Val and 16#FF_0000#) / 2**8;
      Byte3 : constant Uint32 := (Val and 16#FF00_0000#) / 2**24;
   begin
      return Byte0 or Byte1 or Byte2 or Byte3;
   end To_Host;

   function To_Host (Val : in Uint16) return Uint16 is
   begin
      return Interfaces.Rotate_Left (Val, 8);
   end To_Host;

   --  Yeah... this is brewing a lot of tech debt... but time is limited
   procedure Decode (Self : in out Picture_Message; Buffer : Byte_Buffer) is
      --  Start offset after 32-bit "magic number"
      Offset            : Integer := Buffer'First + 5;
      Identifier_Length : Natural := 0;
   begin
      declare
         Frame_Number : constant Byte_Buffer (0 .. 1) :=
           (Buffer (Offset), Buffer (Offset + 1));
      begin
         Self.Frame_Number := Buffer_To_Unsigned_16 (Frame_Number);
         Offset            := Offset + 2;
      end;

      declare
         Frame_Total : constant Byte_Buffer (0 .. 1) :=
           (Buffer (Offset), Buffer (Offset + 1));
      begin
         Self.Frame_Total := Buffer_To_Unsigned_16 (Frame_Total);
         Offset           := Offset + 2;
      end;

      declare
         Payload_Size : constant Byte_Buffer (0 .. 3) :=
           (Buffer (Offset), Buffer (Offset + 1), Buffer (Offset + 2),
            Buffer (Offset + 3));
      begin
         Self.Payload_Size := Buffer_To_Unsigned_32 (Payload_Size);
         Offset            := Offset + 4;
      end;

      declare
         Identifier_Length_Buf : constant Byte_Buffer (0 .. 1) :=
           (Buffer (Offset), Buffer (Offset + 1));
      begin
         Identifier_Length :=
           Natural (Buffer_To_Unsigned_16 (Identifier_Length_Buf));
         Offset := Offset + 2;
      end;

      declare
         Identifier : Byte_Buffer (1 .. Identifier_Length) := (others => 0);
      begin
         for I in 1 .. Identifier_Length loop
            Identifier (I) := Buffer (Offset);
            Offset         := Offset + 1;
         end loop;

         Self.Identifier :=
           To_Unbounded_String (Buffer_To_String (Identifier));
      end;

      for I in Offset .. Buffer'Last loop
         Self.Payload_Data.Append (Buffer (I));
      end loop;

      declare
         Found : Boolean := False;
         Bmp   : Bitmap;
      begin
         for E of Image_Files loop
            if E.Identifier = Self.Identifier then
               E.Data.Append (Self.Payload_Data);
            end if;
         end loop;

         if not Found then
            Bmp.Identifier := Self.Identifier;
            Bmp.Data       := Self.Payload_Data;
            Image_Files.Append (Bmp);
         end if;
      end;

   end Decode;

end Files;
