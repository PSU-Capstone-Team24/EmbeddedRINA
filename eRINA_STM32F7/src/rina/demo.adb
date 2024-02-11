with GUI;
with Debug;
with Textures;
with Textures.PSU;

procedure Demo is
   Counter : Natural := 0;
begin
   GUI.Initialize ("eRINA Debug");

   Textures.Print (Textures.PSU.Bitmap, (5, 10));
   --  Keep board from immediately terminating
   loop
      Debug.Print (Debug.Info, "Message " & Counter'Image);
      Counter := Counter + 1;
      delay 0.5;
      Debug.Print (Debug.Warning, "Message " & Counter'Image);
      Counter := Counter + 1;
      delay 0.5;
      Debug.Print (Debug.Error, "Message " & Counter'Image);
      Counter := Counter + 1;
      delay 0.5;
   end loop;
end Demo;
