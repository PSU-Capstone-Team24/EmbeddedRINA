with GUI;
with Debug;
with Textures;
with Textures.PSU;
with Network;

procedure Demo is
   Counter : Natural := 0;
begin

   GUI.Initialize;
   Network.Initialize;

   --  Keep board from immediately terminating
   loop
      Textures.Print (Textures.PSU.Bitmap, (5, 10));

      GUI.Print ("eRINA Debug", (80, 15));

      GUI.Print
        ("ARP Request Count: " & Network.Get_ARP_Request_Count'Image,
         (80, 30));

      Debug.Print (Debug.Warning, "Blah " & Counter'Image);
      Counter := Counter + 1;
      delay 0.2;
   end loop;
end Demo;