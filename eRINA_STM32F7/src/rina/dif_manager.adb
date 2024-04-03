with Debug;

package body DIF_Manager is

   Element_Not_Found : exception;

   function Get (Name : String; DIF_Type : DIF_Types) return DIF is
   begin
      for E of DIF_List loop
         if E.Name = To_Bounded_String (Name) and E.DIF_Type = DIF_Type then
            return E.all;
         end if;
      end loop;

      raise Element_Not_Found;
   end Get;

   function Exists (Name : String; DIF_Type : DIF_Types) return Boolean is
      DIF_Found : DIF;
   begin
      begin
         DIF_Found := Get (Name, DIF_Type);
         return True;
      exception
         when Element_Not_Found =>
            return False;
      end;
   end Exists;

   function Get_IPCP (Name : String) return IPCP is
   begin
      for DIF of DIF_List loop
         Debug.Print
           (Debug.Info, "Checking local DIF: " & To_String (DIF.Name));
         for IPC_Process of DIF.IPCPs loop
            Debug.Print
              (Debug.Info,
               "Checking local IPCP: " & To_String (IPC_Process.Name));
            if To_Bounded_String (Name) = IPC_Process.Name then
               return IPC_Process;
            end if;
         end loop;
      end loop;

      raise Element_Not_Found;
   end Get_IPCP;

   function IPCP_Exists (Name : String) return Boolean is
      IPC_Process : IPCP;
   begin
      IPC_Process := Get_IPCP (Name);
      return True;
   exception
      when Element_Not_Found =>
         return False;
   end IPCP_Exists;

   function Get_Application (Self : in out DIF; Name : String) return Application is
   begin
      for C in Self.Applications.Iterate loop
         if To_String (Self.Applications (C).Name) = Name then
            return Self.Applications (C);
         end if;
      end loop;

      raise Element_Not_Found;
   end Get_Application;

   function Application_Exists (Name : String) return Boolean is
      App : Application;
   begin
      for DIF of DIF_List loop
         begin
         App := Get_Application (DIF.all, Name);
         return True;
         exception
            when Element_Not_Found =>
               null;
         end;
      end loop;
   
      return False;
   end Application_Exists;

   function Create (Name : String; DIF_Type : DIF_Types) return DIF_Access is
      New_DIF : constant DIF_Access := new DIF;
   begin
      Debug.Print (Debug.Info, "Created local DIF: " & Name);
      New_DIF.Name     := To_Bounded_String (Name);
      New_DIF.DIF_Type := DIF_Type;

      DIF_List.Append (New_DIF);
      return New_DIF;
   end Create;
   
   procedure Register (Self : in out DIF; Appl_Name : String; Proc : Procedure_Access) is
      New_App : Application;
   begin
      New_App.Name := To_Unbounded_String (Appl_Name);
      New_App.Proc := Proc;
      Self.Applications.Append (New_App);
   end Register;

   procedure Enroll (Self : in out DIF; IPC_Process : IPCP)
   is -- Flow_Req : Flow) is
   begin
      Debug.Print
        (Debug.Info,
         "Enrolling IPCP: " & To_String (IPC_Process.Name) & " into " &
         To_String (Self.Name));
      Self.IPCPs.Append (IPC_Process);
      Debug.Print
        (Debug.Info,
         "Enrolled IPCP: " & To_String (IPC_Process.Name) & " into " &
         To_String (Self.Name));
   end Enroll;

end DIF_Manager;
