with Demos;

procedure Demo is
   procedure Header is
   begin
      Demos.Put (0, 0, "eRINA_Debug");
   end Header;

   --  Max of 32 IPCs can be registered
   MAX_IPC_COUNT : constant Natural := 32;
   MAX_IPCP_NAME_LENGTH : constant Natural := 128;

   type IPC_Handler_Array is array (1 .. MAX_IPC_COUNT) of String (1 .. MAX_IPCP_NAME_LENGTH);

   --  Eventually make this the protected object that holds the IPC database
   IPC_Handlers    : IPC_Handler_Array;
begin

   Header;

end Demo;