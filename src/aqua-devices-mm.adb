package body Aqua.Devices.MM is

   subtype Register_Index is Word_32 range 0 .. 1;

   Request_Size     : constant Register_Index := 0;
   Response_Address : constant Register_Index := 1;

   type Register_Array is array (Register_Index) of Word_32;

   type Memory_Manager_Reference is access all Aqua.MM.Memory_Manager'Class;

   subtype Parent is Aqua.Devices.Instance;

   type Instance is new Parent with
      record
         Rs      : Register_Array := [others => 0];
         Fetched : Boolean := False;
         Manager : Memory_Manager_Reference;
      end record;

   type Reference is access all Instance'Class;

   overriding function Name (This : Instance) return String is ("mm");

   overriding procedure Get_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_32);

   overriding procedure Set_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_32);

   -----------------
   -- Get_Word_32 --
   -----------------

   overriding procedure Get_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_32)
   is
   begin
      Value := This.Rs (Address / 4);
   end Get_Word_32;

   ----------
   -- Load --
   ----------

   function Load
     (Command : Aqua.Commands.Command_Line;
      Bus     : not null access Aqua.Addressable.Instance'Class)
      return Devices.Reference
   is
      This : constant Reference := new Instance;
   begin
      This.Initialize (Command, Bus);
      This.Rs := [others => 0];
      return Devices.Reference (This);
   end Load;

   -----------------
   -- Set_Manager --
   -----------------

   procedure Set_Manager
     (This    : Devices.Reference;
      Manager : not null access Aqua.MM.Memory_Manager'Class)
   is
   begin
      Instance (This.all).Manager := Memory_Manager_Reference (Manager);
   end Set_Manager;

   -----------------
   -- Set_Word_32 --
   -----------------

   overriding procedure Set_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_32)
   is
      Word_Address : constant Register_Index := Address / 4;
   begin
      This.Rs (Word_Address) := Value;
      if Word_Address = Request_Size
        and then Value /= 0
      then
         This.Rs (Response_Address) :=
           This.Manager.Request_Heap (This.Rs (Request_Size));
      end if;
   end Set_Word_32;

end Aqua.Devices.MM;
