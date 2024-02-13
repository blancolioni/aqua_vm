package body Aqua.Devices.Linker is

   subtype Register_Index is Word_32 range 0 .. 1;

   Command : constant Register_Index := 0;
   Offset  : constant Register_Index := 1;

   type Register_Array is array (Register_Index) of Word_32;

   type Manager_Reference is access all Aqua.Ld.Link_Manager'Class;

   subtype Parent is Aqua.Devices.Instance;

   type Linker_Instance is new Parent with
      record
         Rs      : Register_Array := [others => 0];
         Manager : Manager_Reference;
      end record;

   type Linker_Reference is access all Linker_Instance'Class;

   overriding function Name (This : Linker_Instance) return String is ("ld");

   overriding procedure Get_Word_32
     (This    : in out Linker_Instance;
      Address : Address_Type;
      Value   : out Word_32);

   overriding procedure Set_Word_32
     (This    : in out Linker_Instance;
      Address : Address_Type;
      Value   : Word_32);

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding procedure Get_Word_32
     (This    : in out Linker_Instance;
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
      return Reference
   is
      This : constant Linker_Reference := new Linker_Instance;
   begin
      This.Initialize (Command, Bus);
      This.Rs := [others => 0];
      return Reference (This);
   end Load;

   -----------------
   -- Set_Manager --
   -----------------

   procedure Set_Manager
     (This    : Reference;
      Manager : not null access Aqua.Ld.Link_Manager'Class)
   is
   begin
      Linker_Instance (This.all).Manager := Manager_Reference (Manager);
   end Set_Manager;

   -----------------
   -- Set_Word_32 --
   -----------------

   overriding procedure Set_Word_32
     (This    : in out Linker_Instance;
      Address : Address_Type;
      Value   : Word_32)
   is
      Word_Address : constant Register_Index := Address / 4;
   begin
      This.Rs (Word_Address) := Value;
      if Word_Address = Command
        and then (Value and 16#8000_0000#) /= 0
      then
         declare
            Op : constant Word_8 :=
                   Word_8 (This.Rs (Command) / 2 ** 8 mod 2 ** 8);
            R  : constant Word_8 :=
                   Word_8 (This.Rs (Command) mod 2 ** 8);
         begin
            This.Manager.Resolve (This.Rs (Offset), Op, R);
         end;
      end if;
   end Set_Word_32;

end Aqua.Devices.Linker;
