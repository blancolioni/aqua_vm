with Ada.Text_IO;

package body Aqua.Devices.TTY is

   subtype Parent is Aqua.Devices.Instance;

   type TTY_Instance is new Parent with
      record
         null;
      end record;

   overriding function Name (This : TTY_Instance) return String is ("tty");

   overriding procedure Get_Word_8
     (This : in out TTY_Instance; Address : Address_Type; Value : out Word_8);

   overriding procedure Set_Word_8
     (This : in out TTY_Instance; Address : Address_Type; Value : Word_8);

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding procedure Get_Word_8
     (This : in out TTY_Instance; Address : Address_Type; Value : out Word_8)
   is
      pragma Unreferenced (This);
   begin
      Value := 0;
   end Get_Word_8;

   ----------
   -- Load --
   ----------

   function Load
     (Command : Aqua.Commands.Command_Line;
      Bus     : not null access Aqua.Addressable.Instance'Class)
      return Reference
   is
   begin
      return This : constant Reference := new TTY_Instance'
        (Parent with null record)
      do
         This.Initialize (Command, Bus);
      end return;
   end Load;

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (This : in out TTY_Instance; Address : Address_Type; Value : Word_8)
   is
      pragma Unreferenced (This);
   begin
      if Address = 3 then
         Ada.Text_IO.Put (Character'Val (Value));
      end if;
   end Set_Word_8;

end Aqua.Devices.TTY;
