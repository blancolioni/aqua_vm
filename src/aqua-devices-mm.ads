with Aqua.Commands;
with Aqua.MM;

package Aqua.Devices.MM is

   function Load
     (Command : Aqua.Commands.Command_Line;
      Bus     : not null access Aqua.Addressable.Instance'Class)
      return Reference;

   procedure Set_Manager
     (This    : Reference;
      Manager : not null access Aqua.MM.Memory_Manager'Class);

end Aqua.Devices.MM;
