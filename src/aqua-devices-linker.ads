with Aqua.Commands;
with Aqua.Ld;

package Aqua.Devices.Linker is

   function Load
     (Command : Aqua.Commands.Command_Line;
      Bus     : not null access Aqua.Addressable.Instance'Class)
      return Reference;

   procedure Set_Manager
     (This    : Reference;
      Manager : not null access Aqua.Ld.Link_Manager'Class);

end Aqua.Devices.Linker;
