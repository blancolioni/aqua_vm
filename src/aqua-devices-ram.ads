with Aqua.Commands;

package Aqua.Devices.RAM is

   function Load
     (Command : Aqua.Commands.Command_Line;
      Bus     : not null access Aqua.Addressable.Instance'Class)
      return Reference;

end Aqua.Devices.RAM;
