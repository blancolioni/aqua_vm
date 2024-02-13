with Aqua.Addressable;
with Aqua.Commands;

package Aqua.Devices.Loader is

   type Loader_Function is access
     function (Command : Aqua.Commands.Command_Line;
               Bus     : not null access Aqua.Addressable.Instance'Class)
               return Reference;

   procedure Register
     (Name   : String;
      Loader : Loader_Function);

   function Load
     (Command : String;
      Bus     : not null access Aqua.Addressable.Instance'Class)
      return Reference;

end Aqua.Devices.Loader;
