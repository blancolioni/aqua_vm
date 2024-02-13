with Aqua.Addressable;
with Aqua.Commands;

package Aqua.Devices is

   subtype Parent is Aqua.Addressable.Instance;
   type Instance is abstract new Parent with private;
   type Reference is access all Instance'Class;

   function Name (This : Instance) return String is abstract;

   procedure Initialize
     (This    : in out Instance'Class;
      Command : Aqua.Commands.Command_Line;
      Bus     : not null access Aqua.Addressable.Instance'Class);

private

   type Instance is abstract new Parent with
      record
         Bus : Aqua.Addressable.Reference;
      end record;

end Aqua.Devices;
