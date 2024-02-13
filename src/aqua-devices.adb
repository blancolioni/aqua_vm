package body Aqua.Devices is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This    : in out Instance'Class;
      Command : Aqua.Commands.Command_Line;
      Bus     : not null access Aqua.Addressable.Instance'Class)
   is
   begin
      This.Initialize (Base => Command.Argument ("base"),
                       Bound => Command.Argument ("bound"));
      This.Bus := Aqua.Addressable.Reference (Bus);
   end Initialize;

end Aqua.Devices;
