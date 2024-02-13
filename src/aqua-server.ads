private with Aqua.Bus;
private with Aqua.CPU;
private with Aqua.OS;

package Aqua.Server is

   type Instance is tagged private;

   type Reference is access all Instance'Class;

   function Create
     (Configuration_Path : String;
      Object_Paths       : String)
      return Reference;

   procedure Load
     (This : in out Instance;
      Path : String);

   procedure Run
     (This  : in out Instance;
      Trace : Boolean := False);

private

   type Instance is tagged
      record
         Bus   : Aqua.Bus.Reference;
         CPU   : Aqua.CPU.Reference;
         OS    : Aqua.OS.Reference;
         Start : Aqua.Address_Type;
      end record;

end Aqua.Server;