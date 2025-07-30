private with Aqua.Bus;
private with Aqua.CPU;
with Aqua.Devices;
private with Aqua.OS;

package Aqua.Server is

   type Instance is tagged private;

   type Reference is access all Instance'Class;

   function Create
     (Configuration_Path : String;
      Object_Paths       : String)
      return Reference;

   procedure Install_Device
     (This   : in out Instance;
      Base   : Address_Type;
      Bound  : Address_Type;
      Device : Aqua.Devices.Reference);

   procedure Load
     (This : in out Instance;
      Path : String);

   procedure Load
     (This    : in out Instance;
      Path    : String;
      On_Note : access procedure
        (Name : String;
         Tag  : Word_32;
         Description : String));

   function Get_Symbol_Address
     (This : Instance;
      Name : String)
      return Address_Type;

   procedure Run
     (This  : in out Instance;
      Trace : Boolean := False);

   procedure Run
     (This      : in out Instance;
      Start     : Address_Type;
      Arguments : Aqua.Array_Of_Words;
      Trace     : Boolean := False);

   function Exit_Status
     (This : Instance'Class)
      return Word_32;

private

   type Instance is tagged
      record
         Bus    : Aqua.Bus.Reference;
         CPU    : Aqua.CPU.Reference;
         OS     : Aqua.OS.Reference;
         Start  : Aqua.Address_Type;
         Status : Word_32 := 0;
      end record;

   function Get_Symbol_Address
     (This : Instance;
      Name : String)
      return Address_Type
   is (This.OS.Get_Symbol_Address (Name));

   function Exit_Status
     (This : Instance'Class)
      return Word_32
   is (This.Status);

end Aqua.Server;
