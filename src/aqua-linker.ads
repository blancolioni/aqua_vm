with Aqua.OS;

package Aqua.Linker is

   function Load
     (OS   : not null access Aqua.OS.Instance'Class;
      Path : String)
      return Address_Type;
   --  load module from path, and return the start address

   procedure Load
     (OS   : not null access Aqua.OS.Instance'Class;
      Path : String);

   procedure Resolve
     (OS     : not null access Aqua.OS.Instance'Class;
      Offset : Address_Type;
      Target : Address_Type);

end Aqua.Linker;
