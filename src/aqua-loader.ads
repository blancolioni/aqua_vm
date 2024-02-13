with Aqua.Bus;
with Aqua.CPU;

package Aqua.Loader is

   function Create_CPU
     (Bus                : Aqua.Bus.Reference;
      Configuration_Path : String := "")
      return Aqua.CPU.Reference;

end Aqua.Loader;
