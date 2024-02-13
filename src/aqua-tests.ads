with Aqua.Addressable;
with Aqua.CPU;

package Aqua.Tests is

   function Create_Test_Memory (Size : Word) return Aqua.Addressable.Reference;

   procedure Test (CPU : in out Aqua.CPU.Instance'Class);

   procedure Run
     (CPU  : in out Aqua.CPU.Instance'Class;
      Argc : Word_32;
      Argv : Word_32;
      Path : String);

end Aqua.Tests;
