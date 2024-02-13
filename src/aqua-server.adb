with Aqua.Files;
with Aqua.Linker;
with Aqua.Loader;

package body Aqua.Server is

   ------------
   -- Create --
   ------------

   function Create
     (Configuration_Path : String;
      Object_Paths       : String)
      return Reference
   is
   begin
      Aqua.Files.Set_Object_Paths (Object_Paths);
      return This : constant Reference := new Instance do
         This.Bus := Aqua.Bus.Create_Bus;
         This.CPU := Aqua.Loader.Create_CPU (This.Bus, Configuration_Path);
         This.OS := Aqua.OS.Create (This.Bus);
         This.CPU.Attach_Memory_Manager (This.OS);
      end return;
   end Create;

   ----------
   -- Load --
   ----------

   procedure Load
     (This : in out Instance;
      Path : String)
   is
   begin
      This.Start := Aqua.Linker.Load (This.OS, Path);
   end Load;

   ---------
   -- Run --
   ---------

   procedure Run
     (This  : in out Instance;
      Trace : Boolean := False)
   is
   begin
      Aqua.CPU.Trace (Trace);
      Aqua.OS.Trace_Loading (Trace);
      This.CPU.Start (This.Start);
   end Run;

end Aqua.Server;
