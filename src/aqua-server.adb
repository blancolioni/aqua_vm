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
         This.CPU.Attach_Debugger (This.OS);
      end return;
   end Create;

   --------------------
   -- Install_Device --
   --------------------

   procedure Install_Device
     (This   : in out Instance;
      Base   : Address_Type;
      Bound  : Address_Type;
      Device : Aqua.Devices.Reference)
   is
   begin
      Device.Initialize (Base, Bound);
      This.Bus.Install (Device);
   end Install_Device;

   ----------
   -- Load --
   ----------

   procedure Load
     (This : in out Instance;
      Path : String)
   is
   begin
      This.Load (Path, null);
   end Load;

   ----------
   -- Load --
   ----------

   procedure Load
     (This    : in out Instance;
      Path    : String;
      On_Note : access procedure
        (Name : String;
         Tag  : Word_32;
         Description : String))
   is
   begin
      This.Start := Aqua.Linker.Load (This.OS, Path, On_Note);
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
      --  Aqua.OS.Trace_Loading (Trace);
      This.CPU.Start (This.Start);
      This.Status := This.CPU.Exit_Status;
   end Run;

   ---------
   -- Run --
   ---------

   procedure Run
     (This      : in out Instance;
      Start     : Address_Type;
      Arguments : Aqua.Array_Of_Words;
      Trace     : Boolean := False)
   is
   begin
      Aqua.CPU.Trace (Trace);
      --  Aqua.OS.Trace_Loading (Trace);
      This.CPU.Start (Start, Arguments);
      This.Status := This.CPU.Exit_Status;
   end Run;

end Aqua.Server;
