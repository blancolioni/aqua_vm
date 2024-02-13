with Ada.Text_IO;

with Aqua.Config;
with Aqua.Images;
with Aqua.Options;

with Aqua.Devices.Loader;

package body Aqua.Loader is

   ----------------
   -- Create_CPU --
   ----------------

   function Create_CPU
     (Bus                : Aqua.Bus.Reference;
      Configuration_Path : String := "")
      return Aqua.CPU.Reference
   is
   begin
      if Configuration_Path = "" then
         Aqua.Config.Load_Configuration (".aqua-config");
      else
         Aqua.Config.Load_Configuration (Configuration_Path);
      end if;

      declare
         procedure Install_Device (Command : String);

         --------------------
         -- Install_Device --
         --------------------

         procedure Install_Device (Command : String) is
            Device : constant Aqua.Devices.Reference :=
                    Aqua.Devices.Loader.Load (Command, Bus);
         begin
            if True or else Aqua.Options.Show_Devices then
               Ada.Text_IO.Put (Device.Name);
               Ada.Text_IO.Set_Col (20);
               Ada.Text_IO.Put (Aqua.Images.Hex_Image (Device.Base));
               Ada.Text_IO.Set_Col (30);
               Ada.Text_IO.Put (Aqua.Images.Hex_Image (Device.Bound - 1));
               Ada.Text_IO.New_Line;
            end if;

            Bus.Install (Device);
         end Install_Device;

      begin
         Aqua.Config.Iterate_Config ("device", Install_Device'Access);

         return This : constant Aqua.CPU.Reference := new Aqua.CPU.Instance do
            This.Attach_Bus (Bus);
         end return;
      end;

   end Create_CPU;

end Aqua.Loader;
