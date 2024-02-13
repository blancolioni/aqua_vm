with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with Aqua.Bus;
with Aqua.CPU;
with Aqua.Linker;
with Aqua.Loader;
with Aqua.OS;

with Aqua.Tests;

procedure Aqua.Driver is
begin
   if Ada.Command_Line.Argument_Count >= 1 then
      if not Ada.Directories.Exists (Ada.Command_Line.Argument (1)) then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Ada.Command_Line.Argument (1) & ": no such file or directory");
         Ada.Command_Line.Set_Exit_Status (1);
         return;
      end if;

      declare
         Argc : constant Word_32 := 16#A000#;
         Argv : constant Word_32 := 16#A004#;
         Arg_Count : constant Natural := Ada.Command_Line.Argument_Count;
         Next : Word_32 := Argv +
                       4 * Word_32 (Ada.Command_Line.Argument_Count) + 4;

         Bus : constant Aqua.Bus.Reference := Aqua.Bus.Create_Bus;
         CPU  : constant Aqua.CPU.Reference := Aqua.Loader.Create_CPU (Bus);
      begin
         CPU.Set_Word_32 (Argc, Word_32 (Arg_Count));
         for I in 1 .. Ada.Command_Line.Argument_Count loop
            declare
               Arg : constant String :=
                       (if I = 1
                        then Ada.Directories.Base_Name
                          (Ada.Command_Line.Argument (I))
                        else Ada.Command_Line.Argument (I));
            begin
               CPU.Set_Word_32
                 (Argv + Word_32 (4 * (I - 1)), Next);
               for Ch of Arg loop
                  CPU.Set_Word_8 (Next, Character'Pos (Ch));
                  Next := Next + 1;
               end loop;
               CPU.Set_Word_8 (Next, 0);
               Next := Next + 1;
               while Next mod 4 /= 0 loop
                  CPU.Set_Word_8 (Next, 0);
                  Next := Next + 1;
               end loop;
            end;
         end loop;
         CPU.Set_Word_32
           (Argv + 4 * Word_32 (Ada.Command_Line.Argument_Count), 0);

         declare
            OS : constant Aqua.OS.Reference := Aqua.OS.Create (Bus);
         begin
            CPU.Attach_Memory_Manager (OS);
            Ada.Text_IO.Put_Line
              ("Loading: " & Ada.Command_Line.Argument (1));

            declare
               Start : constant Address_Type :=
                         Aqua.Linker.Load (OS, Ada.Command_Line.Argument (1));
            begin
               CPU.Start (Start);
            end;
         end;
      end;
   else
      declare
         CPU    : Aqua.CPU.Instance;
      begin
         Aqua.Tests.Test (CPU);
      end;
   end if;
end Aqua.Driver;
