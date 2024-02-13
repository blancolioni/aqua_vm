with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Directories;

package body Aqua.Files is

   package Path_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   Object_Paths : Path_Lists.List := ["./share/aqua_vm/obj"];

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (Name   : String)
      return String
   is
   begin
      for Path of Object_Paths loop
         declare
            File_Path : constant String :=
                          Ada.Directories.Compose (Path, Name);
         begin
            if Ada.Directories.Exists (File_Path) then
               return File_Path;
            end if;
         end;
      end loop;
      return "";
   end Find_File;

   ----------------------
   -- Set_Object_Paths --
   ----------------------

   procedure Set_Object_Paths
     (Paths : String)
   is
   begin
      Object_Paths.Append (Paths);
   end Set_Object_Paths;

end Aqua.Files;
