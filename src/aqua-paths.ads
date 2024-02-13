package Aqua.Paths is

   Config_Path : constant String :=
     "./share/aqua_vm";

   function Config_File
     (File_Path : String)
     return String
   is (Config_Path & "/" & File_Path);

end Aqua.Paths;
