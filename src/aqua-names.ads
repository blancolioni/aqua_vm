package Aqua.Names is

   function Get_Module_Name
     (From_Symbol : String)
      return String;

   function Get_Local_Name
     (From_Symbol : String)
      return String;

   function To_File_Name
     (Module_Name : String)
      return String;

end Aqua.Names;
