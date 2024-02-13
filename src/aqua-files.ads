package Aqua.Files is

   procedure Set_Object_Paths
     (Paths : String);

   function Find_File
     (Name   : String)
      return String;

end Aqua.Files;
