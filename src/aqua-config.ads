package Aqua.Config is

   procedure Load_Configuration
     (Path : String);

   procedure Iterate_Config
     (Section_Name : String;
      Process      : not null access
        procedure (Value : String));

   function Get_Config
     (Section_Name  : String;
      Default_Value : String := "")
      return String;

   function Get_Config
     (Section_Name  : String;
      Default_Value : Word_16 := 0)
      return Word_16;

end Aqua.Config;
