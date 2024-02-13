package Aqua.Images is

   function Hex_Image
     (Value : Word;
      Bytes : Natural := 4)
      return String;

   function Hex_Image
     (Value : Word;
      Size  : Data_Size)
      return String;

   function Octal_Image
     (Value : Word)
      return String;

   function Hex_Image
     (Value : Word_8)
      return String;

end Aqua.Images;
