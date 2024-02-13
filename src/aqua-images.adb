package body Aqua.Images is

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image
     (Value : Word;
      Bytes : Natural := 4)
      return String
   is
      Hex_Digits : constant String := "0123456789ABCDEF";
      Result     : String (1 .. Bytes * 2);
      Acc        : Word := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digits (Natural (Acc mod 16) + 1);
         Acc := Acc / 16;
      end loop;
      return Result;
   end Hex_Image;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image
     (Value : Word_8)
      return String
   is
      Result : constant String := Hex_Image (Word (Value));
   begin
      return Result (Result'Last - 1 .. Result'Last);
   end Hex_Image;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image
     (Value : Word;
      Size  : Data_Size)
      return String
   is
      Result : constant String := Hex_Image (Value);
   begin
      case Size is
         when Word_8_Size =>
            return Result (7 .. 8);
         when Word_16_Size =>
            return Result (5 .. 8);
         when Word_32_Size =>
            return Result;
      end case;
   end Hex_Image;

   -----------------
   -- Octal_Image --
   -----------------

   function Octal_Image
     (Value : Word)
      return String
   is
      Octal_Digits : constant String := "01234567";
      Result       : String (1 .. 12);
      Acc          : Natural := Natural (Value);
   begin
      for I in reverse Result'Range loop
         Result (I) := Octal_Digits (Acc mod 8 + 1);
         Acc := Acc / 8;
      end loop;
      return Result;
   end Octal_Image;

end Aqua.Images;
