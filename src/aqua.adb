package body Aqua is

   ---------
   -- Get --
   ---------

   function Get
     (Source : Word;
      Size   : Data_Size)
      return Word
   is
   begin
      case Size is
         when Word_8_Size =>
            return Source and 16#0000_00FF#;
         when Word_16_Size =>
            return Source and 16#0000_FFFF#;
         when Word_32_Size =>
            return Source;
      end case;
   end Get;

   --------------
   -- Get_Bits --
   --------------

   function Get_Bits
     (Value : Word;
      Start : Word_Bit;
      Count : Word_Bit)
      return Word
   is
      Result : Word := Value;
   begin
      Result := Result and (2 ** Natural (Start + 1) - 1);
      Result := Result / 2 ** Natural (Start + 1 - Count);
      return Result;
   end Get_Bits;

   ---------
   -- Set --
   ---------

   procedure Set
     (Target : in out Word;
      Size   : in     Data_Size;
      Value  : in     Word)
   is
   begin
      case Size is
         when Word_8_Size =>
            Target := (Target and 16#FFFF_FF00#)
              or (Value and 16#0000_00FF#);
         when Word_16_Size =>
            Target := (Target and 16#FFFF_0000#)
              or (Value and 16#0000_FFFF#);
         when Word_32_Size =>
            Target := Value;
      end case;
   end Set;

end Aqua;
