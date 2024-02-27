package Aqua is

   Word_Size      : constant := 32;
   Word_8s_Per_Word : constant := Word_Size / 8;

   type Word_64 is mod 2 ** 64;
   type Word_32 is mod 2 ** 32;
   type Word_16 is mod 2 ** 16;
   type Word_8  is mod 2 ** 8;

   subtype Word is Word_32;
   subtype Address_Type is Word;

   Register_Count : constant := 256;

   type Register_Index is new Word_8;

   Word_16_Address_Mask : constant Address_Type := 16#FFFF_FFFE#;
   Word_32_Address_Mask : constant Address_Type := 16#FFFF_FFFC#;

   type Any_Data_Size is
     (Word_8_Size, Word_16_Size, Word_32_Size, Word_64_Size);

   subtype Data_Size is Any_Data_Size range Word_8_Size .. Word_32_Size;
   subtype Float_Data_Size is Any_Data_Size range Word_32_Size .. Word_64_Size;

   Float_32_Size : constant Float_Data_Size := Word_32_Size;
   Float_64_Size : constant Float_Data_Size := Word_64_Size;

   Address_Size : constant Data_Size := Word_32_Size;

   Data_Word_8s : constant array (Any_Data_Size) of Word :=
                   [1, 2, 4, 8];

   type Bit_Index is range 0 .. 63;

   subtype Word_Bit is Bit_Index range 0 .. 31;

   type Array_Of_Words is array (Positive range <>) of Word;

   type Relocation_Context is (No_Context, Relative_XY, Relative_XYZ);

   function Get_Bits
     (Value : Word;
      Start : Word_Bit;
      Count : Word_Bit)
      return Word;

   procedure Set
     (Target : in out Word;
      Size   : in     Data_Size;
      Value  : in     Word);

   function Get
     (Source : Word;
      Size   : Data_Size)
      return Word with Inline_Always;

   type Debugger_Interface is interface;

   function Get_Module_Name
     (This    : Debugger_Interface;
      Address : Address_Type)
      return String
      is abstract;

   function To_Module_Local_Address
     (This    : Debugger_Interface;
      Address : Address_Type)
      return Address_Type
      is abstract;

end Aqua;
