package Aqua.CPU.Test is

   procedure Set_Register
     (This : in out Instance'Class;
      Local : Word_8;
      Value : Word);

   function Get_Register
     (This  : Instance'Class;
      Local : Word_8)
      return Word;

   procedure Put_R
     (This  : in out Instance'Class;
      R     : Word_8;
      Value : Word);

   procedure Push_Registers
     (This : in out Instance'Class;
      R    : Word_8);

   procedure Pop_Registers
     (This : in out Instance'Class;
      R    : Word_8);

   procedure Log_State
     (This : Instance'Class);

   procedure Execute
     (This : in out Instance'Class;
      Code : Word_32);

end Aqua.CPU.Test;
