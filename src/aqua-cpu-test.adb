with Aqua.Images;
with Aqua.Logging;

package body Aqua.CPU.Test is

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This : in out Instance'Class;
      Code : Word_32)
   is
   begin
      This.Execute (Code);
   end Execute;

   ------------------
   -- Get_Register --
   ------------------

   function Get_Register (This : Instance'Class; Local : Word_8) return Word is
   begin
      return This.Get_R (Register_Index (Local));
   end Get_Register;

   ---------------
   -- Log_State --
   ---------------

   procedure Log_State
     (This : Instance'Class)
   is
   begin
      Aqua.Logging.Log
        ("G=" & Images.Hex_Image (Word_8 (This.Get_G (G_Global)))
         & " L=" & Images.Hex_Image (Word_8 (This.Get_G (G_Local)))
         & " O=" & Images.Hex_Image (This.Get_G (G_Offset), 2)
         & " S=" & Images.Hex_Image (This.Get_G (G_Stack), 2));
   end Log_State;

   -------------------
   -- Pop_Registers --
   -------------------

   procedure Pop_Registers
     (This : in out Instance'Class;
      R    : Word_8)
   is
   begin
      This.Pop (Register_Index (R));
   end Pop_Registers;

   --------------------
   -- Push_Registers --
   --------------------

   procedure Push_Registers
     (This : in out Instance'Class;
      R    : Word_8)
   is
   begin
      This.Push (Register_Index (R));
   end Push_Registers;

   -----------
   -- Put_R --
   -----------

   procedure Put_R
     (This  : in out Instance'Class;
      R     : Word_8;
      Value : Word)
   is
   begin
      This.Set_G (Register_Index (R), Value);
   end Put_R;

   ------------------
   -- Set_Register --
   ------------------

   procedure Set_Register
     (This  : in out Instance'Class;
      Local : Word_8;
      Value : Word)
   is
   begin
      This.Set_R (Register_Index (Local), Value);
   end Set_Register;

end Aqua.CPU.Test;
