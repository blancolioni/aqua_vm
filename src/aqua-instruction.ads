package Aqua.Instruction is

   type Control_Class is
     (Undefined, Trap, ALU, Load_Store, Flow, Conditional_Set,
      YZ_Immediate, Misc);

   type Op_Size is (Size_8, Size_16, Size_32);

   type ALU_Operation is
     (Op_Add, Op_Sub, Op_Mul, Op_Div,
      Op_Neg, Op_And, Op_Or, Op_Xor,
      Op_Nand, Op_Nor,
      Op_Cmp, Op_Sl, Op_Sr);

   type Condition_Type is
     (Cond_Negative, Cond_Zero, Cond_Positive, Cond_Odd);

   type YZ_Immediate_Operation is
     (Op_Set, Op_Inc, Op_And, Op_Or);

   type Miscellaneous_Instruction is
     (Get, Pop, Put, Resume);

   type Control_Record is
      record
         Bad_Instruction : Boolean := False;
         Immediate_Z     : Boolean := False;
         Z_Is_Source     : Boolean := False;
         Immediate_YZ    : Boolean := False;
         Y_Is_Source     : Boolean := False;
         X_Is_Source     : Boolean := False;
         X_Is_Dest       : Boolean := False;
         Relative_Addr   : Boolean := False;
         Backwards       : Boolean := False;
         Memory          : Boolean := False;
         Unsigned        : Boolean := False;
         Floating_Point  : Boolean := False;
         Conditional     : Boolean := False;
         Zero_Or_Set     : Boolean := False;
         Negated         : Boolean := False;
         Load            : Boolean := False;
         Push            : Boolean := False;
         Pop             : Boolean := False;
         Size            : Op_Size := Size_32;
         Class           : Control_Class := Undefined;
         ALU_Op          : ALU_Operation := Op_Add;
         YZ_Op           : YZ_Immediate_Operation := Op_Set;
         YZ_High         : Boolean := False;
         Condition       : Condition_Type := Cond_Zero;
         Misc_Op         : Miscellaneous_Instruction := Get;
      end record;

   function Get_Control (Op : Word_8) return Control_Record;

   function Show (Control : Control_Record) return String;

end Aqua.Instruction;
