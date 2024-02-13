package body Aqua.Instruction is

   type Control_Chart is array (Word_8 range <>) of Control_Record;

   subtype Chart_Row is Control_Chart (0 .. 15);

   Bad : constant Control_Record :=
           (Bad_Instruction => True, others => <>);

   function ALU (Op : ALU_Operation;
                 Imm : Boolean;
                 Signed : Boolean)
                 return Control_Record
   is (Class          => ALU, ALU_Op => Op,
       Immediate_Z => Imm, Unsigned => not Signed,
       others => <>);

   function ALU (Op     : ALU_Operation)
                 return Control_Chart
   is [ALU (Op, False, True),
       ALU (Op, True, True),
       ALU (Op, False, False),
       ALU (Op, True, False)];

   function Logical_ALU (Op     : ALU_Operation)
                         return Control_Chart
   is [ALU (Op, False, False),
       ALU (Op, True, False)];

   function Branch
     (Condition : Condition_Type;
      Negated   : Boolean;
      Backwards : Boolean)
      return Control_Record
   is (Class => Flow, Conditional => True, Condition => Condition,
       Backwards => Backwards, Negated => Negated, others => <>);

   function Branches (Condition : Condition_Type;
                      Negated   : Boolean)
                      return Control_Chart
   is [Branch (Condition, Negated, False),
       Branch (Condition, Negated, True)];

   function Branches (Negated : Boolean) return Control_Chart
   is (Branches (Cond_Negative, Negated)
       & Branches (Cond_Zero, Negated)
       & Branches (Cond_Positive, Negated)
       & Branches (Cond_Odd, Negated));

   function Conditional
     (Condition   : Condition_Type;
      Negated     : Boolean;
      Zero_Or_Set : Boolean;
      Immediate   : Boolean)
      return Control_Record
   is (Class       => Conditional_Set, Condition => Condition,
       Zero_Or_Set => Zero_Or_Set,
       Negated     => Negated,
       Immediate_Z => Immediate,
       others      => <>);

   function Conditionals
     (Condition   : Condition_Type;
      Negated     : Boolean;
      Zero_Or_Set : Boolean)
      return Control_Chart
   is [Conditional (Condition, Negated, Zero_Or_Set, False),
       Conditional (Condition, Negated, Zero_Or_Set, True)];

   function Conditionals
     (Negated     : Boolean;
      Zero_Or_Set : Boolean)
      return Control_Chart
   is (Conditionals (Cond_Negative, Negated, Zero_Or_Set)
       & Conditionals (Cond_Zero, Negated, Zero_Or_Set)
       & Conditionals (Cond_Positive, Negated, Zero_Or_Set)
       & Conditionals (Cond_Odd, Negated, Zero_Or_Set));

   function Conditionals
     (Zero_Or_Set : Boolean)
      return Control_Chart
   is (Conditionals (False, Zero_Or_Set)
       & Conditionals (True, Zero_Or_Set));

   function Load (Imm      : Boolean;
                  Unsigned : Boolean;
                  Size     : Op_Size)
                  return Control_Record
   is (Class          => Load_Store, Load => True,
       Immediate_Z    => Imm, Unsigned => Unsigned, Size => Size,
       others         => <>);

   function Loads return Control_Chart
   is [Load (False, False, Size_8),
       Load (True, False, Size_8),
       Load (False, True, Size_8),
       Load (True, True, Size_8),
       Load (False, False, Size_16),
       Load (True, False, Size_16),
       Load (False, True, Size_16),
       Load (True, True, Size_16),
       Load (False, False, Size_32),
       Load (True, False, Size_32),
       Load (False, True, Size_32),
       Load (True, True, Size_32),
       Bad,
       Bad,
       Bad,
       Bad];

   function Jump (Backward : Boolean) return Control_Record
   is (Class     => Flow, Relative_Addr => True,
       Backwards => Backward, others => <>);

   function Pushj (Backward : Boolean) return Control_Record
   is (Class     => Flow, Relative_Addr => True,
       Backwards => Backward, Push  => True,
       others => <>);

   function Geta (Backward : Boolean) return Control_Record is
     (Class => Load_Store, Load => True, Relative_Addr => True,
      Backwards => Backward, others => <>);

   function Jumps return Control_Chart
   is [Jump (False), Jump (True)];

   function Pushjs return Control_Chart
   is [Pushj (False), Pushj (True)];

   function Getas return Control_Chart
   is [Geta (False), Geta (True)];

   function Store (Imm      : Boolean;
                   Unsigned : Boolean;
                   Size : Op_Size)
                   return Control_Record
   is (Class          => Load_Store, Load => False,
       Immediate_Z => Imm, Unsigned => Unsigned, Size => Size,
       others => <>);

   function YZ_Imm (Op : YZ_Immediate_Operation;
                    High : Boolean)
                    return Control_Record
   is (Class => YZ_Immediate, YZ_Op => Op, YZ_High => High, others => <>);

   function YZ_Imm_Row return Control_Chart
   is [YZ_Imm (Op_Set, True), YZ_Imm (Op_Set, False),
       Bad, Bad,
       YZ_Imm (Op_Inc, True), YZ_Imm (Op_Inc, False),
       Bad, Bad,
       YZ_Imm (Op_Or, True), YZ_Imm (Op_Or, False),
       Bad, Bad, Bad, Bad, Bad, Bad];

   function Stores return Control_Chart
   is [Store (False, False, Size_8),
       Store (True, False, Size_8),
       Store (False, True, Size_8),
       Store (True, True, Size_8),
       Store (False, False, Size_16),
       Store (True, False, Size_16),
       Store (False, True, Size_16),
       Store (True, True, Size_16),
       Store (False, False, Size_32),
       Store (True, False, Size_32),
       Store (False, True, Size_32),
       Store (True, True, Size_32),
       Bad,
       Bad,
       Bad,
       Bad];

   function Misc (Op : Miscellaneous_Instruction) return Control_Record
   is (Control_Record'
         (Class => Misc, Misc_Op => Op, others => <>));

   function Puts return Control_Chart
   is [Misc (Put), (Misc (Put) with delta Immediate_Z => True)];

   Chart : access Control_Chart := null;

   -----------------
   -- Get_Control --
   -----------------

   function Get_Control (Op : Word_8) return Control_Record is
   begin
      if Chart = null then
         declare
            Row_00 : constant Chart_Row :=
                       [(Class => Trap, others => <>), others => (Bad)];

            Row_10 : constant Chart_Row :=
                       Control_Chart'(0 .. 7 => Bad)
                     & ALU (Op_Mul) & ALU (Op_Div);

            Row_20 : constant Chart_Row :=
                       ALU (Op_Add) & ALU (Op_Sub)
                   & Control_Chart'(0 .. 7 => Bad);

            Row_30 : constant Chart_Row :=
                       ALU (Op_Cmp) & ALU (Op_Neg)
                   & ALU (Op_Sl) & ALU (Op_Sr);

            Row_40 : constant Chart_Row :=
                       Branches (False) & Branches (True);

            Row_50 : constant Chart_Row := [others => (Bad)];

            Row_60 : constant Chart_Row := Conditionals (Zero_Or_Set => False);

            Row_70 : constant Chart_Row := Conditionals (Zero_Or_Set => True);

            Row_80 : constant Chart_Row := Loads;

            Row_90 : constant Chart_Row :=
                      [14 => (Class => Flow, others => <>),
                       15 => (Class  => Flow, Immediate_Z => True,
                              others => <>),
                       others    => Bad];

            Row_A0 : constant Chart_Row := Stores;

           Row_B0 : constant Chart_Row :=
                      [15 => (Class       => Flow, Push => True,
                              Immediate_Z => True, others => <>),
                       14 => (Class  => Flow, Push => True, others => <>),
                       others => (Bad)];

            Row_C0 : constant Chart_Row :=
                       Logical_ALU (Op_Or)
                     & Control_Chart'(Bad, Bad)
                     & Logical_ALU (Op_Nor)
                     & Logical_ALU (Op_Xor)
                     & Logical_ALU (Op_And)
                     & Control_Chart'(Bad, Bad)
                     & Logical_ALU (Op_Nand)
                     & Control_Chart'(Bad, Bad);

            Row_D0 : constant Chart_Row := [others => (Bad)];

            Row_E0 : constant Chart_Row := YZ_Imm_Row;

            Row_F0 : constant Chart_Row :=
                       Jumps & Pushjs & Getas & Puts
                       & Misc (Pop) & Misc (Resume)
                       & Control_Chart'(10 .. 13 => Bad)
                       & Misc (Get) & Bad;

         begin
            Chart := new Control_Chart'
              (Row_00
               & Row_10
               & Row_20
               & Row_30
               & Row_40
               & Row_50
               & Row_60
               & Row_70
               & Row_80
               & Row_90
               & Row_A0
               & Row_B0
               & Row_C0
               & Row_D0
               & Row_E0
               & Row_F0);
         end;
      end if;
      return Chart (Op);
   end Get_Control;

   ----------
   -- Show --
   ----------

   function Show (Control : Control_Record) return String is

      function Condition_Image return String
      is (case Control.Condition is
             when Cond_Negative => (if Control.Negated then "NN" else "N"),
             when Cond_Zero => (if Control.Negated then "NZ" else "Z"),
             when Cond_Positive => (if Control.Negated then "NP" else "P"),
             when Cond_Odd      => (if Control.Negated then "EV" else "OD"));

   begin
      case Control.Class is
         when Undefined =>
            return "*UNDEFINED*";
         when Trap =>
            return "TRAP";
         when ALU =>
            declare
               Op_Img : constant String := Control.ALU_Op'Image;
            begin
               return Op_Img (4 .. Op_Img'Last)
                 & (if Control.Unsigned then "U" else "")
                 & (if Control.Immediate_Z then "I" else "");
            end;
         when Load_Store =>
            if Control.Load then
               return "LD"
                 & (if Control.Unsigned then "U" else "")
                 & (if Control.Immediate_Z then "I" else "")
                 & (case Control.Size is
                       when Size_8 => "B",
                       when Size_16 => "H",
                       when Size_32 => "");
            else
               return "ST"
                 & (if Control.Unsigned then "U" else "")
                 & (if Control.Immediate_Z then "I" else "")
                 & (case Control.Size is
                       when Size_8  => "B",
                       when Size_16 => "H",
                       when Size_32 => "");
            end if;
         when Flow =>
            if Control.Conditional then
               return "B"
                 & Condition_Image
                 & (if Control.Backwards then "B" else "");
            else
               return "JMP";
            end if;
         when Conditional_Set =>
            return (if Control.Zero_Or_Set
                    then "ZS" & Condition_Image
                    else "CS" & Condition_Image);

         when YZ_Immediate =>
            declare
               Op_Img : constant String := Control.YZ_Op'Image;
            begin
               return Op_Img (4 .. Op_Img'Last)
                 & (if Control.YZ_High then "H" else "L");
            end;
         when Misc =>
            if Control.Pop then
               return "POP";
            else
               return "*undefined-misc*";
            end if;
      end case;
   end Show;

end Aqua.Instruction;
