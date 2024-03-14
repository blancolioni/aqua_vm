with Ada.Exceptions;
with Ada.Text_IO;

with Aqua.Bus;
with Aqua.Images;
with Aqua.Ld;
with Aqua.Logging;

with Aqua.Instruction;

package body Aqua.CPU is

   use Aqua.Instruction;

   Enable_Trace : Boolean := False;

   type Instruction_Record is
      record
         Op, X, Y, Z : Word_8;
      end record;

   function Split (IR : Word_32) return Instruction_Record;

   procedure Execute_Trap
     (This    : in out Instance'Class;
      X, Y, Z : Word_8);

   function Execute_ALU
     (This     : in out Instance'Class;
      Y, Z     : Word_32;
      Op       : Instruction.ALU_Operation;
      Unsigned : Boolean)
      return Word_32;

   procedure Execute_Branch
     (This      : in out Instance'Class;
      Value     : Word_32;
      Condition : Condition_Type;
      Negated   : Boolean;
      Backwards : Boolean;
      Offset    : Word_32);

   procedure Execute_Load
     (This        : in out Instance'Class;
      Address     : Address_Type;
      Size        : Op_Size;
      Unsigned    : Boolean;
      Destination : out Word_32);

   procedure Execute_Store
     (This     : in out Instance'Class;
      Address  : Address_Type;
      Size     : Op_Size;
      Unsigned : Boolean;
      Source   : Word_32);

   function To_Window_Index
     (This : Instance'Class;
      R    : Register_Index)
      return Window_Register_Index
   is (Window_Register_Index
       ((This.State.G_O / 4 + Word (R)) mod Register_Window_Size));

   type Default_Memory_Map is
     new Aqua.MM.Memory_Manager with null record;

   overriding function Map
     (This    : in out Default_Memory_Map;
      Address : Address_Type;
      Read    : Boolean;
      Write   : Boolean;
      Execute : Boolean)
      return Address_Type
   is (Address);

   overriding function Request_Heap
     (This : in out Default_Memory_Map;
      Size : Word_32)
      return Address_Type
   is (0);

   Local_Default_Memory_Map : aliased Default_Memory_Map;

   function Default_Memory_Manager return access Aqua.MM.Memory_Manager'Class
   is (Local_Default_Memory_Map'Access);

   function Local_Register_Name (R : Register_Index) return String;
   function Global_Register_Name (R : Register_Index) return String;

   ----------------
   -- Attach_Bus --
   ----------------

   procedure Attach_Bus
     (This : in out Instance'Class;
      Bus  : not null access Aqua.Addressable.Instance'Class)
   is
   begin
      This.Bus := Aqua.Addressable.Reference (Bus);
   end Attach_Bus;

   ---------------------
   -- Attach_Debugger --
   ---------------------

   procedure Attach_Debugger
     (This      : in out Instance'Class;
      Debugger  : not null access Debugger_Interface'Class)
   is
   begin
      This.Debugger := Debugger_Reference (Debugger);
   end Attach_Debugger;

   ---------------------------
   -- Attach_Memory_Manager --
   ---------------------------

   procedure Attach_Memory_Manager
     (This   : in out Instance'Class;
      Memory : not null access Aqua.MM.Memory_Manager'Class)
   is
   begin
      This.Memory := Memory_Manager_Reference (Memory);
      This.Mapping := True;
   end Attach_Memory_Manager;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This : in out Instance'Class;
      IR   : Word_32)
   is
      Rec     : constant Instruction_Record := Split (IR);
      Control : constant Aqua.Instruction.Control_Record :=
                  Aqua.Instruction.Get_Control (Rec.Op);
   begin
      case Control.Class is
         when Undefined =>
            raise Bad_Instruction with
              "bad instruction: " & Aqua.Images.Hex_Image (IR);
         when Trap =>
            This.Execute_Trap (Rec.X, Rec.Y, Rec.Z);
         when ALU =>
            declare
               ZZ : constant Word_32 :=
                      (if Control.Immediate_Z
                       then Word_32 (Rec.Z)
                       else This.Get_R (Register_Index (Rec.Z)));
               YY : constant Word_32 :=
                      This.Get_R (Register_Index (Rec.Y));
               XX : constant Word_32 :=
                      This.Execute_ALU (YY, ZZ,
                                        Control.ALU_Op, Control.Unsigned);
            begin
               This.Set_R (Register_Index (Rec.X), XX);
               This.Trace.Update (Rec.X, XX);
            end;
         when Load_Store =>
            if Control.Relative_Addr then
               declare
                  YZ      : constant Word_32 :=
                              Word_32 (Rec.Y) * 256 + Word_32 (Rec.Z);
                  Address : constant Word_32 :=
                              This.State.PC - 4
                                + (4 * YZ
                                   - (if Control.Backwards
                                     then 262144 else 0));
               begin
                  This.Set_R (Register_Index (Rec.X), Address);
                  This.Trace.Update (Rec.X, Address);
               end;
            else
               declare
                  ZZ      : constant Word_32 :=
                              (if Control.Immediate_Z
                               then Word_32 (Rec.Z) * 4
                               else This.Get_R (Register_Index (Rec.Z)));
                  YY      : constant Word_32 :=
                              This.Get_R (Register_Index (Rec.Y));
                  XX      : Word_32;
                  Address : constant Address_Type := Address_Type (YY + ZZ);
               begin
                  if Control.Load then
                     This.Execute_Load
                       (Address, Control.Size, Control.Unsigned, XX);
                     This.Set_R (Register_Index (Rec.X), XX);
                     This.Trace.Fetch (Rec.X, Address, XX);
                  else
                     XX := This.Get_R (Register_Index (Rec.X));
                     This.Execute_Store
                       (Address, Control.Size, Control.Unsigned, XX);
                     This.Trace.Store (Rec.X, Address, XX);
                  end if;
               end;
            end if;
         when Conditional_Set =>
            declare
               YY : constant Word_32 :=
                      This.Get_R (Register_Index (Rec.Y));
               ZZ : constant Word_32 :=
                      (if Control.Immediate_Z
                       then Word_32 (Rec.Z)
                       else This.Get_R (Register_Index (Rec.Z)));
               Sat : constant Boolean :=
                       Control.Negated xor
                           (case Control.Condition is
                               when Cond_Negative =>
                                 YY >= 16#1000_0000#,
                               when Cond_Zero     =>
                                 YY = 0,
                               when Cond_Positive =>
                                 YY in 1 .. 16#1000_0000#,
                               when Cond_Odd      =>
                                 YY mod 2 = 1);
            begin
               Aqua.Logging.Log
                 ("negated " & Control.Negated'Image
                  & "; cond " & Control.Condition'Image
                  & "; yy " & Aqua.Images.Hex_Image (YY)
                  & "; zz " & Aqua.Images.Hex_Image (ZZ)
                  & "; sat " & Sat'Image);

               if Sat then
                  This.Set_R (Register_Index (Rec.X), ZZ);
                  This.Trace.Update (Rec.X, ZZ);
               elsif Control.Zero_Or_Set then
                  This.Set_R (Register_Index (Rec.X), 0);
                  This.Trace.Update (Rec.X, 0);
               end if;
            end;

         when Flow =>
            if Control.Conditional then
               declare
                  XX : constant Word_32 :=
                         This.Get_R (Register_Index (Rec.X));
                  YZ : constant Word_32 :=
                         Word_32 (Rec.Y) * 256 + Word_32 (Rec.Z);
               begin
                  This.Execute_Branch
                    (Value     => XX,
                     Condition => Control.Condition,
                     Negated   => Control.Negated,
                     Backwards => Control.Backwards,
                     Offset    => YZ);
               end;
            elsif Control.Relative_Addr then
               if Control.Push then
                  declare
                     YZ : constant Word_32 :=
                            Word_32 (Rec.Y) * 256 + Word_32 (Rec.Z);
                     Address : constant Word_32 :=
                                 This.State.PC - 4
                                   + (4 * YZ
                                      - (if Control.Backwards
                                        then 2 ** 18 else 0));
                  begin
                     This.State.G_J := This.State.PC;
                     This.State.Level := This.State.Level + 1;
                     Aqua.Logging.Log
                       ("pushj "
                        & Aqua.Images.Hex_Image (Address));
                     This.Push (Register_Index (Rec.X));
                     This.State.PC := Address;
                     if Enable_Trace then
                        for R in Register_Index range 0 .. 2 loop
                           declare
                              Img : String := R'Image;
                           begin
                              Img (Img'First) := '%';
                              Aqua.Logging.Log
                                (Img & " "
                                 & Aqua.Images.Hex_Image (This.Get_R (R)));
                           end;
                        end loop;
                     end if;
                  end;
               else
                  declare
                     XYZ     : constant Word_32 :=
                                 Word_32 (Rec.X) * 65536
                                 + Word_32 (Rec.Y) * 256
                                    + Word_32 (Rec.Z);
                     Address : constant Word_32 :=
                                 This.State.PC - 4
                                   + (4 * XYZ
                                      - (if Control.Backwards
                                        then 2 ** 26 else 0));
                  begin
                     This.State.PC := Address;
                  end;
               end if;
            else
               declare
                  ZZ      : constant Word_32 :=
                              (if Control.Immediate_Z
                               then Word_32 (Rec.Z)
                               else This.Get_R (Register_Index (Rec.Z)));
                  YY      : constant Word_32 :=
                              This.Get_R (Register_Index (Rec.Y));
                  Address : constant Address_Type := Address_Type (YY + ZZ);
               begin
                  if Control.Push then
                     This.State.G_J := This.State.PC;
                     This.State.Level := This.State.Level + 1;
                     Aqua.Logging.Log
                       ("pushgo "
                        & Aqua.Images.Hex_Image (Address));
                     This.Push (Register_Index (Rec.X));
                     This.State.PC := Address;

                     if Enable_Trace then
                        for R in Register_Index range 0 .. 4 loop
                           declare
                              Img : String := R'Image;
                           begin
                              Img (Img'First) := '%';
                              Aqua.Logging.Log
                                (Img & " "
                                 & Aqua.Images.Hex_Image (This.Get_R (R)));
                           end;
                        end loop;
                     end if;

                  else
                     This.Set_R (Register_Index (Rec.X), This.State.PC);
                     This.State.PC := Word_32 (Address);
                  end if;
               end;
            end if;
         when YZ_Immediate =>
            declare
               High  : constant Boolean := Control.YZ_High;
               Shift : constant Natural := (if High then 16 else 0);
               Mask  : constant Word_32 :=
                         (if High then 16#0000_FFFF# else 16#FFFF_0000#);

               YZ : constant Word_16 :=
                      Word_16 (Rec.Z) + 256 * Word_16 (Rec.Y);
               XX : Word_32 :=
                      This.Get_R (Register_Index (Rec.X));
               W  : Word_16 :=
                         Word_16 (XX / 2 ** Shift mod 2 ** 16);
            begin
               case Control.YZ_Op is
                  when Op_Set =>
                     W := YZ;
                  when Op_Inc =>
                     XX := XX + Word_32 (YZ) * 2 ** Shift;
                  when Op_And =>
                     W := W and YZ;
                  when Op_Or =>
                     W := W or YZ;
               end case;

               if Control.YZ_Op = Op_Set then
                  This.Set_R (Register_Index (Rec.X),
                              Word_32 (W) * 2 ** Shift);
               elsif Control.YZ_Op = Op_Inc then
                  This.Set_R (Register_Index (Rec.X), XX);
               else
                  This.Set_R (Register_Index (Rec.X),
                              (XX and Mask) or Word_32 (W) * 2 ** Shift);
               end if;
            end;
         when Misc =>
            case Control.Misc_Op is
               when Get =>
                  declare
                     R : constant Register_Index :=
                           Register_Index (Rec.X);
                     G : constant Register_Index :=
                           Register_Index (Rec.Z mod 32);
                     X : constant Word_32 := This.Get_G (G);
                  begin
                     This.Set_R (R, X);
                     This.Trace.Update (Rec.X, X);
                  end;
               when Pop =>
                  Aqua.Logging.Log ("pop: rJ = "
                                    & Aqua.Images.Hex_Image (This.State.G_J));
                  This.Pop (Register_Index (Rec.X));
                  This.State.PC := This.State.G_J;
                  This.State.Level := Integer'Max (This.State.Level - 1, 0);
               when Put =>
                  declare
                     ZZ : constant Word_32 :=
                            (if Control.Immediate_Z
                             then 256 * Word_32 (Rec.Y) + Word_32 (Rec.Z)
                             else This.Get_R (Register_Index (Rec.Z)));
                  begin
                     Aqua.Logging.Log
                       (Global_Register_Name
                          (Register_Index (Rec.X))
                        & " <- "
                        & (if Control.Immediate_Z
                          then Aqua.Images.Hex_Image (ZZ)
                          else Local_Register_Name
                            (Register_Index (Rec.Z)))
                        & " "
                        & Aqua.Images.Hex_Image (ZZ));
                     This.Set_G (Register_Index (Rec.X mod 32), ZZ);
                  end;

               when Resume =>
                  declare
                     Trap : constant Instruction_Record :=
                              Split (This.Get_G (G_Trap_X));
                  begin
                     This.State.PC := This.Get_G (G_Trap_Where);
                     This.Set_R (Register_Index (Trap.Y),
                                 This.Get_G (G_Trap_Y));
                     This.Set_R (Register_Index (Trap.Z),
                                 This.Get_G (G_Trap_Z));
                  end;
            end case;
      end case;

      This.Tick;

   end Execute;

   -----------------
   -- Execute_ALU --
   -----------------

   function Execute_ALU
     (This     : in out Instance'Class;
      Y, Z     : Word_32;
      Op       : Instruction.ALU_Operation;
      Unsigned : Boolean)
      return Word_32
   is

      function Sign (X : Word_32) return Word_32
      is (if X = 0 then 0
          elsif X < 16#1000_0000# then 1
          else 16#FFFF_FFFF#);

      function Shift_Left (X : Word_32; Bits : Natural) return Word_32;

      function Shift_Right (X : Word_32; Bits : Natural) return Word_32;

      ----------------
      -- Shift_Left --
      ----------------

      function Shift_Left (X : Word_32; Bits : Natural) return Word_32 is
      begin
         if Bits < 32 then
            return X * 2 ** Bits;
         else
            return 0;
         end if;
      end Shift_Left;

      -----------------
      -- Shift_Right --
      -----------------

      function Shift_Right (X : Word_32; Bits : Natural) return Word_32 is
      begin
         if Bits < 32 then
            if Unsigned
              or else X < 16#1000_0000#
            then
               return X / 2 ** Bits;
            else
               return X / 2 ** Bits or not (2 ** (32 - Bits) - 1);
            end if;
         else
            if Unsigned then
               return 0;
            else
               return 16#FFFF_FFFF#;
            end if;
         end if;
      end Shift_Right;

      X : constant Word_32 :=
            (case Op is
                when Op_Add => Y + Z,
                when Op_Sub => Y - Z,
                when Op_Mul => Y * Z,
                when Op_Div => Y / Z,
                when Op_Neg => (not Z) + 1,
                when Op_And => Y and Z,
                when Op_Or  => Y or Z,
                when Op_Xor => Y xor Z,
                when Op_Nand => not (Y and Z),
                when Op_Nor  => not (Y or Z),
                when Op_Cmp => Sign (Y - Z),
                when Op_Sl  => Shift_Left (Y, Natural (Z)),
                when Op_Sr  => Shift_Right (Y, Natural (Z)));
   begin
      if Op = Op_Div then
         This.State.G_R := Y mod Z;
      end if;
      return X;
   end Execute_ALU;

   --------------------
   -- Execute_Branch --
   --------------------

   procedure Execute_Branch
     (This      : in out Instance'Class;
      Value     : Word_32;
      Condition : Condition_Type;
      Negated   : Boolean;
      Backwards : Boolean;
      Offset    : Word_32)
   is
      Taken : constant Boolean :=
                Negated xor
                    (case Condition is
                        when Cond_Negative =>
                          Value >= 16#1000_0000#,
                        when Cond_Zero     =>
                          Value = 0,
                        when Cond_Positive =>
                          Value in 1 .. 16#1000_0000#,
                        when Cond_Odd      =>
                          Value mod 2 = 1);
   begin
      if Taken then
         This.State.PC := This.State.PC - 4
           + (4 * Offset
              - (if Backwards then 262144 else 0));
      end if;
   end Execute_Branch;

   ------------------
   -- Execute_Load --
   ------------------

   procedure Execute_Load
     (This        : in out Instance'Class;
      Address     : Address_Type;
      Size        : Op_Size;
      Unsigned    : Boolean;
      Destination : out Word_32)
   is
   begin
      case Size is
         when Size_8 =>
            declare
               W : Word_8;
            begin
               This.Get_Word_8 (Address, W);
               if Unsigned or else W < 128 then
                  Destination := Word_32 (W);
               else
                  Destination := 16#FFFF_FF00# or Word_32 (W);
               end if;
            end;
         when Size_16 =>
            declare
               W : Word_16;
            begin
               This.Get_Word_16 (Address, W);
               if Unsigned or else W < 32768 then
                  Destination := Word_32 (W);
               else
                  Destination := 16#FFFF_0000# or Word_32 (W);
               end if;
            end;
         when Size_32 =>
            declare
               W : Word_32;
            begin
               This.Get_Word_32 (Address, W);
               Destination := W;
            end;
      end case;
   end Execute_Load;

   -------------------
   -- Execute_Store --
   -------------------

   procedure Execute_Store
     (This     : in out Instance'Class;
      Address  : Address_Type;
      Size     : Op_Size;
      Unsigned : Boolean;
      Source   : Word_32)
   is
      pragma Unreferenced (Unsigned);
   begin
      case Size is
         when Size_8 =>
            This.Set_Word_8 (Address, Word_8 (Source mod 256));
         when Size_16 =>
            This.Set_Word_16 (Address, Word_16 (Source mod 65536));
         when Size_32 =>
            This.Set_Word_32 (Address, Source);
      end case;
   end Execute_Store;

   ------------------
   -- Execute_Trap --
   ------------------

   procedure Execute_Trap
     (This    : in out Instance'Class;
      X, Y, Z : Word_8)
   is
   begin
      if Enable_Trace then
         Aqua.Logging.Log
           ("trap "
            & Aqua.Images.Hex_Image (X)
            & " "
            & Aqua.Images.Hex_Image (Y)
            & " "
            & Aqua.Images.Hex_Image (Z));
      end if;

      if X = 0 and then Y = 0 and then Z = 0 then
         declare
            Msg_Addr : constant Address_Type := This.Get_R (255);
         begin
            if Msg_Addr /= 0 then
               declare
                  Length : Word_32;
                  P      : Address_Type := Msg_Addr + 4;
               begin
                  This.Get_Word_32 (Msg_Addr, Length);

                  declare
                     Message : String (1 .. Natural (Length));
                  begin
                     for I in 1 .. Natural (Length) loop
                        declare
                           Ch : Word_32;
                        begin
                           This.Get_Word_32 (P, Ch);
                           P := P + 4;
                           Message (I) := Character'Val (Ch);
                        end;
                     end loop;
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error, Message);
                  end;
               end;
            end if;
         exception
            when Aqua.MM.Protection_Fault =>
               Ada.Text_IO.Put_Line
                 ("halt: msg = " & Aqua.Images.Hex_Image (Msg_Addr));
         end;

         This.State.Halted := True;

      else
         This.State.G_WW := This.State.PC;
         This.State.G_XX :=
           2 ** 31 + Word_32 (X) * 65536
           + Word_32 (Y) * 256
           + Word_32 (Z);
         This.State.G_YY := This.Get_R (Register_Index (Y));
         This.State.G_ZZ := This.Get_R (Register_Index (Z));
         This.State.G_BB := This.Get_R (255);
         This.Set_R (255, This.State.G_J);
         This.State.PC := This.State.G_T;

         --  if Enable_Trace then
         --     Ada.Text_IO.Put_Line
         --       ("BB <- " & Aqua.Images.Hex_Image (This.State.G_BB));
         --     Ada.Text_IO.Put_Line
         --       ("WW <- " & Aqua.Images.Hex_Image (This.State.G_WW));
         --     Ada.Text_IO.Put_Line
         --       ("XX <- " & Aqua.Images.Hex_Image (This.State.G_XX));
         --     Ada.Text_IO.Put_Line
         --       ("YY <- " & Aqua.Images.Hex_Image (This.State.G_YY));
         --     Ada.Text_IO.Put_Line
         --       ("ZZ <- " & Aqua.Images.Hex_Image (This.State.G_ZZ));
         --     Ada.Text_IO.Put_Line
         --       ("PC <- " & Aqua.Images.Hex_Image (This.State.PC));
         --  end if;
      end if;
   end Execute_Trap;

   -----------
   -- Get_R --
   -----------

   function Get_R
     (This : Instance'Class;
      R    : Register_Index)
      return Word
   is
   begin
      if This.Is_Marginal (R) then
         return 0;
      elsif This.Is_Local (R) then
         return This.State.Window (This.To_Window_Index (R));
      else
         return This.State.Global (R);
      end if;
   end Get_R;

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8)
   is
      Phys_Addr : constant Address_Type :=
                    (if This.Mapping
                     then This.Memory.Map (Address, True)
                     else Address);
   begin
      This.Bus.Get_Word_8 (Phys_Addr, Value);
   end Get_Word_8;

   -----------------
   -- Get_Word_16 --
   -----------------

   overriding procedure Get_Word_16
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_16)
   is
      Phys_Addr : constant Address_Type :=
                    (if This.Mapping
                     then This.Memory.Map (Address)
                     else Address);
   begin
      This.Bus.Get_Word_16 (Phys_Addr, Value);
   end Get_Word_16;

   -----------------
   -- Get_Word_32 --
   -----------------

   overriding procedure Get_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_32)
   is
      Phys_Addr : constant Address_Type :=
                    (if This.Mapping
                     then This.Memory.Map (Address, True)
                     else Address);
   begin
      This.Bus.Get_Word_32 (Phys_Addr, Value);
   end Get_Word_32;

   --------------------------
   -- Global_Register_Name --
   --------------------------

   function Global_Register_Name (R : Register_Index) return String is
      Img : String := R'Image;
   begin
      Img (Img'First) := 'r';
      return Img;
   end Global_Register_Name;

   -------------------------
   -- Local_Register_Name --
   -------------------------

   function Local_Register_Name (R : Register_Index) return String is
      Img : String := R'Image;
   begin
      Img (Img'First) := '%';
      return Img;
   end Local_Register_Name;

   ---------
   -- Pop --
   ---------

   procedure Pop
     (This : in out Instance'Class;
      R    : Register_Index)
   is
      G_L : Register_Index renames This.State.G_L;
      G_G : Register_Index renames This.State.G_G;
      G_O : Word renames This.State.G_O;
      G_S : Word renames This.State.G_S;
      X   : constant Register_Index :=
              (if R <= G_L then R else G_L + 1);
      Y   : Word := 0;

      function RO return Window_Register_Index
      is (Window_Register_Index (G_O / 4 mod Register_Window_Size));

      function RS return Window_Register_Index
      is (Window_Register_Index (G_S / 4 mod Register_Window_Size));

   begin

      if RO = RS then
         This.Stack_Load;
      end if;

      G_O := G_O - 4;

      if X > 0 and then X <= G_L then
         Y := This.Get_R (X);
      end if;

      declare
         Hole : constant Word := This.Get_R (0);
         L    : Word := Word (G_L);
      begin
         while G_O - G_S < Hole * 4 loop
            --  Aqua.Logging.Log ("stack load: o="
            --                    & Aqua.Images.Hex_Image (G_O)
            --                    & ";s="
            --                    &  Aqua.Images.Hex_Image (G_S));
            This.Stack_Load;
         end loop;

         L := Word'Min (L + 1, Word (X)) + Hole;
         L := Word'Min (L, Word (G_G));

         if Hole < L then
            This.State.Window (RO) := Y;
         end if;

         This.Set_G (G_Local, L);
         This.Set_G (G_Offset, G_O - Hole * 4);

         --  G_L := Register_Index (L);
         --  G_O := G_O - Hole * 4;
      end;

   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Instance'Class;
                   R    : Register_Index)
   is
      G_L : Register_Index renames This.State.G_L;
      G_O : Word renames This.State.G_O;
      X   : Register_Index := R;
   begin
      if This.Is_Global (X) then
         X := G_L;
         G_L := G_L + 1;
         This.Stack_Room;
      end if;

      This.Set_R (X, Word (X));

      This.Set_G (G_Local, Word (G_L - X - 1));
      This.Set_G (G_Offset, G_O + 4 * (Word (X) + 1));

      --  G_L := G_L - X - 1;
      --  G_O := G_O + 4 * (Word (X) + 1);

   end Push;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Instance'Class) is
   begin
      This.State := (others => <>);
   end Reset;

   -----------
   -- Set_G --
   -----------

   procedure Set_G
     (This : in out Instance'Class;
      G    : Register_Index;
      V    : Word)
   is
   begin
      pragma Assert (G /= G_Global or else V mod 256 >= 32,
                     "G must be >= 32");
      pragma Assert (G /= G_Local or else V mod 256 < Word (This.State.G_G),
                     "L" & This.State.G_L'Image
                     & " must be < G"
                     & This.State.G_G'Image);

      case G is
         when System_Global =>
            case System_Global (G) is
               when G_Bootstrap   =>
                  This.State.G_B := V;
               when G_Global =>
                  This.State.G_G := Register_Index (V mod 256);
               when G_Jump =>
                  This.State.G_J := V;
               when G_Local  =>
                  This.State.G_L := Register_Index (V mod 256);
               when G_Offset =>
                  This.State.G_O := V;
               when G_Stack =>
                  This.State.G_S := V;
               when G_Trap_Address   =>
                  This.State.G_T := V;
               when G_Trip_Where     =>
                  This.State.G_W := V;
               when G_Trip_X         =>
                  This.State.G_X := V;
               when G_Trip_Y         =>
                  This.State.G_Y := V;
               when G_Trip_Z         =>
                  This.State.G_Z := V;
               when G_Trap_Bootstrap =>
                  This.State.G_BB := V;
               when G_Trap_Where     =>
                  This.State.G_WW := V;
               when G_Trap_X         =>
                  This.State.G_XX := V;
               when G_Trap_Y         =>
                  This.State.G_YY := V;
               when G_Trap_Z         =>
                  This.State.G_ZZ := V;
               when others =>
                  raise Constraint_Error with
                    "cannot set special register" & G'Image;
            end case;
         when General_Global =>
            This.State.Global (G) := V;
      end case;
   end Set_G;

   -----------
   -- Set_R --
   -----------

   procedure Set_R
     (This : in out Instance'Class;
      R    : Register_Index;
      V    : Word)
   is
   begin
      if R < This.State.G_G then
         while This.Is_Marginal (R) loop
            This.Set_G (G_Local, This.Get_G (G_Local) + 1);
            This.Stack_Room;
         end loop;
      end if;

      This.Trace.Update (Word_8 (R), V);

      if This.Is_Local (R) then
         declare
            W : constant Window_Register_Index :=
                  Window_Register_Index
                    (This.State.G_O / 4 mod Register_Window_Size)
                  + Window_Register_Index (R);
         begin
            This.State.Window (W) := V;
         end;
      else
         This.State.Global (R) := V;
      end if;

   end Set_R;

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8)
   is
      Phys_Addr : constant Address_Type :=
                    (if This.Mapping
                     then This.Memory.Map (Address, Write => True)
                     else Address);
   begin
      This.Bus.Set_Word_8 (Phys_Addr, Value);
   end Set_Word_8;

   -----------------
   -- Set_Word_16 --
   -----------------

   overriding procedure Set_Word_16
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_16)
   is
      Phys_Addr : constant Address_Type :=
                    (if This.Mapping
                     then This.Memory.Map (Address, Write => True)
                     else Address);
   begin
      This.Bus.Set_Word_16 (Phys_Addr, Value);
   end Set_Word_16;

   -----------------
   -- Set_Word_32 --
   -----------------

   overriding procedure Set_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_32)
   is
      Phys_Addr : constant Address_Type :=
                    (if This.Mapping
                     then This.Memory.Map (Address, Write => True)
                     else Address);
   begin
      This.Bus.Set_Word_32 (Phys_Addr, Value);
   end Set_Word_32;

   -----------
   -- Split --
   -----------

   function Split (IR : Word_32) return Instruction_Record is
      It : Word_32 := IR;

      function Next return Word_8;

      ----------
      -- Next --
      ----------

      function Next return Word_8 is
      begin
         return W : constant Word_8 := Word_8 (It mod 256) do
            It := It / 256;
         end return;
      end Next;

   begin
      return Rec : Instruction_Record do
         Rec.Z := Next;
         Rec.Y := Next;
         Rec.X := Next;
         Rec.Op := Next;
      end return;
   end Split;

   ----------------
   -- Stack_Load --
   ----------------

   procedure Stack_Load (This : in out Instance'Class) is
      G_S : Word renames This.State.G_S;
   begin
      G_S := G_S - 4;

      This.Get_Word_32
        (Address => Address_Type (G_S),
         Value   =>
           This.State.Window
             (Window_Register_Index (G_S / 4 mod Register_Window_Size)));

   end Stack_Load;

   ----------------
   -- Stack_Room --
   ----------------

   procedure Stack_Room (This : in out Instance'Class) is
      G_L : Register_Index renames This.State.G_L;
      G_O : Word renames This.State.G_O;
      G_S : Word renames This.State.G_S;
   begin
      if (G_S - G_O) / 4 mod Register_Window_Size = Word (G_L) then
         This.Stack_Store;
      end if;
   end Stack_Room;

   -----------------
   -- Stack_Store --
   -----------------

   procedure Stack_Store (This : in out Instance'Class) is
      G_S : Word renames This.State.G_S;
   begin
      if Enable_Trace then
         Ada.Text_IO.Put_Line
           ("out of registers; writing stack to "
            & Aqua.Images.Hex_Image (G_S));
      end if;

      This.Set_Word_32
        (Address => Address_Type (G_S),
         Value   =>
           This.State.Window
             (Window_Register_Index (G_S / 4 mod Register_Window_Size)));
      G_S := G_S + 4;
   end Stack_Store;

   -----------
   -- Start --
   -----------

   procedure Start
     (This             : in out Instance'Class;
      Initial_Location : Address_Type;
      Arguments        : Array_Of_Words := [])
   is
   begin
      if Enable_Trace then
         Aqua.Logging.Start;
      end if;

      This.State.PC := Initial_Location;
      This.State.Halted := False;
      This.State.G_L := 0;
      This.State.G_O := 0;
      This.State.G_S := 0;


      declare
         R : Register_Index := 0;
      begin
         for Arg of Arguments loop
            This.Set_R (R, Arg);
            R := R + 1;
         end loop;
      end;

      while not This.State.Halted loop
         declare
            IR         : Word;
            PC         : Address_Type;
            Virtual_PC : constant Address_Type := This.State.PC;
         begin
            PC := (if This.Mapping
                   then This.Memory.Map (This.State.PC, True, False, True)
                   else This.State.PC);
            This.Bus.Get_Word_32 (PC, IR);

            if Enable_Trace then
               declare
                  Current_Module : constant String :=
                                     (if This.Debugger = null
                                      then ""
                                      else This.Debugger.Get_Module_Name
                                        (This.State.PC));
                  Module_Addr    : constant Address_Type :=
                                     (if This.Debugger = null
                                      then This.State.PC
                                      else This.Debugger
                                      .To_Module_Local_Address
                                        (This.State.PC));
               begin
                  This.Trace.Initialize
                    (Current_Module, This.State.PC, Module_Addr, IR);
               end;
            end if;

            This.State.PC := This.State.PC + 4;
            This.Execute (IR);

            if Enable_Trace then
               This.Trace.Save;
            end if;

         exception
            when Bad_Instruction =>
               raise Bad_Instruction with
                 "addr=" & Aqua.Images.Hex_Image (Virtual_PC)
                 & " ir=" & Aqua.Images.Hex_Image (IR);
            when E : Aqua.MM.Protection_Fault =>
               --  for Index in 0 .. This.State.G_L - 1 loop
               --     declare
               --        Img : String := Index'Image;
               --     begin
               --        Img (Img'First) := '%';
               --        Ada.Text_IO.Put
               --          (Ada.Text_IO.Standard_Error, Img);
               --        Ada.Text_IO.Set_Col
               --          (Ada.Text_IO.Standard_Error, 8);
               --        Ada.Text_IO.Put_Line
               --          (Ada.Text_IO.Standard_Error,
               --           Aqua.Images.Hex_Image
               --             (This.Get_R (Register_Index (Index))));
               --     end;
               --  end loop;

               raise Aqua.MM.Protection_Fault with
                 "pc " & Aqua.Images.Hex_Image (Virtual_PC)
                 & "; ir " & Aqua.Images.Hex_Image (IR)
                 & ": " & Ada.Exceptions.Exception_Message (E);

            when E : Aqua.Ld.Undefined_Reference =>
               raise Aqua.Ld.Undefined_Reference with
                 "pc " & Aqua.Images.Hex_Image (Virtual_PC)
                 & "; ir " & Aqua.Images.Hex_Image (IR)
                 & ": " & Ada.Exceptions.Exception_Message (E);

            when E : Aqua.Bus.Bus_Error =>
               raise Aqua.Bus.Bus_Error with
                 "pc " & Aqua.Images.Hex_Image (Virtual_PC)
                 & "; ir " & Aqua.Images.Hex_Image (IR)
                 & ": " & Ada.Exceptions.Exception_Message (E);
         end;
      end loop;
      if Enable_Trace then
         Aqua.Logging.Stop;
      end if;

   exception
      when others =>
         if Enable_Trace then
            Aqua.Logging.Stop;
         end if;
         raise;
   end Start;

   -----------
   -- Trace --
   -----------

   procedure Trace (Enabled : Boolean) is
   begin
      Enable_Trace := Enabled;
   end Trace;

end Aqua.CPU;
