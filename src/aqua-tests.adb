with Ada.Sequential_IO;
with Ada.Text_IO;

with Aqua.CPU.Test;
with Aqua.Images;
with Aqua.Logging;
with Aqua.Options;

with Aqua.Instruction;

package body Aqua.Tests is

   Log_Enabled  : constant Boolean := False;

   Loader_Value : constant Word_8 := 16#98#;
   L_Header     : constant := 1;
   L_Footer     : constant := 2;
   L_Quote      : constant := 3;
   L_Location   : constant := 4;

   type Word_8_Array is array (Address_Type range <>) of Word_8;
   type Word_8_Array_Access is access Word_8_Array;

   Char_IO_Address : constant Address_Type := 16#FFD2#;

   type Test_Memory is new Aqua.Addressable.Instance with
      record
         M : Word_8_Array_Access;
      end record;

   overriding procedure Get_Word_8
     (This    : in out Test_Memory;
      Address : Address_Type;
      Value   : out Word_8);

   overriding procedure Set_Word_8
     (This    : in out Test_Memory;
      Address : Address_Type;
      Value   : Word_8);

   ------------------------
   -- Create_Test_Memory --
   ------------------------

   function Create_Test_Memory (Size : Word) return Aqua.Addressable.Reference
   is
   begin
      return new Test_Memory'
        (Aqua.Addressable.Instance with M => new Word_8_Array (0 .. Size - 1));
   end Create_Test_Memory;

   ----------------
   -- Get_Word_8 --
   ----------------

   overriding procedure Get_Word_8
     (This    : in out Test_Memory;
      Address : Address_Type;
      Value   : out Word_8)
   is
   begin
      Value := This.M (Address);
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "bad address: " & Aqua.Images.Hex_Image (Address);
   end Get_Word_8;

   ---------
   -- Run --
   ---------

   procedure Run
     (CPU  : in out Aqua.CPU.Instance'Class;
      Argc : Word_32;
      Argv : Word_32;
      Path : String)
   is
      package Word_8_IO is new Ada.Sequential_IO (Word_8);
      use Word_8_IO;
      File : File_Type;
      Address : Address_Type := 0;
      Start   : Address_Type := 0;

      procedure Put (W : Word_8);

      ---------
      -- Put --
      ---------

      procedure Put (W : Word_8) is
      begin
         CPU.Set_Word_8 (Address, W);
         Address := Address + 1;
      end Put;

   begin
      if Aqua.Options.Trace then
         Aqua.Logging.Start;
      end if;
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            W : Word_8;
         begin
            Read (File, W);
            if W = Loader_Value then
               Read (File, W);
               case W is
                  when L_Header =>
                     Read (File, W);
                     if W > 1 then
                        raise Constraint_Error with
                          "cannot read version" & W'Image & " object files";
                     end if;
                     Read (File, W);
                     for I in 1 .. W * 4 loop
                        Read (File, W);
                     end loop;
                  when L_Quote =>
                     Read (File, W);
                     Read (File, W);
                     for I in 1 .. W * 4 loop
                        Read (File, W);
                        Put (W);
                     end loop;
                  when L_Location =>
                     Read (File, W);
                     Read (File, W);
                     Address := 0;
                     for I in 1 .. 4 loop
                        Read (File, W);
                        Address := Address * 256 + Address_Type (W);
                     end loop;
                     if Start = 0 then
                        Start := Address;
                     end if;
                  when L_Footer =>
                     Read (File, W);
                     Read (File, W);
                     Aqua.CPU.Test.Put_R (CPU, 19, Word_32 (W));
                     for I in W .. 255 loop
                        declare
                           G : Word_32 := 0;
                        begin
                           for I in 1 .. 4 loop
                              Read (File, W);
                              G := G * 256 + Word_32 (W);
                           end loop;
                           Aqua.CPU.Test.Set_Register (CPU, I, G);
                        end;
                     end loop;
                  when others =>
                     raise Constraint_Error with
                       "invalid loader command:" & W'Image;
               end case;
            else
               Put (W);
               for I in 1 .. 3 loop
                  Read (File, W);
                  Put (W);
               end loop;
            end if;
         end;
      end loop;
      Close (File);
      Aqua.CPU.Test.Set_Register (CPU, 0, Argc);
      Aqua.CPU.Test.Set_Register (CPU, 1, Argv);
      CPU.Start (Word_32 (Start));
      if Aqua.Options.Trace then
         Aqua.Logging.Stop;
      end if;
   end Run;

   ----------------
   -- Set_Word_8 --
   ----------------

   overriding procedure Set_Word_8
     (This    : in out Test_Memory;
      Address : Address_Type;
      Value   : Word_8)
   is
   begin
      if Address = Char_IO_Address then
         Ada.Text_IO.Put (Character'Val (Value));
      else
         This.M (Address) := Value;
      end if;
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "Bad address: " & Aqua.Images.Hex_Image (Address);
   end Set_Word_8;

   ----------
   -- Test --
   ----------

   procedure Test (CPU : in out Aqua.CPU.Instance'Class) is
   begin

      for Op in Word_8 loop
         declare
            Control : constant Aqua.Instruction.Control_Record :=
                        Aqua.Instruction.Get_Control (Op);
         begin
            if not Control.Bad_Instruction then
               Ada.Text_IO.Put_Line
                 (Aqua.Images.Hex_Image (Op)
                  & " "
                  & Aqua.Instruction.Show (Control));
            end if;
         end;
      end loop;

      for I in Word range 0 .. 16#EFFF# loop
         CPU.Set_Word_8 (I, Word_8 (I mod 256));
      end loop;

      for I in Word range 0 .. 16#EFFF# loop
         declare
            B : Word_8;
         begin
            CPU.Get_Word_8 (I, B);
            pragma Assert (B = Word_8 (I mod 256));
         end;
      end loop;

      for I in Word range 0 .. 16#2FFF# loop
         declare
            W : Word_32;
            A : constant Word := I * 4;
            V : constant Word := A mod 256;
            X : constant Word_32 :=
                  V + 3 + 256 * (V + 2 + 256 * (V + 1 + 256 * (V + 0)));
         begin
            for J in Word range 0 .. 3 loop
               CPU.Get_Word_32 (A + J, W);
               pragma Assert (W = X);
            end loop;
         end;
      end loop;

      declare
         Last_Local       : constant := 126;
         Subroutine_Depth : constant := 30;
      begin

         for I in Word_8 range 0 .. Last_Local loop
            Aqua.CPU.Test.Set_Register
              (CPU, I, Word (I));
         end loop;

         Aqua.CPU.Test.Log_State (CPU);

         for Push_Index in 1 .. Subroutine_Depth loop

            Aqua.CPU.Test.Push_Registers (CPU, Last_Local + 1);
            Aqua.CPU.Test.Log_State (CPU);

            for I in Word_8 range 0 .. Last_Local loop
               declare
                  R : constant Word := Aqua.CPU.Test.Get_Register (CPU, I);
               begin
                  pragma Assert
                    (R = 0,
                     "expected 0 but found" & R'Image & " at" & I'Image);
               end;
            end loop;

            for I in Word_8 range 0 .. Last_Local loop
               Aqua.CPU.Test.Set_Register
                 (CPU, I, Word (Push_Index) * 1000 + Word (I));
            end loop;

            for I in Word_8 range 0 .. Last_Local loop
               declare
                  Expect : constant Word :=
                             Word (Push_Index) * 1000 + Word (I);
                  Found  : constant Word :=
                             Aqua.CPU.Test.Get_Register (CPU, I);
               begin
                  pragma Assert (Expect = Found,
                                 "expected" & Expect'Image
                                 & " but found" & Found'Image
                                 & " at push" & Push_Index'Image
                                 & " and register" & I'Image);
               end;

            end loop;

         end loop;

         for Pop_Index in reverse 1 .. Subroutine_Depth loop

            Aqua.CPU.Test.Pop_Registers (CPU, 0);
            Aqua.CPU.Test.Log_State (CPU);

            for I in Word_8 range 0 .. Last_Local loop
               declare
                  Value : constant Word :=
                            Aqua.CPU.Test.Get_Register (CPU, I);
                  Expect : constant Word :=
                             Word (Pop_Index - 1) * 1000 + Word (I);
               begin
                  pragma Assert
                    (Value = Expect,
                     "expected" & Expect'Image
                     & " but found" & Value'Image
                     & " at pop" & Pop_Index'Image
                     & " and register" & I'Image);
               end;
            end loop;

         end loop;
      end;

      for Y in Word_32 range 0 .. 20 loop
         for Z in Word_32 range 0 .. 20 loop

            declare
               procedure Check (Expected : Word_32);

               -----------
               -- Check --
               -----------

               procedure Check (Expected : Word_32) is
                  Found : constant Word_32 :=
                            Aqua.CPU.Test.Get_Register (CPU, 0);
               begin
                  pragma Assert (Expected = Found,
                                 "expected" & Expected'Image
                                 & " but found" & Found'Image);
               end Check;

            begin
               Aqua.CPU.Test.Set_Register (CPU, 0, 0);
               Aqua.CPU.Test.Set_Register (CPU, 1, Y);
               Aqua.CPU.Test.Set_Register (CPU, 2, Z);
               Aqua.CPU.Test.Execute (CPU, 16#20_00_01_02#);
               Check (Y + Z);
               Aqua.CPU.Test.Execute (CPU, 16#21_00_01_02#);
               Check (Y + 2);
               Aqua.CPU.Test.Execute (CPU, 16#22_00_01_02#);
               Check (Y + Z);
               Aqua.CPU.Test.Execute (CPU, 16#23_00_01_02#);
               Check (Y + 2);
               Aqua.CPU.Test.Execute (CPU, 16#24_00_01_02#);
               Check (Y - Z);
               Aqua.CPU.Test.Execute (CPU, 16#25_00_01_02#);
               Check (Y - 2);
               Aqua.CPU.Test.Execute (CPU, 16#26_00_01_02#);
               Check (Y - Z);
               Aqua.CPU.Test.Execute (CPU, 16#27_00_01_02#);
               Check (Y - 2);

               Aqua.CPU.Test.Set_Register (CPU, 3, Y + Z);
               Aqua.CPU.Test.Execute (CPU, 16#A8_03_01_02#);
               Aqua.CPU.Test.Execute (CPU, 16#88_00_01_02#);
               declare
                  W : Word;
               begin
                  CPU.Get_Word_32 (Y + Z, W);
                  Check (W);
               end;

               Aqua.CPU.Test.Execute (CPU, 16#88_00_01_02#);
               declare
                  W : Word;
               begin
                  CPU.Get_Word_32 ((Y + Z) and 16#FFFF_FFFC#, W);
                  Check (W);
               end;

            end;
         end loop;
      end loop;

      declare
         subtype Hex_Code is String (1 .. 8);
         Program : constant array (Positive range <>) of Hex_Code :=
                     ["E1000020",
                      "E101007F",
                      "E102FFD2",
                      "A1000200",
                      "21000001",
                      "30030100",
                      "4B03FFFC",
                      "E100000A",
                      "A1000200",
                      "00000000"];
         Address : Address_Type := 16#C000#;

         function To_Word_8 (Ch : Character) return Word_8 is
           (if Ch in '0' .. '9'
            then Character'Pos (Ch) - 48
            else Character'Pos (Ch) - 55);

      begin
         for Code of Program loop
            for Byte in 1 .. 4 loop
               declare
                  X : constant Word_8 :=
                        16 * To_Word_8 (Code (Byte * 2 - 1))
                        + To_Word_8 (Code (Byte * 2));
               begin
                  CPU.Set_Word_8 (Address, X);
                  Address := Address + 1;
               end;
            end loop;
         end loop;

         Address := 16#C000#;
         for I in 1 .. 10 loop
            declare
               X : Word_32;
            begin
               CPU.Get_Word_32 (Address, X);
               Ada.Text_IO.Put_Line
                 (Aqua.Images.Hex_Image (Word (Address))
                  & ": "
                  & Aqua.Images.Hex_Image (X)
                  & "   "
                  & Aqua.Instruction.Show
                    (Aqua.Instruction.Get_Control (Word_8 (X / 2 ** 24))));
            end;
            Address := Address + 4;
         end loop;

         if Log_Enabled then
            Aqua.Logging.Start;
         end if;

         CPU.Start (16#C000#);

         if Log_Enabled then
            Aqua.Logging.Stop;
         end if;

      end;

   end Test;

end Aqua.Tests;
