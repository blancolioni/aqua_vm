with Ada.Directories;
with Ada.Strings.Unbounded;

with Aqua.Version;

package body Aqua.IO is

   Local_IO_Path : Ada.Strings.Unbounded.Unbounded_String;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      Word_8_IO.Close (File.F);
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Type;
      Name : String)
   is
      use Ada.Strings.Unbounded;
   begin
      if Local_IO_Path = Null_Unbounded_String then
         raise Constraint_Error
           with "Aqua: scratch path not set";
      end if;
      Word_8_IO.Create (File.F, Word_8_IO.Out_File,
                      Ada.Directories.Compose
                         (To_String (Local_IO_Path), Name));

      for Ch of Aqua.Version.Name loop
         Write_Word_8 (File, Character'Pos (Ch));
      end loop;

      File.Major := Aqua.Version.Major;
      File.Minor := Aqua.Version.Minor;
      File.Release := Aqua.Version.Release;

      Write_Word_8 (File, File.Major);
      Write_Word_8 (File, File.Minor);
      Write_Word_8 (File, File.Release);

   end Create;

   ---------------------
   -- Current_IO_Path --
   ---------------------

   function Current_IO_Path return String is
   begin
      return Ada.Strings.Unbounded.To_String (Local_IO_Path);
   end Current_IO_Path;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Name : String)
   is
      use Ada.Strings.Unbounded;
   begin
      if Local_IO_Path = Null_Unbounded_String then
         raise Constraint_Error
           with "Aqua: scratch path not set";
      end if;
      Word_8_IO.Open (File.F, Word_8_IO.In_File,
                    Ada.Directories.Compose
                       (To_String (Local_IO_Path), Name));

      for Ch of Aqua.Version.Name loop
         declare
            X : Word_8;
         begin
            Read_Word_8 (File, X);
            if X /= Character'Pos (Ch) then
               Word_8_IO.Close (File.F);
               raise Invalid_Header;
            end if;
         end;
      end loop;

      Read_Word_8 (File, File.Major);
      Read_Word_8 (File, File.Minor);
      Read_Word_8 (File, File.Release);

      if File.Major > Aqua.Version.Major
        or else (File.Major = Aqua.Version.Major
                 and then File.Minor > Aqua.Version.Minor)
      then
         Word_8_IO.Close (File.F);
         raise Invalid_Version
           with "aqua version "
           & Character'Val (Aqua.Version.Major + 48)
           & "." & Character'Val (Aqua.Version.Minor + 48)
           & "." & Character'Val (Aqua.Version.Release + 48)
           & " cannot read file version "
           & Character'Val (File.Major + 48)
           & "." & Character'Val (File.Minor + 48)
           & "." & Character'Val (File.Release + 48);
      end if;

   end Open;

   ------------------
   -- Read_Address --
   ------------------

   procedure Read_Address
     (File  : File_Type;
      Value : out Address_Type)
   is
      W : Word;
   begin
      Read_Word (File, W);
      Value := W;
   end Read_Address;

   -------------------------
   -- Read_String_Literal --
   -------------------------

   function Read_String_Literal
     (File : File_Type)
      return String
   is
      Length : Word;
   begin
      Read_Word (File, Length);
      return Result : String (1 .. Natural (Length)) do
         for I in Result'Range loop
            declare
               X : Word_8;
            begin
               Read_Word_8 (File, X);
               Result (I) := Character'Val (X);
            end;
         end loop;
      end return;
   end Read_String_Literal;

   ---------------
   -- Read_Word --
   ---------------

   procedure Read_Word
     (File  : File_Type;
      Value : out Word)
   is
      X : array (1 .. 4) of Word_8;
   begin
      for I in X'Range loop
         Read_Word_8 (File, X (I));
      end loop;
      Value := 0;
      for I in reverse X'Range loop
         Value := Value * 256 + Word (X (I));
      end loop;
   end Read_Word;

   -----------------
   -- Read_Word_8 --
   -----------------

   procedure Read_Word_8
     (File  : File_Type;
      Value : out Word_8)
   is
   begin
      Word_8_IO.Read (File.F, Value);
   end Read_Word_8;

   -----------------
   -- Set_IO_Path --
   -----------------

   procedure Set_IO_Path
     (Path : String)
   is
   begin
      Local_IO_Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
   end Set_IO_Path;

   -------------------
   -- Write_Address --
   -------------------

   procedure Write_Address
     (File  : File_Type;
      Value : Address_Type)
   is
   begin
      Write_Word (File, Value);
   end Write_Address;

   --------------------------
   -- Write_String_Literal --
   --------------------------

   procedure Write_String_Literal
     (File  : File_Type;
      Value : String)
   is
   begin
      Write_Word (File, Word (Value'Length));
      for Ch of Value loop
         Write_Word_8 (File, Character'Pos (Ch));
      end loop;
   end Write_String_Literal;

   ----------------
   -- Write_Word --
   ----------------

   procedure Write_Word
     (File  : File_Type;
      Value : Word)
   is
      It : Word := Value;
   begin
      for I in 1 .. 4 loop
         Write_Word_8 (File, Word_8 (It mod 256));
         It := It / 256;
      end loop;
   end Write_Word;

   ------------------
   -- Write_Word_8 --
   ------------------

   procedure Write_Word_8
     (File  : File_Type;
      Value : Word_8)
   is
   begin
      Word_8_IO.Write (File.F, Value);
   end Write_Word_8;

end Aqua.IO;
