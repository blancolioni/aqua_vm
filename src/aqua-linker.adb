with Ada.Directories;
with Ada.Strings.Fixed;

with Aqua.Logging;

with System.Storage_Elements;

with WL.Files.ELF;

package body Aqua.Linker is

   ----------
   -- Load --
   ----------

   procedure Load
     (OS   : not null access Aqua.OS.Instance'Class;
      Path : String)
   is
      Start : constant Address_Type :=
                Load (OS, Path);
   begin
      pragma Unreferenced (Start);
   end Load;

   ----------
   -- Load --
   ----------

   function Load
     (OS   : not null access Aqua.OS.Instance'Class;
      Path : String)
      return Address_Type
   is
      use WL.Files.ELF;
      File : File_Type;

      Module_Name : constant String :=
                      Ada.Directories.Base_Name (Path);

      Indices : array (Aqua.OS.Memory_Segment) of Elf_Word_32 :=
                  [others => 0];
      Loaders : array (Aqua.OS.Memory_Segment) of Aqua.OS.Loader_Type;

      function Find_Loader (Section : Elf_Word_16) return Aqua.OS.Loader_Type;

      procedure Load_Program_Bits
        (Section : Section_Entry);

      procedure Load_Relocation
        (Section : Section_Entry);

      procedure Load_Symbol
        (Name         : String;
         Value        : Address_32;
         Size         : Elf_Word_32;
         Binding      : Symbol_Table_Binding;
         Typ          : Symbol_Table_Type;
         Visibility   : Symbol_Table_Visibility;
         Section      : Elf_Word_16);

      procedure Load_Relocation
        (Section      : Elf_Word_16;
         Offset       : Address_32;
         Info         : Octet;
         Symbol       : Symbol_Table_Entry);

      -----------------
      -- Find_Loader --
      -----------------

      function Find_Loader
        (Section : Elf_Word_16)
         return Aqua.OS.Loader_Type
      is
      begin
         for Segment in Indices'Range loop
            if Indices (Segment) = Elf_Word_32 (Section) then
               return Loaders (Segment);
            end if;
         end loop;
         raise Constraint_Error with
           "no loader for section" & Section'Image;
      end Find_Loader;

      -----------------------
      -- Load_Program_Bits --
      -----------------------

      procedure Load_Program_Bits
        (Section : Section_Entry)
      is
         use System.Storage_Elements;
         Data : Storage_Array (1 .. Storage_Count (Get_Size (Section)));
         Segment : constant Aqua.OS.Memory_Segment :=
                     (if Get_Write (Section)
                      then (if Get_Alloc (Section)
                        then Aqua.OS.Data
                        else Aqua.OS.Heap)
                      elsif Get_Execinstr (Section)
                      then Aqua.OS.Code
                      else Aqua.OS.Text);
      begin
         Read (File, Get_Offset (Section), Data);

         declare
            Loader : constant Aqua.OS.Loader_Type :=
                       OS.Load
                         (Data    => Data,
                          Segment => Segment);
         begin
            Indices (Segment) := Get_Index (Section);
            Loaders (Segment) := Loader;
         end;
      end Load_Program_Bits;

      ---------------------
      -- Load_Relocation --
      ---------------------

      procedure Load_Relocation
        (Section      : Elf_Word_16;
         Offset       : Address_32;
         Info         : Octet;
         Symbol       : Symbol_Table_Entry)
      is
         Loader     : constant Aqua.OS.Loader_Type := Find_Loader (Section);
         Sym_Loader : constant Aqua.OS.Loader_Type :=
                        Find_Loader (Elf_Word_16 (Get_Section (Symbol)));
      begin
         Aqua.OS.Relocate
           (Loader     => Loader,
            Sym_Loader => Sym_Loader,
            Name       => Get_Name (File, Symbol),
            Offset     => Word_32 (Offset),
            Value      =>
              (if Is_Defined (Symbol)
               then Word_32 (Get_Value (Symbol))
               else 0),
            Context    => Relocation_Context'Val (Info),
            Defined    => Is_Defined (Symbol));
      end Load_Relocation;

      ---------------------
      -- Load_Relocation --
      ---------------------

      procedure Load_Relocation
        (Section : Section_Entry)
      is
         Reloc_Index : constant Elf_Word_32 := Get_Info (Section);
         Sym_Index   : constant Elf_Word_32 := Get_Link (Section);
         Sym_Seg     : Aqua.OS.Memory_Segment;
         pragma Unreferenced (Sym_Seg);
         Have_Reloc  : Boolean := False;
         Have_Sym    : Boolean := False;

      begin
         for Segment in Aqua.OS.Memory_Segment loop
            if Indices (Segment) = Reloc_Index then
               Have_Reloc := True;
            elsif Indices (Segment) = Sym_Index then
               Sym_Seg := Segment;
               Have_Sym := True;
            end if;
         end loop;

         pragma Assert (Have_Reloc,
                        "no such segment for relocation:"
                        & Reloc_Index'Image);
         pragma Assert (Have_Sym,
                        "no such symbol table segment:"
                        & Sym_Index'Image);

         --  Iterate_Relocation_Entries (File, Section, Relocate'Access);

      end Load_Relocation;

      -----------------
      -- Load_Symbol --
      -----------------

      procedure Load_Symbol
        (Name         : String;
         Value        : Address_32;
         Size         : Elf_Word_32;
         Binding      : Symbol_Table_Binding;
         Typ          : Symbol_Table_Type;
         Visibility   : Symbol_Table_Visibility;
         Section      : Elf_Word_16)
      is
         pragma Unreferenced (Size, Binding, Typ, Visibility);
         Last_Dot : constant Natural :=
                      Ada.Strings.Fixed.Index
                        (Source  => Name,
                         Pattern => ".",
                         From    => Name'Last,
                         Going   => Ada.Strings.Backward);
         Local_Module : constant String :=
                          (if Last_Dot = 0
                           then Module_Name
                           else Name (Name'First .. Last_Dot - 1));
         Local_Name   : constant String :=
                          (if Last_Dot = 0
                           then Name
                           else Name (Last_Dot + 1 .. Name'Last));
      begin

         if Section = 0 then
            OS.External_Reference (Local_Module, Local_Name);
         else
            for Segment in Aqua.OS.Memory_Segment loop
               if Indices (Segment) = Elf_Word_32 (Section) then
                  Aqua.OS.Define_Symbol (Loaders (Segment),
                                         Local_Module, Local_Name,
                                         Word_32 (Value));
                  exit;
               end if;
            end loop;
         end if;
      end Load_Symbol;

   begin

      Aqua.Logging.Log ("loading: " & Path);

      Open (File, In_File, Path);
      Iterate_Sections (File, Sht_Progbits, Load_Program_Bits'Access);
      Iterate_Symbols (File, Load_Symbol'Access);
      Iterate_Relocation (File, Load_Relocation'Access);

      if False then
         Iterate_Sections (File, Sht_Progbits, Load_Relocation'Access);
      end if;

      OS.Resolve_Pending_References;

      return Addr : constant Address_Type :=
        Address_Type (Get_Start (File))
          + Aqua.OS.Base (Loaders (Aqua.OS.Code))
      do
         Close (File);
      end return;

   end Load;

   -------------
   -- Resolve --
   -------------

   procedure Resolve
     (OS     : not null access Aqua.OS.Instance'Class;
      Offset : Address_Type;
      Target : Address_Type)
   is
   begin
      null;
   end Resolve;

end Aqua.Linker;
