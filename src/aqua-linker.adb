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
   begin
      return Load (OS, Path, null);
   end Load;

   ----------
   -- Load --
   ----------

   function Load
     (OS      : not null access Aqua.OS.Instance'Class;
      Path    : String;
      On_Note : access procedure
        (Name : String;
         Tag  : Word_32;
         Description : String))
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

      procedure Load_Notes
        (Section : Section_Entry);

      procedure Load_Program_Bits
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

      procedure Install_Exception_Handler
        (Description : String);

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

      -------------------------------
      -- Install_Exception_Handler --
      -------------------------------

      procedure Install_Exception_Handler
        (Description : String)
      is
         Separator_1 : constant Natural :=
                         Ada.Strings.Fixed.Index (Description, ",");
         Separator_2 : constant Natural :=
                         (if Separator_1 > 0
                          then Ada.Strings.Fixed.Index
                            (Description, ",", Separator_1 + 1)
                          else 0);
      begin
         if Separator_1 = 0 or else Separator_2 = 0 then
            raise Constraint_Error with
              "invalid exception handler: " & Description;
         end if;

         declare
            Base_Label : constant String :=
                           Description (Description'First .. Separator_1 - 1);
            Bound_Label : constant String :=
                            Description (Separator_1 + 1 .. Separator_2 - 1);
            Handler_Label : constant String :=
                              Description
                                (Separator_2 + 1 .. Description'Last);
         begin
            OS.Install_Exception_Handler
              (Base    => OS.Get_Symbol_Address (Base_Label),
               Bound   => OS.Get_Symbol_Address (Bound_Label),
               Handler => OS.Get_Symbol_Address (Handler_Label));
         end;
      end Install_Exception_Handler;

      ----------------
      -- Load_Notes --
      ----------------

      procedure Load_Notes
        (Section : Section_Entry)
      is
         use System.Storage_Elements;
         Data  : Storage_Array (1 .. Storage_Count (Get_Size (Section)));
         Index : Storage_Offset := 0;

         function Next_String (Length : Natural) return String;
         function Next_Word return Word_32;
         function Next_Octet return Word_8;

         ----------------
         -- Next_Octet --
         ----------------

         function Next_Octet return Word_8 is
         begin
            Index := Index + 1;
            return Word_8 (Data (Index));
         end Next_Octet;

         -----------------
         -- Next_String --
         -----------------

         function Next_String (Length : Natural) return String is
         begin
            return S : String (1 .. Length) do
               for Ch of S loop
                  Ch := Character'Val (Next_Octet);
               end loop;
               if Length mod 4 /= 0 then
                  for I in 1 .. 4 - Length mod 4 loop
                     declare
                        X : constant Word_8 := Next_Octet;
                     begin
                        pragma Assert (X = 0);
                     end;
                  end loop;
               end if;
            end return;
         end Next_String;

         ---------------
         -- Next_Word --
         ---------------

         function Next_Word return Word_32 is
         begin
            return W : Word_32 := 0 do
               for I in 1 .. 4 loop
                  W := W * 256 + Word_32 (Next_Octet);
               end loop;
            end return;
         end Next_Word;

      begin
         Read (File, Get_Offset (Section), Data);
         while Index < Data'Last loop
            declare
               Name_Length : constant Word_32 := Next_Word;
               Desc_Length : constant Word_32 := Next_Word;
               Tag         : constant Word_32 := Next_Word;
               Name        : constant String :=
                               Next_String (Natural (Name_Length));
               Desc        : constant String :=
                               Next_String (Natural (Desc_Length));
            begin
               if Name = "exception_handler" then
                  Install_Exception_Handler (Desc);
               elsif On_Note /= null then
                  On_Note (Name, Tag, Desc);
               end if;
            end;
         end loop;

      end Load_Notes;

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

      OS.Start_Load (Ada.Directories.Base_Name (Path));

      Open (File, In_File, Path);
      Iterate_Sections (File, Sht_Progbits, Load_Program_Bits'Access);
      Iterate_Symbols (File, Load_Symbol'Access);
      Iterate_Relocation (File, Load_Relocation'Access);

      OS.Resolve_Pending_References;

      Iterate_Sections (File, Sht_Note, Load_Notes'Access);

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
