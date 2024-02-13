with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Aqua.IO;
with Aqua.Options;

package body Aqua.Images is

   Trace_Link : Boolean := False;
   Trace_Load : constant Boolean := False;
   Trace_Code : constant Boolean := False;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Image : Instance'Class;
      Binder : not null access
        procedure (Group_Name  : String;
                   Before      : Boolean;
                   Parent_Name : String;
                   Child_Name  : String;
                   Start       : Address_Type))
   is
      use Ada.Strings.Unbounded;
   begin
      for Binding of Image.Bindings loop
         Binder
           (Group_Name  => To_String (Binding.Group),
            Before      => Binding.Before,
            Parent_Name => To_String (Binding.Parent_Text),
            Child_Name  => To_String (Binding.Child_Text),
            Start       => Binding.Start);
      end loop;
   end Bind;

   -------------------------
   -- Get_Handler_Address --
   -------------------------

   function Get_Handler_Address
     (Image        : Instance'Class;
      Trap_Address : Address_Type)
      return Address_Type
   is
   begin
      for Info of Image.Handlers loop
         if Trap_Address in Info.Base_Address .. Info.Bound_Address then
            return Info.Handler_Address;
         end if;
      end loop;
      return 0;
   end Get_Handler_Address;

   ----------
   -- Link --
   ----------

   procedure Link
     (Image : in out Instance'Class)
   is
      use Link_Maps;
      Have_Error : Boolean := False;
   begin

      Trace_Link := Aqua.Options.Trace_Link;

      for Position in Image.Link_Map.Iterate loop
         declare
            Info : constant Link_Info := Element (Position);
         begin
            if Trace_Link then
               Ada.Text_IO.Put
                 ("[" & Aqua.IO.Hex_Image (Info.Value) & "]"
                  & Key (Position)
                  & ":");
            end if;

            for Ref of Info.References loop
               if Trace_Link then
                  Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (Ref.Addr));
               end if;
               if Ref.Branch then
                  declare
                     Target : constant Address_Type :=
                                Ref.Addr - Info.Value;
                  begin
                     if Ref.Addr > Info.Value then
                        Ada.Text_IO.Put_Line
                          ("branch backward: "
                           & IO.Hex_Image (Ref.Addr)
                           & " "
                           & IO.Hex_Image (Info.Value)
                           & " "
                           & IO.Hex_Image (Target));
                     end if;
                     Image.Set_Value
                             (Ref.Addr, Word_16_Size, Word (Target));
                  end;
               elsif Ref.Relative then
                  declare
                     W : constant Word := Info.Value - Ref.Addr;
                  begin
                     Image.Set_Word
                       (Ref.Addr, W);
                  end;
               else
                  Image.Set_Word
                    (Ref.Addr, Info.Value);
               end if;
            end loop;

            if Trace_Link then
               Ada.Text_IO.New_Line;
            end if;

            if not Info.Has_Value
              and then not Info.References.Is_Empty
            then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  (-Info.Defn_File)
                  & ": undefined reference: "
                  & Key (Position));
               Have_Error := True;
            end if;

            if Info.Start then
               Ada.Text_IO.Put_Line
                 ("start: " & Aqua.IO.Hex_Image (Info.Value));
               Image.Start := Info.Value;
            end if;

         end;
      end loop;

      for Handler of Image.Handlers loop
         declare
            procedure Check
              (Label : Ada.Strings.Unbounded.Unbounded_String;
               Addr  : out Address_Type);

            -----------
            -- Check --
            -----------

            procedure Check
              (Label : Ada.Strings.Unbounded.Unbounded_String;
               Addr  : out Address_Type)
            is
               Key : constant String := -Label;
            begin
               if not Image.Link_Map.Contains (Key)
                 or else not Image.Link_Map.Element (Key).Has_Value
               then
                  Ada.Text_IO.Put_Line
                    ("undefined reference to " & Key);
                  Have_Error := True;
               else
                  declare
                     Info : constant Link_Info :=
                              Image.Link_Map.Element (Key);
                  begin
                     Addr := Info.Value;
                  end;
               end if;
            end Check;

         begin
            Check (Handler.Base_Label, Handler.Base_Address);
            Check (Handler.Bound_Label, Handler.Bound_Address);
            Check (Handler.Handler_Label, Handler.Handler_Address);

            if Trace_Link and then not Have_Error then
               Ada.Text_IO.Put_Line
                 ((-Handler.Handler_Label)
                  & ": "
                  & Aqua.IO.Hex_Image (Handler.Base_Address)
                  & " .. "
                  & Aqua.IO.Hex_Image (Handler.Bound_Address));
            end if;
         end;
      end loop;

      for Segment of Image.Segment_Map loop
         Image.Set_Access_Flags
           (Segment.Base,
            Segment.Bound,
            R => Segment.R,
            W => Segment.W,
            X => Segment.X);
      end loop;

      if Have_Error then
         raise Constraint_Error with "Link error";
      end if;
   end Link;

   ----------
   -- Load --
   ----------

   procedure Load
     (Image : in out Instance'Class;
      Name  : in     String)
   is
      use Aqua.IO;
      File             : File_Type;
      Binding_Count    : Word;
      Handler_Count    : Word;
      Segment_Count    : Word;
      External_Count   : Word;
      Source_Loc_Count : Word;

      package Segment_Lists is
         new Ada.Containers.Doubly_Linked_Lists (Segment_Record);

      Segments         : Segment_Lists.List;

      function Convert_Address
        (Addr : Address_Type)
         return Address_Type;

      ---------------------
      -- Convert_Address --
      ---------------------

      function Convert_Address
        (Addr : Address_Type)
         return Address_Type
      is
      begin
         for Segment of Segments loop
            if Addr >= Segment.Base
              and then Addr < Segment.Bound
            then
               return Image.Segment_Map.Element (-Segment.Name).Bound
                 + Addr - Segment.Base;
            end if;
         end loop;
         raise Constraint_Error with
           "bad address: " & Aqua.IO.Hex_Image (Addr);
      end Convert_Address;

   begin

      if Trace_Load then
         Ada.Text_IO.Put_Line ("image: loading " & Name);
      end if;

      Open (File, Name);

      Read_Word (File, Binding_Count);
      Read_Word (File, Handler_Count);
      Read_Word (File, Segment_Count);
      Read_Word (File, External_Count);
      Read_Word (File, Source_Loc_Count);

      if Trace_Load then
         Ada.Text_IO.Put_Line
           (Name
            & ": bindings:" & Word'Image (Binding_Count)
            & "; handlers:" & Word'Image (Handler_Count)
            & "; segments:" & Word'Image (Segment_Count)
            & "; externals:" & Word'Image (External_Count)
            & "; source locs:" & Word'Image (Source_Loc_Count));
      end if;

      for I in 1 .. Segment_Count loop
         declare
            Name    : constant String := Read_String_Literal (File);
            Rec     : Segment_Record;
            Flags   : Word_8;
         begin
            Read_Word_8 (File, Flags);
            Read_Word (File, Rec.Base);
            Read_Word (File, Rec.Bound);

            Rec.R := (Flags and 1) = 1;
            Rec.W := (Flags and 2) = 2;
            Rec.X := (Flags and 4) = 4;
            Rec.Initialised := (Flags and 8) = 8;

            if Trace_Load then
               Ada.Text_IO.Put (Name);
               Ada.Text_IO.Set_Col (12);
               Ada.Text_IO.Put
                 (if Rec.R then "r" else "-");
               Ada.Text_IO.Put
                 (if Rec.W then "w" else "-");
               Ada.Text_IO.Put
                 (if Rec.X then "x" else "-");
               Ada.Text_IO.Put
                 (if Rec.Initialised then "i" else "-");

               Ada.Text_IO.Put (" ");
               Ada.Text_IO.Put
                 (Aqua.IO.Hex_Image (Rec.Base)
                  & " "
                  & Aqua.IO.Hex_Image (Rec.Bound));
               Ada.Text_IO.New_Line;
            end if;

            Rec.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
            Segments.Append (Rec);

         end;
      end loop;

      for I in 1 .. Binding_Count loop
         declare
            use Ada.Strings.Unbounded;
            Header     : Word;
            Start      : Address_Type;
            Has_Parent : Boolean := False;
            Has_Child  : Boolean := False;
            Binding    : Binding_Info;
         begin
            Read_Word (File, Header);

            Binding.Before := (Header and 1) = 1;
            Has_Parent := (Header and 2) = 2;
            Has_Child := (Header and 4) = 4;

            Binding.Group := To_Unbounded_String (Read_String_Literal (File));

            if Has_Parent then
               Binding.Parent_Text :=
                 To_Unbounded_String (Read_String_Literal (File));
            end if;

            if Has_Child then
               Binding.Child_Text :=
                 To_Unbounded_String (Read_String_Literal (File));
            end if;

            Read_Address (File, Start);
            Binding.Start := Start;
--              Binding.Start := Start + Image.High;

            Image.Bindings.Append (Binding);

         end;
      end loop;

      declare
         use Ada.Strings.Unbounded;
         Source_File_Name : constant Unbounded_String :=
                              To_Unbounded_String (Read_String_Literal (File));
         Start            : Address_Type;
         Line, Column     : Word;
      begin
         for I in 1 .. Source_Loc_Count loop
            Read_Word (File, Line);
            Read_Word (File, Column);
            Read_Address (File, Start);
            Image.Locations.Append
              ((Source_File_Name, Start + Image.Code_Base,
               Natural (Line), Natural (Column)));
         end loop;
      end;

      for Segment of Segments loop
         if Segment.Initialised and then Segment.Bound > Segment.Base then
            declare
               Rec : constant Segment_Record :=
                       Image.Segment_Map.Element (-Segment.Name);
            begin

               if Trace_Load then
                  Ada.Text_IO.Put_Line
                    ("Reading segment "
                     & (-Rec.Name)
                     & ": source "
                     & Aqua.IO.Hex_Image (Segment.Base)
                     & " .. "
                     & Aqua.IO.Hex_Image (Segment.Bound)
                     & "; destination "
                     & Aqua.IO.Hex_Image (Rec.Bound));
               end if;

               Image.Set_Access_Flags
                 (Base  => Rec.Bound,
                  Bound => Rec.Bound + (Segment.Bound - Segment.Base),
                  R     => True,
                  W     => True,
                  X     => False);

               for Addr in Segment.Base .. Segment.Bound - 1 loop

                  declare
                     Target : constant Address_Type :=
                                Addr - Segment.Base + Rec.Bound;
                     X      : Word_8;
                  begin
                     if Trace_Code then
                        if Target mod 16 = 0 then
                           if Addr > Segment.Base then
                              Ada.Text_IO.New_Line;
                           end if;
                           Ada.Text_IO.Put
                             (Aqua.IO.Hex_Image (Target));
                        end if;
                     end if;

                     Read_Word_8 (File, X);

                     if Trace_Code then
                        Ada.Text_IO.Put (" " & Aqua.IO.Hex_Image (X));
                     end if;

                     Image.Set_Word_8 (Target, X);
                  end;
               end loop;

               if Trace_Code then
                  Ada.Text_IO.New_Line;
               end if;

            end;
         end if;
      end loop;

      if Trace_Code then
         Ada.Text_IO.New_Line;
      end if;

      for I in 1 .. External_Count loop
         declare
            Length   : Word;
            Refs     : Word;
            Flags    : Word_8;
            Defined  : Boolean;
            Deferred : Boolean;
            Start    : Boolean;
            Exists   : Boolean := False;
         begin
            Read_Word (File, Length);
            Read_Word (File, Refs);
            Read_Word_8 (File, Flags);

            Defined := (Flags and 1) = 1;
            Deferred := (Flags and 2) = 2;
            Start := (Flags and 4) = 4;

            declare
               S : String (1 .. Natural (Length));
               X : Word_8;
               Info : Link_Info;
            begin
               for J in S'Range loop
                  Read_Word_8 (File, X);
                  S (J) := Character'Val (X);
               end loop;

               if Image.Link_Map.Contains (S) then
                  Exists := True;
                  Info := Image.Link_Map (S);

                  if Info.Has_Value and then Defined then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "multiple definitions for " & S
                        & " in " & (-Info.Defn_File)
                        & " and " & Ada.Directories.Base_Name (Name));
                  end if;

                  if Trace_Load and then Deferred then
                     Ada.Text_IO.Put_Line
                       (S & ": map contains deferred entry");
                  end if;
               end if;

               Info.Start := Start;
               Info.Defn_File := +(Ada.Directories.Base_Name (Name));

               if Defined then
                  Info.Has_Value := True;
               end if;

               if Trace_Load then
                  if Exists then
                     Ada.Text_IO.Put ("e");
                  else
                     Ada.Text_IO.Put ("-");
                  end if;
                  if Info.Has_Value then
                     Ada.Text_IO.Put ("v");
                  else
                     Ada.Text_IO.Put ("-");
                  end if;
                  if Deferred then
                     Ada.Text_IO.Put ("d");
                  else
                     Ada.Text_IO.Put ("-");
                  end if;

                  Ada.Text_IO.Put ("-");

                  Ada.Text_IO.Put
                    (Integer'Image (Integer (I)) & ": "
                     & S);
                  Ada.Text_IO.Set_Col (60);
               end if;

               Image.Label_Vector.Append (S);

               if Defined then
                  Read_Word (File, Info.Value);
                  Info.Value := Convert_Address (Info.Value);
               end if;

               if Info.Has_Value and then Trace_Load then
                  Ada.Text_IO.Put (Aqua.IO.Hex_Image (Info.Value));
               end if;

               if Trace_Load then
                  Ada.Text_IO.Set_Col (72);
               end if;

               for J in 1 .. Refs loop
                  declare
                     Addr : Address_Type;
                     Relative : Word_8;
                  begin
                     Read_Address (File, Addr);
                     Read_Word_8 (File, Relative);
                     Info.References.Append
                       ((Addr     => Convert_Address (Addr),
                         Relative => Boolean'Val (Relative mod 2),
                         Branch   => Boolean'Val (Relative / 2 mod 2)));
                     if Trace_Load then
                        Ada.Text_IO.Put
                          (" " & Aqua.IO.Hex_Image (Convert_Address (Addr)));
                        Ada.Text_IO.Put
                          ((if Relative mod 2 = 1 then "r" else ""));
                        Ada.Text_IO.Put
                          ((if Relative / 2 mod 2 = 1 then "b" else ""));
                     end if;
                  end;
               end loop;

               if Trace_Load then
                  Ada.Text_IO.New_Line;
               end if;

               if Image.Link_Map.Contains (S) then
                  Image.Link_Map (S) := Info;
               else
                  Image.Link_Map.Insert (S, Info);
               end if;

            end;
         end;

      end loop;

      for I in 1 .. Handler_Count loop
         declare
            Base    : constant String :=
                        Aqua.IO.Read_String_Literal (File);
            Bound   : constant String :=
                        Aqua.IO.Read_String_Literal (File);
            Handler : constant String :=
                        Aqua.IO.Read_String_Literal (File);
         begin
            Image.Handlers.Append
              (Exception_Info'
                 (Base_Label    => +Base,
                  Bound_Label   => +Bound,
                  Handler_Label => +Handler,
                  others        => 0));
         end;
      end loop;

      for Segment of Segments loop
         if Segment.Initialised and then Segment.Bound > Segment.Base then
            declare
               Rec : Segment_Record renames
                       Image.Segment_Map (-Segment.Name);
            begin
               Rec.Bound := Rec.Bound + (Segment.Bound - Segment.Base);
            end;
         end if;
      end loop;

      Close (File);

   end Load;

   ---------------
   -- New_Image --
   ---------------

   function New_Image return Image_Type is

      Image : constant Image_Type := new Instance;

      procedure Add_Segment
        (Name        : String;
         Base        : Address_Type;
         Size        : Word := 0;
         Readable    : Boolean := True;
         Writable    : Boolean := False;
         Executable  : Boolean := False;
         Initialised : Boolean := True);

      -----------------
      -- Add_Segment --
      -----------------

      procedure Add_Segment
        (Name        : String;
         Base        : Address_Type;
         Size        : Word := 0;
         Readable    : Boolean := True;
         Writable    : Boolean := False;
         Executable  : Boolean := False;
         Initialised : Boolean := True)
      is
         Rec : constant Segment_Record :=
                 Segment_Record'
                   (Name        => +Name,
                    R           => Readable,
                    W           => Writable,
                    X           => Executable,
                    Initialised => Initialised,
                    Base        => Base,
                    Bound       => Base + Size);
      begin
         Image.Segment_Map.Insert (Name, Rec);
      end Add_Segment;

   begin
      Add_Segment ("code", 16#0000_1000#, Executable => True);
      Add_Segment ("text", 16#1000_0000#);
      Add_Segment ("data", 16#2000_0000#, Writable => True);
      Add_Segment ("heap", 16#4000_0000#,
                   Size     => 16#4000_0000#,
                   Writable => True, Initialised => False);
      return Image;
   end New_Image;

   ----------
   -- Save --
   ----------

   procedure Save
     (Image : Instance'Class;
      Path  : String)
   is
      pragma Unreferenced (Image);
      pragma Unreferenced (Path);
   begin
      null;
   end Save;

   ------------------
   -- Segment_Base --
   ------------------

   function Segment_Base
     (Image : Instance'Class;
      Name  : String)
      return Address_Type
   is
   begin
      return Image.Segment_Map.Element (Name).Base;
   end Segment_Base;

   -------------------
   -- Segment_Bound --
   -------------------

   function Segment_Bound
     (Image : Instance'Class;
      Name  : String)
      return Address_Type
   is
   begin
      return Image.Segment_Map.Element (Name).Bound;
   end Segment_Bound;

   ------------------
   -- Segment_Size --
   ------------------

   function Segment_Size
     (Image : Instance'Class;
      Name  : String)
      return Word
   is
      S : constant Segment_Record := Image.Segment_Map.Element (Name);
   begin
      return S.Bound - S.Base;
   end Segment_Size;

   ----------
   -- Show --
   ----------

   function Show (Image : Instance'Class;
                  Value : Word)
                  return String
   is (Aqua.IO.Hex_Image (Value));

   --------------------------------
   -- Show_Known_Source_Position --
   --------------------------------

   function Show_Known_Source_Position
     (Image : Instance'Class;
      Addr  : Address_Type)
      return String
   is
      use Ada.Strings, Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
      File_Name : Unbounded_String := To_Unbounded_String ("unknown");
      Line      : Natural := 0;
      Column    : Natural := 0;
   begin
      for Loc of Image.Locations loop
         if Loc.Start > Addr then
            declare
               Name : constant String := To_String (File_Name);
            begin
               if Name /= "" then
                  if Line > 1 or else Column > 1 then
                     return Ada.Directories.Simple_Name (To_String (File_Name))
                       & ":" & Trim (Natural'Image (Line), Left)
                       & ":" & Trim (Natural'Image (Column), Left);
                  else
                     return Ada.Directories.Simple_Name
                       (To_String (File_Name));
                  end if;
               end if;
            end;
         else
            File_Name := Loc.Source_File;
            Line      := Loc.Line;
            Column    := Loc.Column;
         end if;
      end loop;

      return "";

   end Show_Known_Source_Position;

   --------------------------
   -- Show_Source_Position --
   --------------------------

   function Show_Source_Position
     (Image : Instance'Class;
      Addr  : Address_Type)
      return String
   is
      use Ada.Strings, Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
      File_Name : Unbounded_String := To_Unbounded_String ("unknown");
      Line      : Natural := 0;
      Column    : Natural := 0;
   begin
      for Loc of Image.Locations loop
         if Loc.Start > Addr then
            declare
               Name : constant String := To_String (File_Name);
            begin
               if Name /= "" then
                  return Ada.Directories.Base_Name (To_String (File_Name))
                    & ":" & Trim (Natural'Image (Line), Left)
                    & ":" & Trim (Natural'Image (Column), Left);
               end if;
            end;
         else
            File_Name := Loc.Source_File;
            Line      := Loc.Line;
            Column    := Loc.Column;
         end if;
      end loop;

      return "unknown";

   end Show_Source_Position;

end Aqua.Images;
