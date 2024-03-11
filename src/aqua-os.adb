with Ada.Text_IO;

with Aqua.Files;
with Aqua.Images;
with Aqua.Linker;
with Aqua.Names;

with Aqua.Devices.Linker;
with Aqua.Devices.MM;

package body Aqua.OS is

   Trace_Relocation : constant Boolean := False;
   Trace_Loads      : Boolean := False;

   procedure Resolve_Reference
     (This      : not null access Instance'Class;
      Reference : Undefined_Reference;
      Op        : Word_8;
      R         : Word_8);

   ------------
   -- Create --
   ------------

   function Create
     (Bus : not null access Aqua.Bus.Instance'Class) return Reference
   is
      This : constant Reference := new Instance;

      procedure Process
        (Base, Bound : Address_Type;
         Device      : Aqua.Devices.Reference);

      -------------
      -- Process --
      -------------

      procedure Process
        (Base, Bound : Address_Type;
         Device      : Aqua.Devices.Reference)
      is
         Name : constant String := Device.Name;
      begin
         if Name = "RAM" then
            declare
               Page_Base  : constant Physical_Page_Address :=
                              To_Physical_Page (Base)
                              + (if Base mod Page_Size = 0
                                 then 0 else 1);
               Page_Bound : constant Physical_Page_Address :=
                              To_Physical_Page (Bound);
            begin
               if Page_Base < Page_Bound then
                  declare
                     Rec : constant Page_Range_Record :=
                             Page_Range_Record'
                               (Available, Page_Base, Page_Bound);
                  begin
                     This.Pages.Append (Rec);
                     This.Free_Pages.Append (Rec);
                  end;
               end if;
            end;
         elsif Name = "ld" then
            Aqua.Devices.Linker.Set_Manager (Device, This);
         elsif Name = "mm" then
            Aqua.Devices.MM.Set_Manager (Device, This);
         end if;
      end Process;

   begin
      This.Bus := Aqua.Bus.Reference (Bus);
      Bus.Scan_Devices (Process'Access);
      This.Protection := [
                          (16#100#, 16#1FF#, (True, False, True)),
                          (16#200#, 16#2FF#, (True, False, False)),
                          (16#300#, 16#3FF#, (True, True, False)),
                          (16#800#, 16#EFF#, (True, True, False)),
                          (16#400#, 16#7FF#, (True, True, False))
                         ];
      return This;
   end Create;

   -------------------
   -- Define_Symbol --
   -------------------

   procedure Define_Symbol
     (Loader : Loader_Type;
      Module : String;
      Name   : String;
      Offset : Word_32)
   is
      Full_Name : constant String :=
                    Module & "." & Name;
   begin
      if Trace_Relocation then
         Ada.Text_IO.Put_Line
           (Full_Name & " = "
            & Aqua.Images.Hex_Image (Address_Type (Offset) + Loader.Base));
      end if;

      if Loader.OS.Symbols.Contains (Full_Name) then
         if Loader.OS.Symbols (Full_Name).Defined then
            raise Constraint_Error with
              "redefinition of symbol " & Full_Name;
         end if;
         Loader.OS.Symbols.Delete (Full_Name);
      end if;

      Loader.OS.Symbols.Insert
        (Full_Name,
         Symbol_Reference'
           (Name'Length, Name, True, Address_Type (Offset) + Loader.Base));
   end Define_Symbol;

   ------------------------
   -- External_Reference --
   ------------------------

   procedure External_Reference
     (This   : in out Instance'Class;
      Module : String;
      Name   : String)
   is
   begin
      if Trace_Relocation then
         Ada.Text_IO.Put_Line
           ("EXT: " & Module & "." & Name);
      end if;

      declare
         Full_Name : constant String :=
                       Module & "." & Name;
      begin
         if not This.Symbols.Contains (Full_Name) then
            This.Symbols.Insert
              (Full_Name,
               Symbol_Reference'
                 (Name'Length, Name, False, 0));
         end if;
      end;

   end External_Reference;

   ----------
   -- Find --
   ----------

   function Find
     (Map  : Module_Maps.Map;
      Addr : Address_Type)
      return Module_Maps.Cursor
   is
      Best_Base : Address_Type := 0;
      Best_Pos  : Module_Maps.Cursor := Module_Maps.No_Element;
   begin
      for Position in Map.Iterate loop
         declare
            Element : constant Module_Reference := Map (Position);
            Base    : constant Address_Type :=
                        Element.Virt_Base (Code);
         begin
            if Addr >= Base
              and then Base > Best_Base
            then
               Best_Base := Base;
               Best_Pos  := Position;
            end if;
         end;
      end loop;
      return Best_Pos;
   end Find;

   ---------------------
   -- Get_Module_Name --
   ---------------------

   overriding function Get_Module_Name
     (This    : Instance;
      Address : Address_Type)
      return String
   is
      Position : constant Module_Maps.Cursor := Find (This.Modules, Address);
   begin
      if Module_Maps.Has_Element (Position) then
         return Module_Maps.Element (Position).Name;
      else
         return "";
      end if;
   end Get_Module_Name;

   -------------------------
   -- Get_Protection_Bits --
   -------------------------

   function Get_Protection_Bits
     (This    : Instance;
      Page    : Virtual_Page_Address)
      return Protection_Bits
   is
   begin
      for Element of This.Protection loop
         if Page in Element.Base .. Element.Bound - 1 then
            if False then
               Ada.Text_IO.Put_Line
                 (Aqua.Images.Hex_Image (Word_32 (Page) * 16#1000#)
                  & ": "
                  & (if Element.Protection.R then "r" else "-")
                  & (if Element.Protection.W then "w" else "-")
                  & (if Element.Protection.X then "x" else "-"));
            end if;
            return Element.Protection;
         end if;
      end loop;
      return (others => False);
   end Get_Protection_Bits;

   ------------------------
   -- Get_Symbol_Address --
   ------------------------

   function Get_Symbol_Address
     (This : Instance;
      Name : String)
      return Address_Type
   is
      Position : constant Symbol_Reference_Maps.Cursor :=
                   This.Symbols.Find (Name);
   begin
      if Symbol_Reference_Maps.Has_Element (Position) then
         declare
            Rec : Symbol_Reference renames This.Symbols (Position);
         begin
            if Rec.Defined then
               return Rec.Virtual_Addr;
            else
               return 0;
            end if;
         end;
      else
         return 0;
      end if;
   end Get_Symbol_Address;

   ----------
   -- Load --
   ----------

   function Load
     (This     : not null access Instance'Class;
      Data     : System.Storage_Elements.Storage_Array;
      Segment  : Memory_Segment)
      return Loader_Type
   is

      Loader : constant Loader_Type := Loader_Type'
        (Base => This.Bounds (Segment),
         Bound => This.Bounds (Segment) + Address_Type (Data'Length),
         OS    => Reference (This),
         Segment => Segment);

      Dest : Address_Type := This.Bounds (Segment);

   begin

      if Trace_Loads then
         Ada.Text_IO.Put_Line
           ("load: segment " & Segment'Image
            & " start " & Aqua.Images.Hex_Image (Dest)
            & " phys " & Aqua.Images.Hex_Image (This.Map (Dest))
            & " size "
            & System.Storage_Elements.Storage_Count'Image
              (Data'Length));
      end if;

      for Item of Data loop
         This.Bus.Set_Word_8
           (Address => This.Map (Dest),
            Value   => Word_8 (Item));
         Dest := Dest + 1;
      end loop;
      This.Bounds (Segment) := Dest;

      return Loader;

   end Load;

   ---------
   -- Map --
   ---------

   overriding function Map
     (This    : in out Instance;
      Address : Address_Type;
      Read    : Boolean := False;
      Write   : Boolean := False;
      Execute : Boolean := False)
      return Address_Type
   is
      use Map_Entry_Lists;
      Virtual_Page : constant Virtual_Page_Address :=
                       To_Virtual_Page (Address);
      Position : Cursor := This.Memory_Map.First;
   begin
      if Address >= 16#F000_0000# then
         return Address;
      end if;

      declare
         Bits : constant Protection_Bits :=
                  This.Get_Protection_Bits (Virtual_Page);
      begin
         if Read and then not Bits.R then
            raise Aqua.MM.Protection_Fault with
              "read attempt: " & Aqua.Images.Hex_Image (Address);
         end if;
         if Write and then not Bits.W then
            raise Aqua.MM.Protection_Fault with
              "write attempt: " & Aqua.Images.Hex_Image (Address);
         end if;
         if Execute and then not Bits.X then
            raise Aqua.MM.Protection_Fault with
              "execution attempt: " & Aqua.Images.Hex_Image (Address);
         end if;
      end;

      while Has_Element (Position)
        and then Element (Position).Virtual_Page /= Virtual_Page
      loop
         Next (Position);
      end loop;

      if Has_Element (Position) then
         if Position /= This.Memory_Map.First then
            This.Memory_Map.Insert (This.Memory_Map.First, Element (Position));
            This.Memory_Map.Delete (Position);
            Position := This.Memory_Map.First;
         end if;
      elsif not This.Free_Pages.Is_Empty then
         declare
            First : Page_Range_Record renames
                      This.Free_Pages (This.Free_Pages.First);
         begin
            if False then
               Ada.Text_IO.Put_Line
                 ("map: "
                  & Aqua.Images.Hex_Image
                    (Word_32 (Virtual_Page) * Page_Size)
                  & " -> "
                  & Aqua.Images.Hex_Image
                    (Word_32 (First.Base) * Page_Size));
            end if;

            This.Memory_Map.Insert
              (This.Memory_Map.First,
               Map_Entry'(Virtual_Page, First.Base));
            First.Base := First.Base + 1;
         end;
         declare
            First : constant Page_Range_Record :=
                      This.Free_Pages.First_Element;
         begin
            if First.Base = First.Bound then
               This.Free_Pages.Delete_First;
            end if;
         end;
         Position := This.Memory_Map.First;
      else
         raise Out_Of_Memory;
      end if;

      return Address_Type (Element (Position).Physical_Page) * Page_Size
        + Address mod Page_Size;

   end Map;

   --------------
   -- Relocate --
   --------------

   procedure Relocate
     (Loader     : Loader_Type;
      Sym_Loader : Loader_Type;
      Name       : String;
      Offset     : Word_32;
      Value      : Word_32;
      Context    : Relocation_Context;
      Defined    : Boolean)
   is
      Address : constant Address_Type :=
                  Loader.OS.Map (Offset + Loader.Base);
      Loader_Addr : constant Address_Type :=
                      Offset + Loader.Base;
      Loader_Value : constant Word_32 := Value + Sym_Loader.Base;
   begin

      if Trace_Relocation then
         Ada.Text_IO.Put_Line
           ("Relocate: " & Name
            & ": loader base=" & Aqua.Images.Hex_Image (Loader.Base)
            & "; sym base=" & Aqua.Images.Hex_Image (Sym_Loader.Base)
            & "; offset=" & Aqua.Images.Hex_Image (Offset)
            & "; value=" & Aqua.Images.Hex_Image (Value)
            & "; context=" & Context'Image
            & "; defined=" & Defined'Image);
      end if;

      if not Defined then

         if Context = No_Context then

            if Trace_Relocation then
               Ada.Text_IO.Put_Line
                 ("no context: " & Name
                  & " at "
                  & Aqua.Images.Hex_Image (Loader_Addr));
            end if;

            declare
               Position : constant Undefined_Reference_Maps.Cursor :=
                            Loader.OS.Pending_Refs.Find (Loader_Addr);
            begin
               if not Undefined_Reference_Maps.Has_Element (Position) then
                  Loader.OS.Pending_Refs.Insert
                    (Loader_Addr,
                     Undefined_Reference'
                       (Name'Length, Name, Loader_Addr, Context));
               end if;
            end;

         else
            declare
               Op, R : Word_8;
               Trap  : Word_32;
            begin
               Loader.OS.Bus.Get_Word_8 (Address, Op);
               Loader.OS.Bus.Get_Word_8 (Address + 1, R);

               Trap :=
                 16#0002_0000#
               + Word_32 (Op) * 256
                 + Word_32 (R);

               Loader.OS.Bus.Set_Word_32
                 (Address, Trap);

               Loader.OS.Refs.Insert
                 (Offset + Loader.Base,
                  (Name'Length, Name, Offset + Loader.Base, Context));
            end;
         end if;
      else
         case Context is
            when No_Context =>
               if Trace_Relocation then
                  Ada.Text_IO.Put_Line
                    ("set no context " & Name
                     & " at " & Aqua.Images.Hex_Image (Loader_Addr)
                     & " (phys " & Aqua.Images.Hex_Image (Address) & ")"
                     & " to " & Aqua.Images.Hex_Image (Loader_Value));
               end if;
               Loader.OS.Bus.Set_Word_32 (Address, Loader_Value);
            when Relative_XY =>
               declare
                  Op      : Word_8;
                  Offset  : constant Word_32 :=
                              (if Loader_Value >= Loader_Addr
                               then Loader_Value - Loader_Addr
                               else (not (Loader_Addr - Loader_Value)) + 1)
                              / 4;
               begin
                  Loader.OS.Bus.Get_Word_8 (Address, Op);
                  Op := Op - Op mod 2;

                  if Loader_Value < Loader_Addr then
                     Op := Op + 1;
                  end if;

                  declare
                     W : Word_32;
                  begin
                     Loader.OS.Bus.Get_Word_32 (Address, W);
                     W :=
                       Word_32 (Op) * 2 ** 24
                       + (W and (255 * 2 ** 16))
                       + Offset mod 65536;
                     Loader.OS.Bus.Set_Word_32 (Address, W);
                  end;
               end;
            when Relative_XYZ =>
               declare
                  Op      : Word_8;
                  Offset  : constant Word_32 :=
                              (if Loader_Value >= Loader_Addr
                               then Loader_Value - Loader_Addr
                               else (not (Loader_Addr - Loader_Value)) + 1)
                              / 4;
                  Code    : Word_32;
               begin

                  Loader.OS.Bus.Get_Word_8 (Address, Op);
                  Op := Op - Op mod 2;

                  if Loader_Value < Loader_Addr then
                     Op := Op + 1;
                     Loader.OS.Bus.Set_Word_8 (Address, Op);
                  end if;

                  Code := Word_32 (Op) * 2 ** 24
                    + Offset mod 2 ** 24;
                  Loader.OS.Bus.Set_Word_32 (Address, Code);
               end;
         end case;

      end if;
   end Relocate;

   ------------------
   -- Request_Heap --
   ------------------

   overriding function Request_Heap
     (This : in out Instance;
      Size : Word_32)
      return Address_Type
   is
      Actual_Size : constant Word_32 := Size * 1024;
      Result      : constant Address_Type := This.Bounds (Heap);
   begin
      if Actual_Size > 65536 then
         Ada.Text_IO.Put_Line
           ("size too big: " & Aqua.Images.Hex_Image (Actual_Size));
         return 0;
      end if;

      This.Bounds (Heap) := @ + Actual_Size;
      if False then
         Ada.Text_IO.Put_Line
           ("request size "
            & Aqua.Images.Hex_Image (Actual_Size)
            & "; returning "
            & Aqua.Images.Hex_Image (Result));
      end if;
      return Result;
   end Request_Heap;

   -------------
   -- Resolve --
   -------------

   overriding procedure Resolve
     (This   : not null access Instance;
      Address : Address_Type;
      Op      : Word_8;
      R       : Word_8)
   is
      Ref : constant Undefined_Reference_Maps.Cursor :=
              This.Refs.Floor (Address);

   begin
      if not Undefined_Reference_Maps.Has_Element (Ref) then
         raise Constraint_Error with
           "no relocation found at "
           & Aqua.Images.Hex_Image (Word_32 (Address));
      else
         This.Resolve_Reference
           (Undefined_Reference_Maps.Element (Ref), Op, R);
      end if;
   end Resolve;

   -----------------------
   -- Resolve_Reference --
   -----------------------

   procedure Resolve_Reference
     (This      : not null access Instance'Class;
      Reference : Undefined_Reference;
      Op        : Word_8;
      R         : Word_8)
   is

      Address : constant Address_Type := Reference.Offset;

      procedure Update
        (Value : Word_32);

      ------------
      -- Update --
      ------------

      procedure Update
        (Value : Word_32)
      is
      begin
         case Reference.Context is
            when No_Context =>
               This.Bus.Set_Word_32
                 (This.Map (Address), Value);
               if Trace_Relocation then
                  Ada.Text_IO.Put_Line
                    ("set no context " & Aqua.Images.Hex_Image (Address)
                     & " to " & Aqua.Images.Hex_Image (Value));
               end if;

            when Relative_XY =>
               declare
                  Offset   : constant Word_32 :=
                               (if Value >= Address
                                then Value - Address
                                else (not (Address - Value)) + 1)
                               / 4;
                  W        : Word_32;
                  Op_Code  : constant Word_8 := Op - Op mod 2;
                  Op_Value : constant Word_32 :=
                               (Word_32 (Op_Code)
                                + (if Value < Address then 1 else 0))
                               * 2 ** 24;
               begin
                  W := Op_Value
                    + (Word_32 (R) * 2 ** 16)
                    + Offset mod 65536;
                  This.Bus.Set_Word_32 (This.Map (Address), W);
               end;
            when Relative_XYZ =>
               declare
                  Offset  : constant Word_32 :=
                              (if Value >= Address
                               then Value - Address
                               else (not (Address - Value)) + 1)
                              / 4;
               begin

                  declare
                     W        : Word_32;
                     Op_Code  : constant Word_8 := Op - Op mod 2;
                     Op_Value : constant Word_32 :=
                                  (Word_32 (Op_Code)
                                   + (if Value < Address then 1 else 0))
                                  * 2 ** 24;
                  begin
                     W := Op_Value
                       + Offset mod 2 ** 24;
                     This.Bus.Set_Word_32 (This.Map (Address), W);
                  end;
               end;
         end case;
      end Update;

      Full_Name   : constant String := Reference.Name;
      Module_Name : constant String :=
                      Aqua.Names.Get_Module_Name (Full_Name);
   begin
      if Trace_Relocation then
         Ada.Text_IO.Put_Line
           ("looking for: " & Full_Name);
      end if;

      if This.Symbols.Contains (Full_Name)
        and then This.Symbols (Full_Name).Defined
      then
         if Trace_Relocation then
            Ada.Text_IO.Put_Line
              ("Found " & Full_Name & " at "
               & Aqua.Images.Hex_Image
                 (This.Symbols (Full_Name).Virtual_Addr));
         end if;
         Update (This.Symbols (Full_Name).Virtual_Addr);
      elsif Module_Name = "" then
         raise Aqua.Ld.Undefined_Reference with
           "undefined: " & Full_Name;
      else
         if not This.Modules.Contains (Module_Name) then
            declare
               File_Name : constant String :=
                             Aqua.Names.To_File_Name (Module_Name);
               Path      : constant String :=
                             Aqua.Files.Find_File (File_Name);
            begin
               if Path = "" then
                  raise Constraint_Error with
                    "cannot find module: " & Module_Name;
               end if;

               Aqua.Linker.Load (This, Path);
            end;
         end if;

         if This.Symbols.Contains (Full_Name) then
            if Trace_Relocation then
               Ada.Text_IO.Put_Line
                 ("Found " & Full_Name & " at "
                  & Aqua.Images.Hex_Image
                    (This.Symbols (Full_Name).Virtual_Addr));
            end if;
            Update (This.Symbols (Full_Name).Virtual_Addr);
         else
            raise Constraint_Error with
              "label " & Aqua.Names.Get_Local_Name (Full_Name)
              & " not defined in module " & Module_Name;
         end if;
      end if;
   end Resolve_Reference;

   --------------------------------
   -- Resolve_Pending_References --
   --------------------------------

   procedure Resolve_Pending_References
     (This : not null access Instance'Class)
   is
   begin
      while not This.Pending_Refs.Is_Empty loop
         declare
            Ref : constant Undefined_Reference :=
                    This.Pending_Refs.First_Element;
         begin
            This.Pending_Refs.Delete_First;
            pragma Assert (Ref.Context = No_Context,
                           "expected a no context reference for "
                           & Ref.Name
                           & " in pending reference list");
            This.Resolve_Reference (Ref, 0, 0);
         end;
      end loop;
   end Resolve_Pending_References;

   ----------------
   -- Start_Load --
   ----------------

   procedure Start_Load
     (This : in out Instance;
      Name : String)
   is
      Bases : constant Segment_Address_Array := This.Bounds;
   begin
      This.Modules.Insert
        (Name,
         Module_Reference'
           (Name'Length, Name,
            Virt_Base => Bases,
            Phys_Base => [others => 0]));
   end Start_Load;

   -----------------------------
   -- To_Module_Local_Address --
   -----------------------------

   overriding function To_Module_Local_Address
     (This    : Instance;
      Address : Address_Type)
      return Address_Type
   is
      Position : constant Module_Maps.Cursor := Find (This.Modules, Address);
   begin
      if Module_Maps.Has_Element (Position) then
         return Address - Module_Maps.Element (Position).Virt_Base (Code);
      else
         return Address;
      end if;
   end To_Module_Local_Address;

   -------------------
   -- Trace_Loading --
   -------------------

   procedure Trace_Loading (Enabled : Boolean) is
   begin
      Trace_Loads := Enabled;
   end Trace_Loading;

end Aqua.OS;
