with Ada.Characters.Handling;
with Ada.Strings.Fixed;

package body Aqua.Names is

   --------------------
   -- Get_Local_Name --
   --------------------

   function Get_Local_Name
     (From_Symbol : String)
      return String
   is
      Last_Dot : constant Natural :=
                   Ada.Strings.Fixed.Index
                     (From_Symbol, ".", From_Symbol'Last,
                      Ada.Strings.Backward);
   begin
      if Last_Dot = 0 then
         return From_Symbol;
      else
         return From_Symbol (Last_Dot + 1 .. From_Symbol'Last);
      end if;
   end Get_Local_Name;

   ---------------------
   -- Get_Module_Name --
   ---------------------

   function Get_Module_Name
     (From_Symbol : String)
      return String
   is
      Last_Dot : constant Natural :=
                   Ada.Strings.Fixed.Index
                     (From_Symbol, ".", From_Symbol'Last,
                      Ada.Strings.Backward);
   begin
      if Last_Dot = 0 then
         return "";
      else
         return From_Symbol (From_Symbol'First .. Last_Dot - 1);
      end if;
   end Get_Module_Name;

   ------------------
   -- To_File_Name --
   ------------------

   function To_File_Name
     (Module_Name : String)
      return String
   is
      Name : String :=
               Ada.Characters.Handling.To_Lower
                 (Module_Name);
   begin
      for Ch of Name loop
         if Ch = '.' then
            Ch := '-';
         end if;
      end loop;
      return Name & ".o";
   end To_File_Name;

end Aqua.Names;
