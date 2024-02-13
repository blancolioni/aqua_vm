with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Aqua.Commands is

   --------------
   -- Argument --
   --------------

   function Argument (This : Command_Line'Class; Name : String) return String
   is
   begin
      for Argument of This.Arguments loop
         declare
            Equal : constant Natural :=
                      Ada.Strings.Fixed.Index (Argument, "=");
         begin
            if Equal > 0
              and then Argument (Argument'First .. Equal - 1) = Name
            then
               return Argument (Equal + 1 .. Argument'Last);
            end if;
         end;
      end loop;
      return "";
   end Argument;

   ----------------
   -- To_Word_32 --
   ----------------

   function To_Word_32 (Value : String) return Word_32 is
   begin
      return Word_32'Value ("16#" & Value & "#");
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "invalid argument: [" & Value & "]");
         raise;
   end To_Word_32;

end Aqua.Commands;
