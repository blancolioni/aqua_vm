with Ada.Characters.Handling;
with Ada.Strings.Fixed;

function Aqua.Commands.Parse
  (Line : String)
   return Command_Line
is
   use Ada.Strings, Ada.Strings.Fixed;
   use Ada.Characters.Handling;
   First  : Boolean := True;
   Start  : Natural := Index_Non_Blank (Line);
   Result : Command_Line;
begin
   while Start > 0 loop
      pragma Assert (not Is_Space (Line (Start)));
      declare
         Next  : constant Natural := Index (Line, " ", Start + 1);
         Value : constant String :=
                   (if Next > 0
                    then Line (Start .. Next - 1)
                    else Line (Start .. Line'Last));
      begin
         pragma Assert (Value /= "");
         if First then
            First := False;
            Result.Command :=
              Device_Commands.To_Bounded_String (Value);
         else
            Result.Arguments.Append (Value);
         end if;
         if Next > 0 then
            Start := Index_Non_Blank (Line, Next);
         else
            Start := 0;
         end if;
      end;
   end loop;
   return Result;
end Aqua.Commands.Parse;
