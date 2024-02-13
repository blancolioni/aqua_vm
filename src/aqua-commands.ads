private with Ada.Strings.Bounded;
private with Ada.Containers.Indefinite_Vectors;

package Aqua.Commands is

   type Command_Line is tagged private;

   function Command (This : Command_Line'Class) return String;
   function Argument_Count (This : Command_Line'Class) return Natural;
   function Argument
     (This  : Command_Line'Class;
      Index : Positive)
      return String
     with Pre => Index <= This.Argument_Count;

   function Argument
     (This  : Command_Line'Class;
      Index : Positive)
      return Word_32
     with Pre => Index <= This.Argument_Count;

   function Argument
     (This  : Command_Line'Class;
      Name  : String)
      return String;

   function Argument
     (This  : Command_Line'Class;
      Name  : String)
      return Word_32;

private

   function To_Word_32 (Value : String) return Word_32;

   package Device_Commands is
     new Ada.Strings.Bounded.Generic_Bounded_Length (32);

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Command_Line is tagged
      record
         Command   : Device_Commands.Bounded_String;
         Arguments : String_Vectors.Vector;
      end record;

   function Command (This : Command_Line'Class) return String
   is (Device_Commands.To_String (This.Command));

   function Argument_Count (This : Command_Line'Class) return Natural
   is (This.Arguments.Last_Index);

   function Argument
     (This  : Command_Line'Class;
      Index : Positive)
      return String
   is (This.Arguments (Index));

   function Argument
     (This  : Command_Line'Class;
      Index : Positive)
      return Word_32
   is (To_Word_32 (This.Argument (Index)));

   function Argument
     (This  : Command_Line'Class;
      Name  : String)
      return Word_32
   is (To_Word_32 (This.Argument (Name)));

end Aqua.Commands;
