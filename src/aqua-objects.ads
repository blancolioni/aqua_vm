private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Vectors;
private with Ada.Strings.Fixed.Less_Case_Insensitive;
private with Ada.Strings.Unbounded;

private with WL.String_Maps;

with Aqua.Memory;

package Aqua.Images is

   subtype Parent is Aqua.Memory.Instance;
   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   procedure Load
     (This : in out Instance'Class;
      Name : in     String);

   procedure Link
     (This : in out Instance'Class);

   procedure Bind
     (This   : Instance'Class;
      Binder : not null access
        procedure (Group_Name  : String;
                   Before      : Boolean;
                   Parent_Name : String;
                   Child_Name  : String;
                   Start       : Address_Type));

   procedure Save
     (This : Instance'Class;
      Path : String);

   function Start_Address
     (This : Instance'Class)
      return Address_Type;

   function Show (This : Instance'Class;
                  Value : Word)
                  return String;

   function Show_Source_Position
     (This : Instance'Class;
      Address : Address_Type)
      return String;

   function Show_Known_Source_Position
     (Image : Instance'Class;
      Address  : Address_Type)
      return String;

   function Get_Handler_Address
     (This        : Instance'Class;
      Trap_Address : Address_Type)
      return Address_Type;

   function Segment_Base
     (This : Instance'Class;
      Name  : String)
      return Address_Type;

   function Segment_Bound
     (This : Instance'Class;
      Name  : String)
      return Address_Type;

   function Segment_Size
     (This : Instance'Class;
      Name  : String)
      return Word;

   function Code_Base
     (This : Instance'Class)
      return Address_Type;

   function Code_Bound
     (This : Instance'Class)
      return Address_Type;

   function Code_Size
     (This : Instance'Class)
      return Word;

   function New_Image return Reference;

private

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   type Reference_Info is
      record
         Address     : Address_Type;
         Relative : Boolean;
         Branch   : Boolean;
      end record;

   package List_Of_References is
     new Ada.Containers.Doubly_Linked_Lists (Reference_Info);

   type Link_Info is
      record
         Value          : Word;
         References     : List_Of_References.List;
         Defn_File      : Ada.Strings.Unbounded.Unbounded_String;
         Has_Value      : Boolean := False;
         Start          : Boolean := False;
      end record;

   package Link_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Natural, String);

   package Link_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type        => String,
        Element_Type    => Link_Info,
        "<"             => Ada.Strings.Fixed.Less_Case_Insensitive);

   type Binding_Info is
      record
         Group       : Ada.Strings.Unbounded.Unbounded_String;
         Start       : Address_Type;
         Before      : Boolean;
         Parent_Text : Ada.Strings.Unbounded.Unbounded_String;
         Child_Text  : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Binding_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Binding_Info);

   type Exception_Info is
      record
         Base_Label      : Ada.Strings.Unbounded.Unbounded_String;
         Bound_Label     : Ada.Strings.Unbounded.Unbounded_String;
         Handler_Label   : Ada.Strings.Unbounded.Unbounded_String;
         Base_Address    : Address_Type;
         Bound_Address   : Address_Type;
         Handler_Address : Address_Type;
      end record;

   package Exception_Info_Vectors is
     new Ada.Containers.Vectors (Positive, Exception_Info);

   type Source_Location is
      record
         Source_File : Ada.Strings.Unbounded.Unbounded_String;
         Start       : Address_Type;
         Line        : Natural;
         Column      : Natural;
      end record;

   package List_Of_Source_Locations is
     new Ada.Containers.Doubly_Linked_Lists (Source_Location);

   type Segment_Record is
      record
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         R, W, X     : Boolean := False;
         Initialised : Boolean := False;
         Base, Bound : Address_Type;
      end record;

   package Segment_Maps is
     new WL.String_Maps (Segment_Record);

   type Instance is new Parent with
      record
         Locations     : List_Of_Source_Locations.List;
         Bindings      : Binding_Info_Vectors.Vector;
         Handlers      : Exception_Info_Vectors.Vector;
         String_Vector : Link_Vectors.Vector;
         Label_Vector  : Link_Vectors.Vector;
         Link_Map      : Link_Maps.Map;
         Segment_Map   : Segment_Maps.Map;
         Start         : Address_Type := 16#1000#;
      end record;

   function Start_Address
     (This : Instance'Class)
      return Address_Type
   is (This.Start);

   function Code_Base
     (This : Instance'Class)
      return Address_Type
   is (This.Segment_Map.Element ("code").Base);

   function Code_Bound
     (This : Instance'Class)
      return Address_Type
   is (This.Segment_Map.Element ("code").Bound);

   function Code_Size
     (This : Instance'Class)
      return Word
   is (This.Segment_Size ("code"));

end Aqua.Images;
