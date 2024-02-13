private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Doubly_Linked_Lists;
private with WL.String_Maps;

with System.Storage_Elements;

with Aqua.Bus;
with Aqua.Ld;
with Aqua.MM;

package Aqua.OS is

   Out_Of_Memory : exception;

   type Instance is
     new Aqua.MM.Memory_Manager
     and Aqua.Ld.Link_Manager
   with private;

   type Reference is access all Instance'Class;

   function Create
     (Bus : not null access Aqua.Bus.Instance'Class)
      return Reference;

   type Memory_Segment is (Code, Text, Data, Heap, Stack);

   type Loader_Type is private;

   function Load
     (This     : not null access Instance'Class;
      Data     : System.Storage_Elements.Storage_Array;
      Segment  : Memory_Segment)
     return Loader_Type;

   function Base
     (This : Loader_Type)
      return Address_Type;

   procedure Define_Symbol
     (Loader : Loader_Type;
      Module : String;
      Name   : String;
      Offset : Word_32);

   procedure External_Reference
     (This   : in out Instance'Class;
      Module : String;
      Name   : String);

   procedure Relocate
     (Loader     : Loader_Type;
      Sym_Loader : Loader_Type;
      Name       : String;
      Offset     : Word_32;
      Value      : Word_32;
      Context    : Relocation_Context;
      Defined    : Boolean);

   procedure Resolve_Pending_References
     (This : not null access Instance'Class);

   procedure Trace_Loading (Enabled : Boolean);

private

   Page_Size : constant := 16#1000#;

   type Virtual_Page_Address is new Address_Type range 0 .. 2 ** 20 - 1;
   type Physical_Page_Address is new Address_Type range 0 .. 2 ** 20 - 1;

   function To_Physical_Page
     (Address : Address_Type)
      return Physical_Page_Address
   is (Physical_Page_Address (Address / Page_Size));

   function To_Virtual_Page
     (Address : Address_Type)
      return Virtual_Page_Address
   is (Virtual_Page_Address (Address / Page_Size));

   type Map_Entry is
      record
         Virtual_Page  : Virtual_Page_Address;
         Physical_Page : Physical_Page_Address;
      end record;

   package Map_Entry_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Map_Entry);

   type Memory_Page_Type is (Available, Device);

   type Page_Range_Record is
      record
         Page_Type   : Memory_Page_Type;
         Base, Bound : Physical_Page_Address;
      end record;

   package Page_Range_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Page_Range_Record);

   type Symbol_Reference (Length : Natural) is
      record
         Name         : String (1 .. Length);
         Defined      : Boolean;
         Virtual_Addr : Address_Type;
      end record;

   package Symbol_Reference_Maps is
     new WL.String_Maps (Symbol_Reference);

   type Undefined_Reference (Length : Natural) is
      record
         Name    : String (1 .. Length);
         Offset  : Address_Type;
         Context : Relocation_Context;
      end record;

   package Undefined_Reference_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Address_Type, Undefined_Reference, "<");

   type Segment_Address_Array is array (Memory_Segment) of Address_Type;

   type Module_Reference (Length : Natural) is
      record
         Name      : String (1 .. Length);
         Phys_Base : Segment_Address_Array := [others => 0];
         Virt_Base : Segment_Address_Array := [others => 0];
      end record;

   package Module_Maps is
      new WL.String_Maps (Module_Reference);

   Base_Address : constant Segment_Address_Array :=
                    [Code  => 16#10_0000#,
                     Text  => 16#20_0000#,
                     Data  => 16#30_0000#,
                     Heap  => 16#80_0000#,
                     Stack => 16#40_0000#];

   type Protection_Bits is
      record
         R, W, X : Boolean := False;
      end record;

   type Page_Protection_Record is
      record
         Base, Bound : Virtual_Page_Address;
         Protection  : Protection_Bits;
      end record;

   package Page_Protection_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Page_Protection_Record);


   type Instance is
     new Aqua.MM.Memory_Manager
     and Aqua.Ld.Link_Manager with
      record
         Bounds       : Segment_Address_Array := Base_Address;
         Bus          : Aqua.Bus.Reference;
         Pages        : Page_Range_Lists.List;
         Free_Pages   : Page_Range_Lists.List;
         Memory_Map   : Map_Entry_Lists.List;
         Protection   : Page_Protection_Lists.List;
         Symbols      : Symbol_Reference_Maps.Map;
         Modules      : Module_Maps.Map;
         Refs         : Undefined_Reference_Maps.Map;
         Pending_Refs : Undefined_Reference_Maps.Map;
      end record;

   overriding function Map
     (This    : in out Instance;
      Address : Address_Type;
      Read    : Boolean := False;
      Write   : Boolean := False;
      Execute : Boolean := False)
      return Address_Type;

   overriding function Request_Heap
     (This : in out Instance;
      Size : Word_32)
      return Address_Type;

   overriding procedure Resolve
     (This   : not null access Instance;
      Address : Address_Type;
      Op      : Word_8;
      R       : Word_8);

   function Get_Protection_Bits
     (This    : Instance;
      Page    : Virtual_Page_Address)
      return Protection_Bits;

   type Loader_Type is
      record
         OS      : Reference;
         Base    : Address_Type;
         Bound   : Address_Type;
         Segment : Memory_Segment;
      end record;

   function Base
     (This : Loader_Type)
      return Address_Type
   is (This.Base);

end Aqua.OS;
