package Aqua.Addressable is

   type Instance is abstract tagged limited private;
   type Reference is access all Instance'Class;

   function Base (This : Instance'Class) return Word_32;
   function Bound (This : Instance'Class) return Word_32;

   procedure Tick (This : in out Instance) is null;

   procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8);

   procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8);

   procedure Get_Word_16
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_16);

   procedure Set_Word_16
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_16);

   procedure Get_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_32);

   procedure Set_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_32);

   procedure Initialize
     (This        : in out Instance'Class;
      Base, Bound : Address_Type);

private

   subtype Dispatch is Instance'Class;
   type Instance is abstract tagged limited
      record
         Base, Bound : Address_Type;
      end record;

   function Base (This : Instance'Class) return Word_32 is (This.Base);
   function Bound (This : Instance'Class) return Word_32 is (This.Bound);

end Aqua.Addressable;
