package body Aqua.Addressable is

   ----------------
   -- Get_Word_8 --
   ----------------

   procedure Get_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_8)
   is
      W : Word_32;
      Shift : constant Word_32 := 2 ** Natural (Address mod 4) * 8;
   begin
      Dispatch (This).Get_Word_32 (Address and Word_32_Address_Mask, W);
      Value := Word_8 (W / Shift mod 256);
   end Get_Word_8;

   -----------------
   -- Get_Word_16 --
   -----------------

   procedure Get_Word_16
     (This : in out Instance; Address : Address_Type; Value : out Word_16)
   is
      W : Word_32;
      Shift : constant Word_32 :=
                2 ** Natural ((Address and Word_16_Address_Mask) mod 4)
                * 16;
   begin
      Dispatch (This).Get_Word_32 (Address and Word_32_Address_Mask, W);
      Value := Word_16 (W / Shift mod 65536);
   end Get_Word_16;

   -----------------
   -- Get_Word_32 --
   -----------------

   procedure Get_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : out Word_32)
   is
      A : constant Address_Type := Address and Word_32_Address_Mask;
      B : Word_8;
   begin
      Value := 0;
      for I in Address_Type range 0 .. 3 loop
         Dispatch (This).Get_Word_8 (A + I, B);
         Value := Value * 256 + Word_32 (B);
      end loop;
   end Get_Word_32;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This        : in out Instance'Class;
      Base, Bound : Address_Type)
   is
   begin
      This.Base := Base;
      This.Bound := Bound;
   end Initialize;

   ----------------
   -- Set_Word_8 --
   ----------------

   procedure Set_Word_8
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_8)
   is
      W : Word_32;
      A     : constant Address_Type := Address and Word_32_Address_Mask;
      Shift : constant Word_32 := 2 ** Natural (Address mod 4) * 8;
      Mask  : constant Word_32 := 16#FF# * Shift;
   begin
      Dispatch (This).Get_Word_32 (A, W);
      W := (W and not Mask) or Word_32 (Value) * Shift;
      Dispatch (This).Set_Word_32 (A, W);
   end Set_Word_8;

   -----------------
   -- Set_Word_16 --
   -----------------

   procedure Set_Word_16
     (This : in out Instance; Address : Address_Type; Value : Word_16)
   is
      W     : Word_32;
      Shift : constant Word_32 :=
                2 ** Natural ((Address and Word_16_Address_Mask) mod 4)
                * 16;
      Mask  : constant Word_32 := 16#FFFF# * Shift;
   begin
      Dispatch (This).Get_Word_32 (Address and Word_32_Address_Mask, W);
      W := (W and not Mask) or Word_32 (Value) * Shift;
      Dispatch (This).Set_Word_32 (Address and Word_32_Address_Mask, W);
   end Set_Word_16;

   -----------------
   -- Set_Word_32 --
   -----------------

   procedure Set_Word_32
     (This    : in out Instance;
      Address : Address_Type;
      Value   : Word_32)
   is
      W : Word_32 := Value;
   begin
      for I in reverse Address_Type range 0 .. 3 loop
         Dispatch (This).Set_Word_8 (Address + I, Word_8 (W mod 256));
         W := W / 256;
      end loop;
   end Set_Word_32;

end Aqua.Addressable;
