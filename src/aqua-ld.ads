package Aqua.Ld is

   Undefined_Reference : exception;

   type Link_Manager is interface;

   procedure Resolve
     (This    : not null access Link_Manager;
      Address : Address_Type;
      Op      : Word_8;
      R       : Word_8)
   is abstract;

end Aqua.Ld;
