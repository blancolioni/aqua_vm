package Aqua.MM is

   Protection_Fault : exception;

   type Memory_Manager is interface;

   function Map (This    : in out Memory_Manager;
                 Address : Address_Type;
                 Read    : Boolean := False;
                 Write   : Boolean := False;
                 Execute : Boolean := False)
                 return Address_Type
                 is abstract;

   function Request_Heap
     (This : in out Memory_Manager;
      Size : Word_32)
      return Address_Type
      is abstract;

end Aqua.MM;
