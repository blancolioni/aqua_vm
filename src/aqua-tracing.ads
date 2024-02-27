package Aqua.Tracing is

   type Element is tagged private;

   procedure Initialize
     (This                 : in out Element;
      Module               : String;
      Virtual_Address      : Address_Type;
      Module_Local_Address : Address_Type;
      IR                   : Word_32);

   procedure Update
     (This   : in out Element;
      R      : Word_8;
      Value  : Word_32);

   procedure Fetch
     (This  : in out Element;
      Dst_R : Word_8;
      Src   : Address_Type;
      Value : Word_32);

   procedure Store
     (This  : in out Element;
      Src_R : Word_8;
      Dst   : Address_Type;
      Value : Word_32);

   procedure Save
     (This : Element);

private

   type Module_Id is new Natural;

   type Element is tagged
      record
         Has_Update  : Boolean      := False;
         Has_Fetch   : Boolean      := False;
         Has_Store   : Boolean      := False;
         Module      : Module_Id    := 0;
         V_Addr      : Address_Type := 0;
         M_Addr      : Address_Type := 0;
         IR          : Word_32      := 0;
         Update_R    : Word_8       := 0;
         R_Value     : Word_32      := 0;
         Fetch_Addr  : Address_Type := 0;
         Fetch_Value : Word_32      := 0;
         Store_Addr  : Address_Type := 0;
         Store_Value : Word_32      := 0;
      end record;

end Aqua.Tracing;
