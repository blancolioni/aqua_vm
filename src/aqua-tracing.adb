with Ada.Containers.Indefinite_Vectors;
with WL.String_Maps;

with Aqua.Images;
with Aqua.Logging;

package body Aqua.Tracing is

   package Module_Name_Vectors is
     new Ada.Containers.Indefinite_Vectors (Module_Id, String);

   package Module_Id_Maps is
     new WL.String_Maps (Module_Id);

   Module_Name_Vector : Module_Name_Vectors.Vector;
   Module_Id_Map      : Module_Id_Maps.Map;

   -----------
   -- Fetch --
   -----------

   procedure Fetch
     (This  : in out Element;
      Dst_R : Word_8;
      Src   : Address_Type;
      Value : Word_32)
   is
   begin
      This.Has_Fetch := True;
      This.Update_R  := Dst_R;
      This.Fetch_Addr := Src;
      This.Fetch_Value := Value;
   end Fetch;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This                 : in out Element;
      Module               : String;
      Virtual_Address      : Address_Type;
      Module_Local_Address : Address_Type;
      IR                   : Word_32)
   is
   begin
      This := (others => <>);
      This.V_Addr := Virtual_Address;
      This.M_Addr := Module_Local_Address;
      This.IR := IR;

      if not Module_Id_Map.Contains (Module) then
         Module_Name_Vector.Append (Module);
         Module_Id_Map.Insert (Module, Module_Name_Vector.Last_Index);
      end if;

      This.Module := Module_Id_Map.Element (Module);

   end Initialize;

   -----------
   -- Store --
   -----------

   procedure Store
     (This  : in out Element;
      Src_R : Word_8;
      Dst   : Address_Type;
      Value : Word_32)
   is
   begin
      This.Has_Store := True;
      This.Update_R := Src_R;
      This.Store_Addr := Dst;
      This.Store_Value := Value;
   end Store;

   ------------
   -- Update --
   ------------

   procedure Update
     (This   : in out Element;
      R      : Word_8;
      Value  : Word_32)
   is
   begin
      This.Has_Update := True;
      This.Update_R := R;
      This.R_Value  := Value;
   end Update;

   ----------
   -- Save --
   ----------

   procedure Save
     (This : Element)
   is
      use Aqua.Images;

      function R_Image (R : Word_8) return String;

      -------------
      -- R_Image --
      -------------

      function R_Image (R : Word_8) return String is
         Img : String := R'Image;
      begin
         Img (Img'First) := '%';
         return (if R < 10 then "  " & Img
                 elsif R < 100 then " " & Img
                 else Img);
      end R_Image;

   begin
      Aqua.Logging.Log
        (Hex_Image (This.V_Addr)
         & " "
         & Hex_Image (This.M_Addr)
         & ": "
         & Hex_Image (This.IR)
         & "    "
         & (if This.Has_Update
           then R_Image (This.Update_R)
           & " <- " & Hex_Image (This.R_Value)
           else "                ")
         & "    "
         & Module_Name_Vector (This.Module));
   end Save;

begin
   Module_Name_Vector.Append ("");
   Module_Id_Map.Insert ("", 0);
end Aqua.Tracing;
