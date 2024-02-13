with WL.Command_Line;

package body Aqua.Options is

   pragma Style_Checks (Off);

   function Show_Devices return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("show-devices", ' ');
   end Show_Devices;

   function Trace return Boolean is
   begin
      return WL.Command_Line.Find_Option
               ("trace", ' ');
   end Trace;

end Aqua.Options;
