with Ada.Text_IO;

package body Aqua.Logging is

   Logging_Enabled : Boolean := False;

   ---------
   -- Log --
   ---------

   procedure Log (Message : String) is
   begin
      if Logging_Enabled then
         Ada.Text_IO.Put_Line (Message);
      end if;
   end Log;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Logging_Enabled := True;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Logging_Enabled := False;
   end Stop;

end Aqua.Logging;
