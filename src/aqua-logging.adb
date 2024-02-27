with Ada.Text_IO;

package body Aqua.Logging is

   Logging_Enabled : Boolean := False;

   File : Ada.Text_IO.File_Type;

   ---------
   -- Log --
   ---------

   procedure Log (Message : String) is
   begin
      if Logging_Enabled then
         Ada.Text_IO.Put_Line (File, Message);
      end if;
   end Log;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Logging_Enabled := True;
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, "trace.txt");
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Logging_Enabled := False;
      Ada.Text_IO.Close (File);
   end Stop;

end Aqua.Logging;
