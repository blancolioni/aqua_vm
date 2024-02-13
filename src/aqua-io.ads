private with Ada.Sequential_IO;

package Aqua.IO is

   Invalid_Header  : exception;
   Invalid_Version : exception;

   procedure Set_IO_Path
     (Path : String);
   --  Aqua will only read or write files contained in the directory
   --  specified by Path

   function Current_IO_Path return String;

   type File_Type is limited private;

   procedure Create (File : in out File_Type;
                     Name : String);

   procedure Open (File : in out File_Type;
                   Name : String);

   procedure Close (File : in out File_Type);

   procedure Write_Word_8
     (File  : File_Type;
      Value : Word_8);

   procedure Read_Word_8
     (File  : File_Type;
      Value : out Word_8);

   procedure Write_Word
     (File  : File_Type;
      Value : Word);

   procedure Read_Word
     (File  : File_Type;
      Value : out Word);

   procedure Write_Address
     (File  : File_Type;
      Value : Address_Type);

   procedure Read_Address
     (File  : File_Type;
      Value : out Address_Type);

   procedure Write_String_Literal
     (File  : File_Type;
      Value : String);

   function Read_String_Literal
     (File : File_Type)
      return String;

private

   package Word_8_IO is
     new Ada.Sequential_IO (Word_8);

   type File_Type is limited
      record
         F                     : Word_8_IO.File_Type;
         Major, Minor, Release : Word_8;
      end record;

end Aqua.IO;
