with GNATCOLL.Mmap;

generic
   Separator_Sequence : in String;
package Linereader is

   use GNATCOLL.Mmap;

   End_Of_Input_Exception : Exception;

   type Reader (Str : Str_Access;
                Len : Natural) is tagged private;

   -- Read a line of text
   function Get_Line (Self : in out Reader) return String;

   -- Has the last call to Get_Line or Get_Ramainder reached the end?
   function End_Of_Input (Self : Reader) return Boolean;

   -- Backup current position
   procedure Backup (Self : in out Reader);

   -- Restore backed up position
   procedure Restore (Self : in out Reader);

   -- Get remainder: position .. last
   function Get_Remainder (Self : in out Reader) return String;

private

   type Reader(
      Str : Str_Access;
      Len : Natural) is tagged
   record
      Source   : Str_Access   := Str;
      Backup   : Natural      := 0;
      Position : Positive      := 1;
      Last     : Natural      := Len;
      End_Of_Input : Boolean  := false;
   end record;

end Linereader;