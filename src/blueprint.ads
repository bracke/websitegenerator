with Ada.Directories; use Ada.Directories;

package Blueprint is

   type Action is (Write, Delete, DryRun);

   function Get_Blueprint_Folder return String;

   procedure Process_File
      (Target : String; Search_Item : Directory_Entry_Type; Todo : Action);

   procedure Parse_Content (Source_File : String; Target : String);

   procedure Iterate (Blueprint_Folder : String; Path : String; Todo : Action);

private

end Blueprint;