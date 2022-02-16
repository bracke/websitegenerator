with Ada.Text_IO;
with Ada.Directories;
with Ada.Command_Line;
with Templates_Parser;
with CLIC.TTY;
with Filesystem;
with Commands;
with Blueprint; use Blueprint;

package body Init_Project is

  package IO renames Ada.Text_IO;
  package TT renames CLIC.TTY;

  use Ada.Directories;

  Errors : Boolean := false;
  Filter : constant Filter_Type :=
   (Ordinary_File => True, Special_File => False, Directory => True);

  procedure Init (Path : String; ToDo: Action) is
    Blueprint_Folder      : String := Get_Blueprint_Folder;
    App_Blueprint_Folder  : String := Compose (Blueprint_Folder, "app");
    Blueprint             : String := "standard";
    Blueprint_Path        : String := Compose (App_Blueprint_Folder, Blueprint);
    Name                  : String := Simple_Name (Path);
  begin
    Templates_Parser.Insert
     (Commands.Translations, Templates_Parser.Assoc ("APPNAME", Name));

    if Exists (Blueprint_Path) then
      IO.Put_Line
       (TT.Italic ("Creating a new project") & " " & TT.Bold (Path) & ":");

      Iterate (Blueprint_Path, Path, ToDo);

      IO.New_Line;
      if Errors then
        IO.Put_Line
         (TT.Warn ("Created project") & " " & TT.Bold (Name) & " " & "with errors.");
      else
        IO.Put_Line (TT.Success( "Successfully created project") & " " & TT.Warn (TT.Bold (Name)));
      end if;

      IO.New_Line;
      IO.Put_Line
       (TT.Info
         (TT.Description ("Build your project using") & " " &
          TT.Terminal ("/scripts/build.sh")));

      IO.Put_Line
       (TT.Info
         (TT.Description ("Add components and other items using") & " " &
          TT.Terminal ("WebsiteGenerator generate")));
    else
      IO.Put_Line (TT.Error("Blueprint not found: " & Blueprint_Path));
    end if;
  -- TODO: Move text out into xml file

  end Init;

end Init_Project;