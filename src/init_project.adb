with Ada.Text_IO;
with Ada.Directories;
with Ada.Command_Line;
with Templates_Parser;
with CLIC.TTY;
with Filesystem;
with Commands;
with GNAT.Strings;
with Blueprint; use Blueprint;
with Ada.Characters.Conversions;
with Generator;
with Globals;

package body Init_Project is

  package IO renames Ada.Text_IO;
  package TT renames CLIC.TTY;

   use Ada.Characters.Conversions;
   use Ada.Directories;
   use GNAT.Strings;
   use Generator.aString;

  Errors : Boolean := false;
  Filter : constant Filter_Type :=
   (Ordinary_File => True, Special_File => False, Directory => True);

  procedure Init (Path : String; Blueprint: string; ToDo: Action) is
    Blueprint_Folder      : String := Get_Blueprint_Folder;
    App_Blueprint_Folder  : String := Compose (Blueprint_Folder, "site");
    Blueprint_Path        : XString;
    Name                  : String := Simple_Name (Path);
  begin
      if Blueprint /= "" then
         Blueprint_Path := To_XString(To_Wide_Wide_String(Compose (App_Blueprint_Folder, Blueprint)));
      else
         Blueprint_Path := To_XString(To_Wide_Wide_String(Compose (App_Blueprint_Folder, Globals.Blueprint_Default)));
      end if;
    Templates_Parser.Insert
     (Commands.Translations, Templates_Parser.Assoc ("SITENAME", Name));

    if Exists (To_String(To_String(Blueprint_Path))) then
      IO.Put_Line
       (TT.Italic ("Creating a new project") & " " & TT.Bold (Path) & ":");

      Iterate (To_String(To_String(Blueprint_Path)), Path, ToDo);

      IO.New_Line;
      if Errors then
        IO.Put_Line
         (TT.Warn ("Created site") & " " & TT.Bold (Name) & " " & "with errors.");
      else
        IO.Put_Line (TT.Success( "Successfully created site") & " " & TT.Warn (TT.Bold (Name)));
      end if;

      IO.New_Line;
      IO.Put_Line
       (TT.Info
         (TT.Description ("Build your site using") & " " &
          TT.Terminal ("WebsiteGenerator publish")));

      IO.Put_Line
       (TT.Info
         (TT.Description ("Add components and other items using") & " " &
          TT.Terminal ("WebsiteGenerator generate")));
    else
      IO.Put_Line (TT.Error("Blueprint not found: " & To_String(To_String(Blueprint_Path))));
    end if;
  -- TODO: Move text out into xml file

  end Init;

end Init_Project;