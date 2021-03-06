with Blueprint; use Blueprint;
with Server;
with Templates_Parser;
with Ada.Text_IO;
with Generator;
with Ada.Containers;
with Ada.Calendar;
with Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Text_IO.Text_Streams;
with Ada.Command_Line;
with Ada.Exceptions;
with CLIC.Subcommand;
with Commands;
with CLIC.TTY;
with Ada.Characters.Conversions;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Filesystem;
with Ada.Strings.Fixed;
with Ada.Directories.Hierarchical_File_Names;

package body Commands.Publish is

   use Ada.Characters.Conversions;
   use GNAT.Strings;
   use Ada.Containers;
   use Generator.aString;
   use Ada.Strings.Unbounded;
   use Ada.Directories;
   use Ada.Directories.Hierarchical_File_Names;

   package IO renames Ada.Text_IO;
   package TT renames CLIC.TTY;
   package DIR renames Ada.Directories;

  -------------
  -- Execute --
  -------------
   overriding
   procedure Execute (Cmd  : in out Instance;
                      Args :        AAA.Strings.Vector)
   is
      ToDo : Action := Write;
      Source : XString :=
         To_XString (To_Wide_Wide_String (DIR.Current_Directory));

      Default_Target : constant String :=
         DIR.Compose (
         DIR.Current_Directory,
         Globals.Dist_Folder_Name);

      Target : XString :=
         To_XString (To_Wide_Wide_String (Default_Target));
   begin
      if Args.Length > 0 then
         Target := To_XString (
               To_Wide_Wide_String (DIR.Full_Name (Args (1))));
      end if;

      if Cmd.Source.all /= "" then
         Source := To_XString (
            To_Wide_Wide_String (Cmd.Source.all));
      end if;

      declare
         Website_Source : constant String :=
            DIR.Full_Name (To_String (To_String (Source)));
         Source_Layout_Folder : constant String :=
            DIR.Compose (Website_Source, Globals.Layout_Folder_Name);
         Website_Target : constant String :=
            DIR.Full_Name (To_String (To_String (Target)));
      begin
         if DIR.Kind (Website_Source) = Directory then
            if DIR.Exists (Source_Layout_Folder) then
               --  Delete result of last run
               if Cmd.Delete_Target_Content and then
                  DIR.Exists (Website_Target)
               then
                  if Is_Root_Directory_Name (Website_Target) then
                     IO.Put_Line (
                        TT.Error ("Ignored 'delete " & "target' switch (-d)" &
                     " since the target directory appears " &
                     "to be a root directory."));
                  else
                     if Exists (
                        DIR.Compose (Website_Target, "index", "html"))
                     then
                        DIR.Delete_Tree (Website_Target);
                     else
                        IO.Put_Line (
                           TT.Error ("Ignored 'delete target' switch (-d) " &
                        " since the target directory does not " &
                        "appear to be a website."));
                     end if;
                  end if;
               end if;

               if not DIR.Exists (Website_Target) then
                  DIR.Create_Directory (Website_Target);
               end if;

               if DIR.Kind (Website_Target) = Directory then
                  if DIR.Exists (Website_Source) then
                     if not Filesystem.Is_Subfolder
                        (Website_Source, Website_Target) or else
                     Ada.Strings.Fixed.Head (
                        DIR.Base_Name (Website_Target), 1) = "_"
                     then

                        Generator.Start (Website_Source, Website_Target);
                        DIR.Set_Directory (Directory => Website_Target);
                        Server.Start_Server;
                     else
                        IO.Put_Line (
                           TT.Error ("Publish target may not be " &
                            "inside the source folder or must be" &
                            " prefixed with and underscore."));
                     end if;
                  else
                     IO.Put_Line (TT.Error ("Source folder (" &
                     Website_Source & ") does not exist."));
                  end if;
               else
                  IO.Put_Line (
                     TT.Error ("Target (" & Website_Target &
                     ") is not a folder."));
               end if;
            else
               IO.Put_Line (TT.Error ("Source folder (" &
               Website_Source &
               ") is not a website. Please create a website using the " &
               TT.Emph ("wg new <name>") & " command."));
            end if;
         else
            IO.Put_Line (
               TT.Error ("Source (" &
               Website_Source & ") is not a folder."));
         end if;
      end;
   end Execute;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Cmd    : in out Instance;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      Define_Switch
        (Config      => Config,
         Output      => Cmd.Source'Access,
         Argument    => "PATH",
         Long_Switch => "--source=",
         Help        => "Selects a source folder other" &
         " than the current directory");

      Define_Switch
      (Config, Cmd.Delete_Target_Content'Access,
         "", "-d", "Delete target folder before publish");

   end Setup_Switches;

end Commands.Publish;