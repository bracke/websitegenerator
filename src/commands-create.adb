with Ada.Text_IO;
with Ada.Directories;
with Ada.Containers.Vectors;
with Init_Project;
with Blueprint; use Blueprint;
with Ada.Command_Line;
with Templates_Parser.Utils;

package body Commands.Create is

  package IO renames Ada.Text_IO;
  package TT renames CLIC.TTY;
  package Dir renames Ada.Directories;

  use Ada.Directories;
  use Ada.Containers;

  -------------
  -- Execute --
  -------------

  overriding
  procedure Execute ( Cmd  : in out Instance;
                      Args :        AAA.Strings.Vector)
  is
  begin
    if Args.Length > 0 then
      declare
        Path : String := Args.First_Element;
        ToDo : Action := Write;
      begin
        if Cmd.Dry_Run then
          IO.Put_Line(TT.Emph("You specified the dry-run flag, so no changes will be written."));
          ToDo := DryRun;
        end if;
        if not Dir.Exists(Path) then
          if ToDo = Write then
            Dir.Create_Directory(Path);
          end if;
        end if;

        if Dir.Kind(Path) = Ada.Directories.Directory then
          Init_Project.Init(Path,ToDo);
        else
          IO.Put_Line(TT.Error(Path & " exists and is not a directory."));
        end if;
      end;
    else
      IO.Put_Line(TT.Error("Command requires a project name to be specified."));
    end if;

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
      (Config, Cmd.Dry_Run'Access, "", "-dry-run", "Dry-run");

   end Setup_Switches;

end Commands.Create;