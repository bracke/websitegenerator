with Blueprint; use Blueprint;
with Server;
with Templates_Parser;
with Ada.Text_IO;
with Generator;
with Ada.Directories;
with Ada.Containers;
with Ada.Calendar;
with Ada.Wide_Wide_Text_IO, Ada.Wide_Wide_Text_IO.Text_Streams, Ada.Command_Line, Ada.Exceptions, Ada.Calendar;
with Ada.Exceptions; use Ada.Exceptions;
with CLIC.Subcommand;
with Commands;
with CLIC.TTY;

package body Commands.Publish is

   use Ada.Containers;

  package IO renames Ada.Text_IO;
  package TT renames CLIC.TTY;

  -------------
  -- Execute --
  -------------

  overriding
  procedure Execute ( Cmd  : in out Instance;
                      Args :        AAA.Strings.Vector)
  is
    ToDo : Action := Write;
  begin
    if Cmd.Dry_Run then
      IO.Put_Line(TT.Emph("You specified the dry-run flag, so no changes will be written."));
      ToDo := DryRun;
    end if;

   if Args.Length > 0 then

      declare
         Website_Source : String := Ada.Directories.Current_Directory;
         Website_Target : String :=
         Ada.Directories.Full_Name (Ada.Command_Line.Argument (1));
      begin
         Generator.Start (Website_Source, Website_Target);

         Ada.Directories.Set_Directory (Directory => Website_Target);
         Server.Start_Server;
      end;
    else
      IO.Put_Line(TT.Error("Command requires a target folder to be specified."));
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

end Commands.Publish;