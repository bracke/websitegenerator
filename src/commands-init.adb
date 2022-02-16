with Ada.Text_IO;
with Ada.Directories;
with Init_Project;
with Blueprint; use Blueprint;

package body Commands.Init is

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

    Init_Project.Init(Ada.Directories.Current_Directory,ToDo);
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

end Commands.Init;