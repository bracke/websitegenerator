with Version;
with CLIC.Subcommand;
private with CLIC.Subcommand.Instance;
private with Ada.Text_IO;
private with CLIC.TTY;
private with GNAT.OS_Lib;
with Templates_Parser;

package Commands is

   Translations : Templates_Parser.Translate_Set;
   Wrong_Command_Arguments : exception;
   Child_Failed : exception;
   --  Used to notify that a subprocess completed with non-zero error

   Command_Failed : exception;
   --  Signals "normal" command completion with failure
   --  (i.e., no need to print stack trace).

   -------------
   -- Execute --
   -------------

   procedure Execute;
   --  Entry point into WebsiteGenerator,
   --  will parse the command line and proceed as needed.

   -------------
   -- Command --
   -------------

   type Command
   is abstract limited new CLIC.Subcommand.Command
   with private;
   --  This type encapsulates configuration and execution of a specific
   --  command.

private

   type Command is abstract limited new CLIC.Subcommand.Command
   with null record;

   procedure Set_Global_Switches
      (Config : in out CLIC.Subcommand.Switches_Configuration);

   package Sub_Cmd is new CLIC.Subcommand.Instance
         (Main_Command_Name  => "websitegenerator",
         Version             => Version.Current,
         Put                 => Ada.Text_IO.Put,
         Put_Line            => Ada.Text_IO.Put_Line,
         Put_Error           => Ada.Text_IO.Put_Line,
         Error_Exit          => GNAT.OS_Lib.OS_Exit,
         Set_Global_Switches => Set_Global_Switches,
         TTY_Chapter         => CLIC.TTY.Bold,
         TTY_Description     => CLIC.TTY.Description,
         TTY_Version         => CLIC.TTY.Version,
         TTY_Underline       => CLIC.TTY.Underline,
         TTY_Emph            => CLIC.TTY.Emph);

end Commands;