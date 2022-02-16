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
with GNAT.Strings;
with Generator;
with Ada.Characters.Conversions;

package body Commands.Publish is

 use Ada.Characters.Conversions;
   use GNAT.Strings;
   use Ada.Containers;
  use Generator.aString;

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
    Source : XString := To_XString(To_Wide_Wide_String(Ada.Directories.Current_Directory));
  begin
   if Args.Length > 0 then
      if Cmd.Source.all /= "" then
        Source := To_XString(To_Wide_Wide_String(Cmd.Source.all));
      end if;

      declare
         Website_Source : String := Ada.Directories.Full_Name(To_String(To_String(Source)));
         Website_Target : String :=
         Ada.Directories.Full_Name (Ada.Command_Line.Argument (1));
      begin
         if Ada.Directories.Exists(Website_Source) then
            Generator.Start (Website_Source, Website_Target);

            Ada.Directories.Set_Directory (Directory => Website_Target);
            Server.Start_Server;
         else
            IO.Put_Line(TT.Error("Source folder (" & Website_Source & ") does not exist."));
         end if;
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
        (Config,
         Cmd.Source'Access,
         "", "--source=",
         "Selects a source folder other than the current directory");

   end Setup_Switches;

end Commands.Publish;