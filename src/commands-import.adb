with Ada.Containers;  use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Blueprint;       use Blueprint;
with AAA.Strings;     use AAA.Strings;
with Ada.Command_Line;
with Templates_Parser;
with Filesystem;
with Commands;

package body Commands.Import is

   -------------
   -- Execute --
   -------------
   overriding
   procedure Execute (Cmd  : in out Instance;
                      Args :        AAA.Strings.Vector) is
   begin
      null;
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
      null;
   end Setup_Switches;

end Commands.Import;