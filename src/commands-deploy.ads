with AAA.Strings;
with CLIC.TTY;
with Ada.Text_IO;
private with GNAT.Strings;

package Commands.Deploy is

package IO renames Ada.Text_IO;
  package TT renames CLIC.TTY;

   type Instance
   is new CLIC.Subcommand.Command
   with private;

  overriding function Name (Cmd : Instance) return CLIC.Subcommand.Identifier is
   ("deploy");

  overriding procedure Execute
   (Cmd : in out Instance; Args : AAA.Strings.Vector);

   overriding
   function Switch_Parsing (This : Instance)
                            return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.All_As_Args);

  overriding function Long_Description
   (Cmd : Instance) return AAA.Strings.Vector is
   (AAA.Strings.Empty_Vector.Append
     ("Deploys files.")
     .New_Line
     );

  overriding procedure Setup_Switches
   (Cmd    : in out Instance;
    Config : in out CLIC.Subcommand.Switches_Configuration);

  overriding function Short_Description (Cmd : Instance) return String is
     ("Deploys files.");

  overriding function Usage_Custom_Parameters (Cmd : Instance) return String is
   ("");

private

  type Instance is new CLIC.Subcommand.Command with null record;

end Commands.Deploy;