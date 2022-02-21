with AAA.Strings;
private with GNAT.Strings;

package Commands.Create is

  type Instance
   is new CLIC.Subcommand.Command
   with private;

  overriding function Name (Cmd : Instance) return CLIC.Subcommand.Identifier is
   ("new");

  overriding procedure Execute
   (Cmd : in out Instance; Args : AAA.Strings.Vector);

   overriding
   function Switch_Parsing (This : Instance)
                            return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.All_As_Args);

  overriding function Long_Description
   (Cmd : Instance) return AAA.Strings.Vector is
   (AAA.Strings.Empty_Vector.Append
     ("Creates a new folder and runs websitegenerator init in it.")
     .New_Line
     );

  overriding procedure Setup_Switches
   (Cmd    : in out Instance;
    Config : in out CLIC.Subcommand.Switches_Configuration);

  overriding function Short_Description (Cmd : Instance) return String is
   ("Creates a new folder and runs websitegenerator init in it.");

  overriding function Usage_Custom_Parameters (Cmd : Instance) return String is
   ("<targetpath> [-dry-run]");

private

   type Instance is new CLIC.Subcommand.Command with record
    Dry_Run : aliased Boolean := False;
  end record;

end Commands.Create;