with AAA.Strings; use AAA.Strings;
private with GNAT.Strings;

package Commands.Generate is

   type Instance
   is new CLIC.Subcommand.Command
   with private;

   function Target_Folder_Name (ItemType : String) return String;

   overriding function Name (Cmd : Instance) return
      CLIC.Subcommand.Identifier is ("generate");

   overriding procedure Execute
   (Cmd : in out Instance; Args : AAA.Strings.Vector);

   overriding
   function Switch_Parsing (This : Instance)
                            return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.All_As_Args);

   overriding function Long_Description
      (Cmd : Instance) return AAA.Strings.Vector;

   overriding procedure Setup_Switches
      (Cmd    : in out Instance;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding function Short_Description (Cmd : Instance) return String is
   ("Generates new files from blueprints.");

   overriding function Usage_Custom_Parameters (Cmd : Instance)
      return String is ("<blueprint> <name> [-dry-run] ");

private

   type Instance is new CLIC.Subcommand.Command with record
      Dry_Run : aliased Boolean := False;
   end record;

end Commands.Generate;