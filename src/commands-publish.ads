with AAA.Strings;
private with GNAT.Strings;
with Globals;

package Commands.Publish is

   type Instance
   is new CLIC.Subcommand.Command
   with private;

   overriding
   function Name (Cmd : Instance) return
      CLIC.Subcommand.Identifier is ("publish");

   overriding procedure Execute
   (Cmd : in out Instance; Args : AAA.Strings.Vector);

   overriding
   function Switch_Parsing (This : Instance)
                            return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.Parse_All);

   overriding
   function Long_Description
   (Cmd : Instance) return AAA.Strings.Vector is
   (AAA.Strings.Empty_Vector.Append
     ("Publish the website to the target folder.")
     .New_Line
     );

   overriding procedure Setup_Switches
   (Cmd    : in out Instance;
    Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding function Short_Description (Cmd : Instance) return
      String is ("Publish the website to the target folder.");

   overriding function Usage_Custom_Parameters (Cmd : Instance)
   return String is ("[targetfolder] [--source <folderpath>] [-d]");

private

   type Instance is new CLIC.Subcommand.Command with record
      Source : aliased GNAT.Strings.String_Access;
      Delete_Target_Content : aliased Boolean := False;
   end record;

end Commands.Publish;