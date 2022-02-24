with Ada.Containers;  use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Blueprint;       use Blueprint;
with AAA.Strings;     use AAA.Strings;
with Ada.Text_IO;
with Ada.Command_Line;
with Templates_Parser;
with CLIC.TTY;
with Filesystem;
with Commands;
with Globals;
with Ada.Characters.Handling;

package body Commands.Generate is

  package IO renames Ada.Text_IO;
  package TT renames CLIC.TTY;

   use Ada.Characters.Handling;

 function Target_Folder_Name(ItemType: string) return string is
 begin
   if ItemType = "POST" then
      return Globals.Posts_Folder_Name;
   elsif ItemType = "PAGE" then
      return Globals.Pages_Folder_Name;
   elsif ItemType = "LAYOUT" then
      return Globals.Layout_Folder_Name;
   end if;

   return "";

 end Target_Folder_Name;

  -------------
  -- Execute --
  -------------

  overriding
  procedure Execute ( Cmd  : in out Instance;
                      Args :        AAA.Strings.Vector) is
  begin
    if Args.Length > 1 then

      declare
        ItemType          : string := To_Upper(Element (Args, 1));
        Name              : String := Element (Args, 2);
        Blueprint         : String := Args.First_Element;
        Blueprint_Path    : String := Compose(Get_Blueprint_Folder,Blueprint);
        Current           : String := Current_Directory;
        ToDo              : Action := Write;
        Target            : String := Compose(Current,Target_Folder_Name(ItemType));
      begin
        if Cmd.Dry_Run then
          IO.Put_Line(TT.Emph("You specified the dry-run flag, so no changes will be written."));
          ToDo := DryRun;
        end if;
        if not Exists(Target) then
         Create_Directory(Target);
        end if;
        Templates_Parser.Insert
         (Commands.Translations, Templates_Parser.Assoc ("NAME", Name));

        if Exists (Blueprint_Path) then
          Iterate (Blueprint_Path, Target, ToDo);

          IO.Put_Line (TT.Success( "Successfully generated " & Blueprint) & " " & TT.Warn (TT.Bold (Name)));
        else
          IO.Put_Line (TT.Error("Blueprint" & " " & Blueprint_Path & " " & "not found"));
        end if;
      end;
    else
      IO.Put_Line(TT.Error("Command requires a blueprint and a name to be specified."));
    end if;

  end Execute;

  overriding
  function Long_Description(Cmd : Instance) return AAA.Strings.Vector is

    Description : AAA.Strings.Vector := AAA.Strings.Empty_Vector;
    Blueprints  : AAA.Strings.Vector := Filesystem.Read_Directory(Get_Blueprint_Folder, false);
  begin
    Append(Description, TT.Description("Generates new code from blueprints."));
    Description.New_Line;
    Description.Append(TT.Underline("Available blueprints"));

    for A_Blueprint of Blueprints loop
      Append(Description, TT.Emph (A_Blueprint));
    end loop;

    return Description;

  end Long_Description;

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

end Commands.Generate;