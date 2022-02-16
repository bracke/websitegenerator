with Ada.Containers;  use Ada.Containers;
with Ada.Directories; use Ada.Directories;
with Blueprint;       use Blueprint;
with AAA.Strings;     use AAA.Strings;
with Ada.Command_Line;
with Ada.Text_IO;
with Templates_Parser;
with CLIC.TTY;
with Filesystem;
with Commands;

package body Commands.Destroy is

  -------------
  -- Execute --
  -------------

  overriding
  procedure Execute ( Cmd  : in out Instance;
                      Args :        AAA.Strings.Vector) is
  begin
    if Args.Length > 1 then
      declare
        Name            : String := Element (Args, 2);
        Blueprint       : String := Args.First_Element;
        Blueprint_Path  : String := Compose(Get_Blueprint_Folder,Blueprint);
        Current         : String := Current_Directory;
      begin
        Templates_Parser.Insert
         (Commands.Translations, Templates_Parser.Assoc ("NAME", Name));

        if Exists (Blueprint_Path) then
          Iterate (Blueprint_Path, Current, Delete);
           IO.Put_Line (TT.Success("Successfully deleted " & Blueprint) & " " & TT.Warn (TT.Bold (Name)));
        else
          IO.Put_Line (TT.Error("Blueprint" & " " & Blueprint_Path & " " & "not found"));
        end if;
      end;
    else
      IO.Put_Line(TT.Error("Command requires a blueprint and a name to be specified."));
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
      null;
   end Setup_Switches;

end Commands.Destroy;