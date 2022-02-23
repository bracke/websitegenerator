with Blueprint; use Blueprint;
with Server;
with Templates_Parser;
with Ada.Text_IO;
with Generator;
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
with Ada.Directories;
with Ada.Strings.Unbounded;

package body Commands.Publish is

   use Ada.Characters.Conversions;
   use GNAT.Strings;
   use Ada.Containers;
   use Generator.aString;
   use Ada.Strings.Unbounded;

   package IO renames Ada.Text_IO;
   package TT renames CLIC.TTY;
   package DIR renames Ada.Directories;

  -------------
  -- Execute --
  -------------
  overriding
  procedure Execute ( Cmd  : in out Instance;
                      Args :        AAA.Strings.Vector)
  is
    ToDo : Action := Write;
    Source : XString := To_XString(To_Wide_Wide_String(DIR.Current_Directory));
    Default_Target : string := DIR.Compose(
      DIR.Current_Directory,
      "_dist");
    Target : XString := To_XString(To_Wide_Wide_String(Default_Target));
  begin
   if Args.Length > 0 then
     Target := To_XString(To_Wide_Wide_String(DIR.Full_Name (Args(1))));
   end if;

   if Cmd.Source.all /= "" then
      Source := To_XString(To_Wide_Wide_String(Cmd.Source.all));
   end if;

   declare
      Website_Source : String := DIR.Full_Name(To_String(To_String(Source)));
      Source_Layout_Folder : string := DIR.Compose(Website_Source, "_layouts");
      Website_Target : String := DIR.Full_Name(To_String(To_String(Target)));
   begin
      if DIR.Exists(Source_Layout_Folder) then
         -- Delete result of last run
         if Cmd.Delete_Target_Content and then DIR.Exists(Website_Target) then
            DIR.Delete_Tree(Website_Target);
         end if;
         if not DIR.Exists(Website_Target) then
            DIR.Create_Directory(Website_Target);
         end if;
         if DIR.Exists(Website_Source) then
            Generator.Start (Website_Source, Website_Target);
            DIR.Set_Directory (Directory => Website_Target);
            Server.Start_Server;
         else
            IO.Put_Line(TT.Error("Source folder (" & Website_Source & ") does not exist."));
         end if;
      else
         IO.Put_Line(TT.Error("Source folder (" & Website_Source & ") is not a website. Please create a website using the " & TT.Emph ("websitegenerator new <name>") & " command."));
      end if;
   end;
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
        (Config      => Config,
         Output      => Cmd.Source'Access,
         Argument    => "PATH",
         Long_Switch => "--source=",
         Help        => "Selects a source folder other than the current directory");

      Define_Switch
      (Config, Cmd.Delete_Target_Content'Access, "", "-d", "Publish_Delete_Switch_Message");

   end Setup_Switches;

end Commands.Publish;