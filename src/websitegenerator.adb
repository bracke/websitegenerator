with Server;
with Templates_Parser;
with Ada.Text_IO; use Ada.Text_IO;
with Generator;
with Ada.Directories;
with Ada.Calendar;
with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Wide_Wide_Text_IO, Ada.Wide_Wide_Text_IO.Text_Streams, Ada.Command_Line, Ada.Exceptions, Ada.Calendar;

procedure Websitegenerator is

   use Ada.Calendar;
   use Ada.Wide_Wide_Text_IO.Text_Streams;

   use Generator.aString;
   use Templates_Parser;

   Start_Processing, End_Processing : Time;
   How_Long                         : Duration;

   Config          : Command_Line_Configuration;
   Display_Version : aliased Boolean := False;
   Limit_Output    : aliased Boolean := False;

   Version : constant String := "1.0";

   S : constant Stream_Access := Stream (Ada.Wide_Wide_Text_IO.Current_Output);
begin
   Templates_Parser.Set_Tag_Separators("{{","}}");

   Define_Switch
     (Config, Display_Version'Access, "-v", Help => "Display version info");

   Define_Switch
     (Config, Limit_Output'Access, "-q",
      Help => "Quiet, limits output to data - no status information");

   if Display_Version then
      String'Write (S, Version);
   end if;

   if Ada.Command_Line.Argument_Count < 2 then
      String'Write (S, "usage: websitegenerator [source] [target]");
      return;
   else

      Start_Processing := Clock;

      declare
         Website_Source : String :=
         Ada.Directories.Full_Name (Ada.Command_Line.Argument (1));
         Website_Target : String :=
         Ada.Directories.Full_Name (Ada.Command_Line.Argument (2));
      begin
         Generator.Start (Website_Source, Website_Target);

         End_Processing := Clock;
         How_Long       := (End_Processing - Start_Processing) * 1_000;

         if not Limit_Output then
            Character'Write (S, ASCII.LF);
            String'Write (S, "Website generated in ");
            Wide_Wide_String'Write (S, How_Long'Wide_Wide_Image);
            String'Write (S, "ms!");
            Character'Write (S, ASCII.LF);
         end if;

         Ada.Directories.Set_Directory (Directory => Website_Target);
         Server.Start_Server;
      end;
   end if;

end Websitegenerator;