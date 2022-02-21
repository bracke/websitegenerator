with Server;
with Templates_Parser;
with Ada.Text_IO; use Ada.Text_IO;
with Generator;
with Ada.Directories;
with Ada.Calendar;
with GNAT.Command_Line; use GNAT.Command_Line;
with Ada.Wide_Wide_Text_IO, Ada.Wide_Wide_Text_IO.Text_Streams, Ada.Command_Line, Ada.Exceptions, Ada.Calendar;
with Ada.Exceptions; use Ada.Exceptions;
with CLIC.Subcommand;
with Commands;
with CLIC.TTY;

procedure Websitegenerator is

   use Ada.Calendar;
   use Ada.Wide_Wide_Text_IO.Text_Streams;

   use Generator.aString;
   use Templates_Parser;

   Start_Processing, End_Processing : Time;
   How_Long                         : Duration;

   S : constant Stream_Access := Stream (Ada.Wide_Wide_Text_IO.Current_Output);
begin
   Ada.Text_IO.New_Line;

   Start_Processing := Clock;
   Commands.Execute;
   End_Processing := Clock;
   How_Long       := (End_Processing - Start_Processing) * 1_000;

   Character'Write (S, ASCII.LF);
   String'Write (S, "Website generated in ");
   Wide_Wide_String'Write (S, How_Long'Wide_Wide_Image);
   String'Write (S, "ms!");
   Character'Write (S, ASCII.LF);

exception
   when E : others =>
      Ada.Text_IO.Put_Line(Exception_Message (E));
end Websitegenerator;