pragma Ada_2012;

with Ada.Characters.Conversions;
with Ada.Directories;
with Ada.Text_IO;

package body Generator.Configuration is

   use Ada.Characters.Conversions;

   ----------
   -- Read --
   ----------
   function Read (Filepath : String) return GNATCOLL.Config.Config_Pool is

      Config : Config_Pool;
      C : INI_Parser;
   begin
      if Ada.Directories.Exists(Filepath) then

         GNATCOLL.Config.Open (C, (Filepath));
         Fill (Config, C);

      else
         if (Debug) then
            Ada.Text_IO.Put_Line("Config file " & Filepath & " does not exist");
         end if;
      end if;
      return Config;
   end Read;

end Generator.Configuration;
