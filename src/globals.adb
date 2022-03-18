with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings;

package body Globals is

   Environment_String : String :=
      Ada.Characters.Handling.To_Upper (
         Ada.Environment_Variables.Value ("OS", "linux"));

   Unix_Lineending  : constant String
      := String'(1 => ASCII.LF);

   Windows_Lineending : constant String
      := String'(1 => ASCII.CR, 2 => ASCII.LF);

begin
   if Ada.Strings.Fixed.Index (Environment_String,"WINDOWS") = 0 then
      Current_OS := Unix;
      Current_Lineending := To_Unbounded_String(Unix_Lineending);
   else
      Current_OS := Windows;
      Current_Lineending := To_Unbounded_String(Windows_Lineending);
   end if;

end Globals;