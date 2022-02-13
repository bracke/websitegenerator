pragma Ada_2012;
package body Generator.Markdown is

   -------------
   -- To_HTML --
   -------------

   function To_HTML (Filepath : XString) return XString is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_HTML unimplemented");
      return raise Program_Error with "Unimplemented function To_HTML";
   end To_HTML;

end Generator.Markdown;
