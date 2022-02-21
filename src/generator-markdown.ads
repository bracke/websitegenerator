with Ada.Streams;
with Ada.Streams.Stream_IO;

package Generator.Markdown is

   procedure To_HTML (Filein : in out Ada.Streams.Stream_IO.File_Type);

private

end Generator.Markdown;