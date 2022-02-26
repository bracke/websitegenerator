with Ada.Streams;
with Ada.Streams.Stream_IO;

package Generator.Rssfeed is

   function Create(Posts: Document_Container.list; Targetpath: String; Site_Set: Translate_Set) return string;

end Generator.Rssfeed;