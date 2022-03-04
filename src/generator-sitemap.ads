with Ada.Streams;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;   use Ada.Wide_Wide_Text_IO;
with DOM.Core.Documents;      use DOM.Core.Documents;
with DOM.Core;                use DOM.Core;
with DOM.Core.Elements;       use DOM.Core.Elements;
with DOM.Core.Nodes;          use DOM.Core.Nodes;
with Ada.Streams.Stream_IO;   use Ada.Streams.Stream_IO;

with DOM.Readers;
with Globals;
with Version;

package Generator.Sitemap is

   function Create (
      Posts       : Document_Container.List;
      Documents   : Document_Container.List;
      Targetpath  : String;
      Site_Set    : Translate_Set)

      return String;

end Generator.Sitemap;
