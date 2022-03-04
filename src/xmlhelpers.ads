with Ada.Streams;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;   use Ada.Wide_Wide_Text_IO;
with DOM.Core.Documents;      use DOM.Core.Documents;
with DOM.Core;                use DOM.Core;
with DOM.Core.Elements;       use DOM.Core.Elements;
with DOM.Core.Nodes;          use DOM.Core.Nodes;
with Ada.Streams.Stream_IO;   use Ada.Streams.Stream_IO;
with Ada.Characters.Conversions;
with Ada.Directories;
with DOM.Readers;
with Globals;
with Version;

package Xmlhelpers is

   procedure Add_Node (
      Node_Name,
      Node_Value : String;
      Parent_Node : DOM.Core.Element; Feed : Node);

   procedure Add_Link (
      Parent_Node : DOM.Core.Element;
      Url, Relationship : String; Feed : Node);

   procedure Add_Generator (
      Parent_Node : DOM.Core.Element; Feed : Node);

   procedure Add_Author (Parent_Node : DOM.Core.Element;
                         Name, Email : String; Feed : Node);

end Xmlhelpers;
