with Ada.Directories;
with Xmlhelpers;
with Ada.Characters.Conversions;

package body Generator.Sitemap is

   use Ada.Characters.Conversions;
   package DIR renames Ada.Directories;

   function Create (
      Posts       : Document_Container.List;
      Documents   : Document_Container.List;
      Targetpath  : String;
      Site_Set    : Translate_Set) return String is

      Main_Node, Entry_Node : DOM.Core.Element;
      DOMi                  : DOM_Implementation;
      Feed                  : Node;
      Entries_Amount        : Natural := 0;

      Filepath : constant String :=
        DIR.Compose (Targetpath, Globals.Sitemap_filename);

      File_Handle : Ada.Streams.Stream_IO.File_Type;

   begin

      Feed      := Create_Document (Implementation => DOMi);
      Main_Node := Create_Element (Doc => Feed, Tag_Name => "urlset");
      DOM.Core.Elements.Set_Attribute
        (Main_Node, "xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9");

      Main_Node := Append_Child (N => Feed, New_Child => Main_Node);

      for aPost of Posts loop
         Entry_Node := Append_Child (Main_Node, Create_Element (Feed, "url"));

         Xmlhelpers.Add_Node
           ("loc",
            Generator.Read_From_Set (Site_Set, "site_base") & "/" &
            To_String (To_String (aPost.Linkpath)),
            Entry_Node, Feed);

         Xmlhelpers.Add_Node
           ("lastmod", Generator.Read_From_Set (aPost.T, "updated"),
            Entry_Node, Feed);

      end loop;

      for aDocument of Documents loop
         Entry_Node := Append_Child (Main_Node, Create_Element (Feed, "url"));

         Xmlhelpers.Add_Node
           ("loc",
            Generator.Read_From_Set (Site_Set, "site_base") & "/" &
            To_String (To_String (aDocument.Linkpath)),
            Entry_Node, Feed);

         Xmlhelpers.Add_Node
           ("lastmod", Generator.Read_From_Set (aDocument.T, "updated"),
            Entry_Node, Feed);

      end loop;

      Ada.Streams.Stream_IO.Create (File_Handle, Out_File, Filepath);
      Write (Stream (File => File_Handle), Feed, True);
      Close (File_Handle);

      return Filepath;

   end Create;

end Generator.Sitemap;