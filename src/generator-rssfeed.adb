with Xmlhelpers;

package body Generator.Rssfeed is

   use Ada.Characters.Conversions;
   package DIR renames Ada.Directories;

   function Create (
      Posts       : Document_Container.List;
      Targetpath  : String;
      Site_Set    : Translate_Set) return String is

      Main_Node, Entry_Node : DOM.Core.Element;
      DOMi : DOM_Implementation;
      Feed : Node;
      Entries_Amount : Natural := 0;

      Filepath : constant String :=
         DIR.Compose (Targetpath, Globals.Feed_filename);

      File_Handle : Ada.Streams.Stream_IO.File_Type;

      site_feed_max_items : constant String :=
         Generator.Read_From_Set (Site_Set, "site_feed_max_items");

      Max_Items : Natural := 0;
   begin
      if site_feed_max_items /= "" then
         begin
            Max_Items := Natural'Value (site_feed_max_items);
         exception
            when Constraint_Error => Max_Items := 0;
         end;
      end if;
      Feed      := Create_Document (Implementation => DOMi);
      Main_Node := Create_Element (Doc => Feed, Tag_Name => "feed");

      DOM.Core.Elements.Set_Attribute (Main_Node,
         "xmlns", "http://www.w3.org/2005/Atom");
      Main_Node := Append_Child (N => Feed, New_Child => Main_Node);

      Xmlhelpers.Add_Link
        (Main_Node,
         Generator.Read_From_Set (Site_Set, "site_base") & "/" &
            Globals.Feed_filename, "self", Feed);

      Xmlhelpers.Add_Node ("id",
         Generator.Read_From_Set (Site_Set, "site_base") &
         "/", Main_Node, Feed);

      Xmlhelpers.Add_Node ("title",
         Generator.Read_From_Set (Site_Set, "site_name"),
         Main_Node, Feed);

      Xmlhelpers.Add_Node ("subtitle",
         Generator.Read_From_Set (Site_Set, "site_description"),
         Main_Node, Feed);

      Xmlhelpers.Add_Generator (Main_Node, Feed);
      Xmlhelpers.Add_Node ("rights",
         Generator.Read_From_Set (Site_Set, "site_license"),
         Main_Node,
         Feed);

      --   Xmlhelpers.Add_Node
      --    (Node_Name => "updated",
      --     Node_Value => To_HTTP_Date(Date =>
      --     Local_Entries(1).Updated),
      --     Parent_Node => Main_Node);
      Xmlhelpers.Add_Author (Main_Node,
                  Generator.Read_From_Set (Site_Set, "site_author"),
                  Generator.Read_From_Set (Site_Set, "site_base"), Feed);

      for aPost of Posts loop
         Entry_Node := Append_Child (Main_Node, Create_Element (Feed, "entry"));

         Xmlhelpers.Add_Node ("id",
         Generator.Read_From_Set (Site_Set, "site_base") &
         "/" & To_String (To_String (aPost.Linkpath)),
         Entry_Node, Feed);

         Xmlhelpers.Add_Node ("title",
            Generator.Read_From_Set (aPost.T, "title"),
            Entry_Node, Feed);

         Xmlhelpers.Add_Node ("updated",
            Generator.Read_From_Set (aPost.T, "updated"),
            Entry_Node, Feed);

         Xmlhelpers.Add_Node ("content",
            Generator.Read_From_Set (aPost.T, "content"),
            Entry_Node, Feed);

         Xmlhelpers.Add_Link (Entry_Node,
            Generator.Read_From_Set (Site_Set, "site_base") & "/" &
            To_String (To_String (aPost.Linkpath)), "alternate", Feed);

         if Generator.Read_From_Set (aPost.T, "author") /= "" then
            Xmlhelpers.Add_Author (
                  Entry_Node,
                  Generator.Read_From_Set (aPost.T, "author"),
                  Generator.Read_From_Set (aPost.T, "email"),
                  Feed);
         else
            Xmlhelpers.Add_Author (
                  Entry_Node,
                  Generator.Read_From_Set (aPost.T, "site_author"),
                  Generator.Read_From_Set (aPost.T, "site_email"),
                  Feed);
         end if;
         if Generator.Read_From_Set (aPost.T, "excerpt") /= "" then
            Xmlhelpers.Add_Node ("summary",
            Generator.Read_From_Set (aPost.T, "summary"),
            Entry_Node,
            Feed);
         end if;

         Entries_Amount := Entries_Amount + 1;
         exit when Max_Items /= 0 and then Entries_Amount = Max_Items;
      end loop;

      Ada.Streams.Stream_IO.Create (File_Handle, Out_File, Filepath);
      Write (Stream (File => File_Handle), Feed, True);
      Close (File_Handle);

      return Filepath;
   end Create;

end Generator.Rssfeed;