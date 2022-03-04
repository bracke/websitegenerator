package body Generator.Rssfeed is

   use Ada.Characters.Conversions;
   package DIR renames Ada.Directories;

   procedure Add_Node (
      Node_Name,
      Node_Value : String;
      Parent_Node : DOM.Core.Element; Feed : Node) is

      Feed_Text : Text;
      Feed_Data : DOM.Core.Element;
   begin
      Feed_Data := Append_Child (Parent_Node,
      Create_Element (Feed, Node_Name));
      Feed_Text := Create_Text_Node (Feed, Node_Value);
      if Append_Child (Feed_Data, Feed_Text) /= null then
         return;
      end if;
   end Add_Node;

   procedure Add_Link (
      Parent_Node : DOM.Core.Element;
      Url, Relationship : String; Feed : Node) is
      Link_Node : DOM.Core.Element;
   begin
      Link_Node := Append_Child (Parent_Node, Create_Element (Feed, "link"));
      Set_Attribute (Link_Node, "rel", Relationship);
      Set_Attribute (Link_Node, "href", Url);
   end Add_Link;

   procedure Add_Generator (
      Parent_Node : DOM.Core.Element; Feed : Node) is

      Generator_Node : DOM.Core.Element;
      Feed_Text : Text;
   begin
      Generator_Node := Append_Child (Parent_Node,
         Create_Element (Feed, "generator"));

      Set_Attribute (Generator_Node, "uri", Version.Link);
      Set_Attribute (Generator_Node, "version", Version.Current);

      Feed_Text := Create_Text_Node (Feed, Version.Name);
      if Append_Child (Generator_Node, Feed_Text) /= null then
         return;
      end if;

   end Add_Generator;

   procedure Add_Author (Parent_Node : DOM.Core.Element;
                         Name, Email : String; Feed : Node) is

      Author_Node : DOM.Core.Element;
   begin
      Author_Node := Append_Child (Parent_Node,
         Create_Element (Feed, "author"));

      if Name'Length > 0 then
         Add_Node ("name", Name, Author_Node, Feed);
      end if;
      if Email'Length > 0 then
         Add_Node ("email", Email, Author_Node, Feed);
      end if;
   end Add_Author;

   function Create (
      Posts       : Document_Container.List;
      Targetpath  : String;
      Site_Set    : Translate_Set) return String is

      Main_Node, Entry_Node : DOM.Core.Element;
      dsdfs : DOM_Implementation;
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
      Feed := Create_Document (Implementation => dsdfs);
      Main_Node := Create_Element (Doc => Feed, Tag_Name => "feed");

      DOM.Core.Elements.Set_Attribute (Main_Node,
         "xmlns", "http://www.w3.org/2005/Atom");
      Main_Node := Append_Child (N => Feed, New_Child => Main_Node);

      Add_Link (Main_Node,
         Generator.Read_From_Set (
            Site_Set, "site_base") & "/" &
            Globals.Feed_filename, "self", Feed);

      Add_Node ("id",
         Generator.Read_From_Set (Site_Set, "site_base") &
         "/", Main_Node, Feed);

      Add_Node ("title",
         Generator.Read_From_Set (Site_Set, "site_name"),
         Main_Node, Feed);

      Add_Node ("subtitle",
         Generator.Read_From_Set (Site_Set, "site_description"),
         Main_Node, Feed);

      Add_Generator (Main_Node, Feed);
      Add_Node ("rights",
         Generator.Read_From_Set (Site_Set, "site_license"),
         Main_Node,
         Feed);

      --   Add_Node
      --    (Node_Name => "updated",
      --     Node_Value => To_HTTP_Date(Date =>
      --     Local_Entries(1).Updated),
      --     Parent_Node => Main_Node);
      Add_Author (Main_Node,
                  Generator.Read_From_Set (Site_Set, "site_author"),
                  Generator.Read_From_Set (Site_Set, "site_base"), Feed);

      for aPost of Posts loop
         Entry_Node := Append_Child (Main_Node, Create_Element (Feed, "entry"));

         Add_Node ("id",
         Generator.Read_From_Set (Site_Set, "site_base") &
         "/" & To_String (To_String (aPost.Linkpath)),
         Entry_Node, Feed);

         Add_Node ("title",
            Generator.Read_From_Set (aPost.T, "title"),
            Entry_Node, Feed);

         Add_Node ("updated",
            Generator.Read_From_Set (aPost.T, "updated"),
            Entry_Node, Feed);

         Add_Node ("content",
            Generator.Read_From_Set (aPost.T, "content"),
            Entry_Node, Feed);

         Add_Link (Entry_Node,
            Generator.Read_From_Set (Site_Set, "site_base") & "/" &
            To_String (To_String (aPost.Linkpath)), "alternate", Feed);

         if Generator.Read_From_Set (aPost.T, "author") /= "" then
            Add_Author (
                  Entry_Node,
                  Generator.Read_From_Set (aPost.T, "author"),
                  Generator.Read_From_Set (aPost.T, "email"),
                  Feed);
         else
            Add_Author (
                  Entry_Node,
                  Generator.Read_From_Set (aPost.T, "site_author"),
                  Generator.Read_From_Set (aPost.T, "site_email"),
                  Feed);
         end if;
         if Generator.Read_From_Set (aPost.T, "excerpt") /= "" then
            Add_Node ("summary",
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