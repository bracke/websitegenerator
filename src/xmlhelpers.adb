package body Xmlhelpers is

   procedure Add_Node
     (Node_Name, Node_Value : String; Parent_Node : DOM.Core.Element;
      Feed                  : Node)
   is

      Feed_Text : Text;
      Feed_Data : DOM.Core.Element;
   begin
      Feed_Data :=
        Append_Child (Parent_Node, Create_Element (Feed, Node_Name));
      Feed_Text := Create_Text_Node (Feed, Node_Value);
      if Append_Child (Feed_Data, Feed_Text) /= null then
         return;
      end if;
   end Add_Node;

   procedure Add_Link
     (Parent_Node : DOM.Core.Element; Url, Relationship : String; Feed : Node)
   is
      Link_Node : DOM.Core.Element;
   begin
      Link_Node := Append_Child (Parent_Node, Create_Element (Feed, "link"));
      Set_Attribute (Link_Node, "rel", Relationship);
      Set_Attribute (Link_Node, "href", Url);
   end Add_Link;

   procedure Add_Generator (Parent_Node : DOM.Core.Element; Feed : Node) is

      Generator_Node : DOM.Core.Element;
      Feed_Text      : Text;
   begin
      Generator_Node :=
        Append_Child (Parent_Node, Create_Element (Feed, "generator"));

      Set_Attribute (Generator_Node, "uri", Version.Link);
      Set_Attribute (Generator_Node, "version", Version.Current);

      Feed_Text := Create_Text_Node (Feed, Version.Name);
      if Append_Child (Generator_Node, Feed_Text) /= null then
         return;
      end if;

   end Add_Generator;

   procedure Add_Author
     (Parent_Node : DOM.Core.Element; Name, Email : String; Feed : Node)
   is

      Author_Node : DOM.Core.Element;
   begin
      Author_Node :=
        Append_Child (Parent_Node, Create_Element (Feed, "author"));

      if Name'Length > 0 then
         Add_Node ("name", Name, Author_Node, Feed);
      end if;
      if Email'Length > 0 then
         Add_Node ("email", Email, Author_Node, Feed);
      end if;
   end Add_Author;

end Xmlhelpers;
