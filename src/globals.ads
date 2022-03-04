with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Globals is

   package Integer_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   use Integer_Hashed_Maps;

   --  M : Map :=(
   --  "Publish_Delete_Switch_Message" =>
   --  "Delete content of the target folder before publishing into that folder"
   --  );

   Blueprint_Folder_Name      : constant String := "blueprints";
   Blueprint_Default          : constant String := "simple";
   HTML_Filetype              : constant String := "html";
   Front_Matter_Deliminator   : constant String := "---";
   Posts_Source_Folder_Name   : constant String := "_posts";
   Pages_Folder_Name          : constant String := "pages";
   Layout_Folder_Name         : constant String := "_layouts";
   Dist_Folder_Name           : constant String := "_site";
   Blog_Target_Folder_Name    : constant String := "blog";
   Feed_filename              : constant String := "rssfeed.xml";
   Site_Configuration_Name    : constant String := "_site.cfg";
   Excerpt_Separator          : constant String := "<!--more-->";

end Globals;