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
 --     "Publish_Delete_Switch_Message" => "Delete content of the target folder before publishing into that folder"
 --  );

   Blueprint_Folder_Name      : constant string := "blueprints";
   Blueprint_Default          : constant String := "simple";

   Posts_Source_Folder_Name   : constant string := "_posts";
   Pages_Folder_Name          : constant string := "pages";
   Layout_Folder_Name         : constant string := "_layouts";
   Dist_Folder_Name           : constant string := "_dist";
   Blog_Target_Folder_Name    : constant string := "blog";
   Feed_filename              : constant string := "rssfeed.xml";
   Site_Configuration_Name    : constant string := "_site.cfg";
   Excerpt_Separator          : constant string := "<!--more-->";

end Globals;