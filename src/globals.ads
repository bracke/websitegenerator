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

   M : Map :=(
      ("Publish_Delete_Switch_Message","Delete content of the target folder before publishing into that folder")
   );

   Blueprint_Folder_Name : string := "blueprints";
   Posts_Folder_Name : string := "_posts";
   Layout_Folder_Name : string := "_layouts";
   Site_Configuration_Name : string := "_site.cfg";



end Globals;