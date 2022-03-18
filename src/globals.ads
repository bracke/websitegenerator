with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Environment_Variables;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

package Globals is

   pragma Elaborate_Body;
   use Ada.Strings.Unbounded;

   Blueprint_Folder_Name      : constant String := "blueprints";
   Blueprint_Default          : constant String := "simple";
   HTML_Filetype              : constant String := "html";
   Front_Matter_Prefix        : constant String := "--";
   Front_Matter_Separator     : constant String := ":";
   Posts_Source_Folder_Name   : constant String := "_posts";
   Pages_Folder_Name          : constant String := "pages";
   Layout_Folder_Name         : constant String := "_layouts";
   Dist_Folder_Name           : constant String := "_site";
   Blog_Target_Folder_Name    : constant String := "blog";
   Feed_filename              : constant String := "rssfeed.xml";
   Sitemap_filename           : constant String := "sitemap.xml";
   Site_Configuration_Name    : constant String := "_site.cfg";
   Excerpt_Separator          : constant String := "<!--more-->";

   type OS is (Unix, Windows);
   Current_OS : OS := Unix;

   Current_Lineending : Unbounded_String;
end Globals;