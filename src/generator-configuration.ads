with GNATCOLL.Config;

package Generator.Configuration is

   use GNATCOLL.Config;

   Site_Title        : constant Config_Key := Create ("title", "site");
   Site_Email        : constant Config_Key := Create ("email", "site");
   Site_Description  : constant Config_Key := Create ("description", "site");
   Site_Baseurl      : constant Config_Key := Create ("baseurl", "site");
   Site_Url          : constant Config_Key := Create ("url", "site");

   function Read(Filepath : String) return GNATCOLL.Config.Config_Pool;

private

end Generator.Configuration;