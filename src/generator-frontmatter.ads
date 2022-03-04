with Generator;
with GNATCOLL.Config;

package Generator.Frontmatter is

   use GNATCOLL.Config;

   function Read_Excerpt (
      Content : String;
      Excerpt_Separator : String) return String;

   function Read_CreateDate (Filename : String) return String;

   function Read (Filepath : String;
                  Targetpath : String;
                  Linkpath : String) return Document;

   procedure Read_Content (Filepath : String; T : in out Translate_Set);

private

end Generator.Frontmatter;