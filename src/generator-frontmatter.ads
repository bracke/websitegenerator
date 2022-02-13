with Generator;
with GNATCOLL.Config;

package Generator.Frontmatter is

   use GNATCOLL.Config;

   function Read (Filepath : string; Targetpath: string; Linkpath : string) return Document;

   procedure Read_Content(Filepath: string; T: in out Translate_Set);

private

end Generator.Frontmatter;