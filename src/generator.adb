pragma Ada_2012;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Generator.Frontmatter;
with Generator.Rssfeed;
with Generator.Sitemap;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Numerics.Discrete_Random;
with Progress_Indicators.Spinners;
with Version;
with Globals;

package body Generator is

   use Ada.Directories;
   use Ada.Text_IO;
   use Progress_Indicators.Spinners;
   package CH renames Ada.Characters.Handling;
   package CC renames Ada.Characters.Conversions;
   package DIR renames Ada.Directories;

   subtype Die is Integer range 1 .. 256;
   subtype Dice is Integer range 2 * Die'First .. 2 * Die'Last;
   package Random_Integer is new Ada.Numerics.Discrete_Random (Die);
   use Random_Integer;

   function "<" (Left, Right : Document) return Boolean is
   begin
      return Left.Basename < Right.Basename;
   end "<";

   ------------------
   -- Process_File --
   ------------------
   procedure Process_File
     (List : out Document_Container.List;
      Filepath : String;
      Targetpath : String;
      Linkpath : String)
   is
      Extension : constant String := CH.To_Upper (DIR.Extension (Filepath));
   begin
      if Extension = "MD" or else Extension = "MARKDOWN" then
         List.Append (
            Generator.Frontmatter.Read (Filepath, Targetpath, Linkpath)
         );
         declare
            Filein : Ada.Streams.Stream_IO.File_Type;
            Fileout : Ada.Text_IO.File_Type;
         begin
            null;
            --  Ada.Streams.Stream_IO.Open
            --  (Filein, Ada.Streams.Stream_IO.In_File, Filepath);
            --  Ada.Text_IO.Create
            --  (Fileout, Ada.Text_IO.Out_File,Targetpath);
            --  Renderer.Set_Output
            --  (Ada.Text_IO.Text_Streams.Stream(Fileout));
            --  Generator.Markdown.To_HTML (Filein);
         end;
      elsif Extension = "HTML" or else Extension = "HTM" then
         List.Append (
            Generator.Frontmatter.Read (Filepath, Targetpath, Linkpath)
         );

      elsif Extension = "TMPLT" then
         List.Append (
            Generator.Frontmatter.Read (Filepath, Targetpath, Linkpath)
         );
      else
         if DIR.Exists (Targetpath) then
            DIR.Delete_File (Targetpath);
         end if;
         Copy_File (Filepath, Targetpath);
      end if;
   end Process_File;

   -----------------------
   -- Process_Directory --
   -----------------------
   procedure Process_Directory
     (List              : out Document_Container.List;
     Source_Directory   : String;
      Target_Directory  : String;
      LinkpathIn        : String)
   is
      Linkpath : constant String :=
      Ada.Strings.Fixed.Trim (LinkpathIn, Slash, Slash);
      Dir : Directory_Entry_Type;
      Dir_Search : Search_Type;
   begin
      if Exists (Source_Directory) then

         Start_Search (Search => Dir_Search,
               Directory => Source_Directory,
               Pattern => "*");
         loop
            Get_Next_Entry (Dir_Search, Dir);

            if Simple_Name (Dir) /= "." and then Simple_Name (Dir) /= ".." then
               declare
                  Name : constant String := Simple_Name (Dir);
                  Fullname : constant String := Full_Name (Dir);
                  Targetname : constant String
                     := Compose (Target_Directory, Name);
                  Basename : constant String := Base_Name (Fullname);
                  Process : constant Boolean :=
                     Name /= "." and
                     Name /= ".." and
                     Ada.Strings.Fixed.Head (Name, 1) /= "_";
               begin
                  if Process then
                     if Debug then
                        Ada.Text_IO.Put_Line (Fullname);
                     end if;

                     if Kind (Dir) = Ordinary_File then
                        Process_File (
                           List,
                           Fullname,
                           Targetname,
                           Linkpath & "/" & Basename & ".html");
                     else
                        if not Exists (Targetname) then
                           Create_Directory (Targetname);
                        end if;
                        Process_Directory (
                           List,
                           Fullname,
                           Targetname,
                           Linkpath & "/" & Name);
                     end if;
                  end if;
               end;
            end if;

            exit when not More_Entries (Dir_Search);
         end loop;

         End_Search (Dir_Search);
      end if;
   end Process_Directory;

   function Get_Nav_Links (
      Document : Cursor;
      List : Document_Container.List) return Translate_Set is

      Set : Translate_Set;
      P : constant Cursor := Previous (Document);
      N : constant Cursor := Next (Document);
   begin
      if P /= No_Element then
         Insert (Set, Assoc ("previouslink",
                  CC.To_String (To_String (Element (P).Linkpath))));
      end if;

      if N /= No_Element then
         Insert (Set, Assoc ("nextlink",
                  CC.To_String (To_String (Element (N).Linkpath))));
      end if;

      return Set;

   end Get_Nav_Links;

   procedure Process_Documents (
      List : Document_Container.List;
      Set : Translate_Set;
      Layoutfolder : String;
      Source_Directory : String;
      Targetpath : String) is
   begin
      for Document in List.Iterate loop
         if Debug then
            Ada.Text_IO.Put_Line (
               CC.To_String (
               To_String (
               Element (Document).Targetpath))
            );
         end if;
         if Length (Element (Document).Layout) > 0 then
            declare
               Name : constant String :=
                  CC.To_String (
                  To_String (Element (Document).Layout)
               );
               Base_Name : constant String := DIR.Base_Name (Name);
               Extension : constant String := DIR.Extension (Name);
               Layoutfile : constant String :=
                  DIR.Compose (Layoutfolder, Base_Name, Extension);
               Combined_Set : Translate_Set;
               Filename : constant String :=
                  CC.To_String (
                  To_String (Element (Document).Targetpath)
               );
            begin
               Insert (Combined_Set, Set);
               Insert (Combined_Set, Element (Document).T);
               Insert (Combined_Set, Get_Nav_Links (Document, List));

               if DIR.Exists (Layoutfile) then

                  declare
                     F : File_Type;
                     Template : constant String :=
                     Templates_Parser.Parse (Layoutfile, Combined_Set);
                  begin
                     if Exists (Filename) then
                        Delete_File (Filename);
                     end if;
                     Create (F, Mode => Out_File, Name => Filename);
                     Put (F, Template);
                     Close (F);
                  end;
               else
                  Ada.Text_IO.Put_Line ("Layoutfile " &
                     Layoutfile & " does not exist");
               end if;
            end;
         else
            Ada.Text_IO.Put_Line ("Layout for " &
               CC.To_String (To_String (
               Element (Document).Filepath)) & " is not defined"
            );
         end if;
      end loop;

   end Process_Documents;

   function Create_Vector (
      List : Document_Container.List;
      Prefix : String) return Translate_Set is

      Set : Translate_Set;
      Pagepath : Tag;
      Pagename : Tag;
      Pageexcerpt : Tag;
   begin
      for Document of List loop
         declare
            Name : constant String
               := Read_From_Set (Document.T, "title");
            Base_Name : constant String :=
               CC.To_String (To_String (Document.Basename));
            Excerpt : constant String := Read_From_Set (Document.T, "title");
         begin
            Pagepath := Pagepath & Ada.Strings.Fixed.Trim (
               CC.To_String (To_String (Document.Linkpath)),
               Slash, Slash);

            Pageexcerpt := Pageexcerpt & Excerpt;
            if Name'Length > 0 then
               Pagename := Pagename & Name;
            else
               Pagename := Pagename & Base_Name;
            end if;
         end;
      end loop;
      Insert (Set, Assoc (Prefix & "path", Pagepath));
      Insert (Set, Assoc (Prefix & "name", Pagename));
      Insert (Set, Assoc (Prefix & "excerpt", Pageexcerpt));

      return Set;
   end Create_Vector;

   function Read_From_Set (
      Set : Translate_Set;
      Token : String) return String is

      Assoc : constant Association := Get (Set, Token);
   begin
      if Assoc /= Null_Association then
         return Get (Assoc);
      end if;
      return "";
   end Read_From_Set;

   -----------
   -- Start --
   -----------
   procedure Start (
      Source_Directory : String;
      Target_Directory : String) is

      Config_Path : constant String :=
      DIR.Compose (Source_Directory,
      Globals.Site_Configuration_Name);

      Layoutfolder : constant String := Compose (Source_Directory,
      Globals.Layout_Folder_Name);

      Blog_Source_Directory : constant String :=
      Compose (Source_Directory, Globals.Posts_Source_Folder_Name);
      Blog_Target_Directory : constant String :=
      Compose (Target_Directory, Globals.Blog_Target_Folder_Name);

      Documents   : Document_Container.List;
      Posts       : Document_Container.List;

      Set      : Translate_Set;
      Site_Set : Translate_Set;

      G : Random_Integer.Generator;
      D : Dice;
      Indicator : Spinner := Make;
   begin
      Ada.Text_IO.Put (Value (Indicator));
      Site_Set := Null_Set;
      if Exists (Config_Path) then
         Generator.Frontmatter.Read_Content (Config_Path, Site_Set);
      else
         Ada.Text_IO.Put_Line ("No site configuration found at " & Config_Path);
      end if;

      --  Copy static files and directories and create List of pages.
      Process_Directory (Documents, Source_Directory, Target_Directory, "");
      Sort (Documents);

      --  Process blog
      if Exists (Blog_Source_Directory) then
         --  Copy static files and directories and create List of pages.
         if not Exists (Blog_Target_Directory) then
            Create_Directory (Blog_Target_Directory);
         end if;
         Process_Directory (Posts, Blog_Source_Directory,
         Blog_Target_Directory, Globals.Blog_Target_Folder_Name);
      end if;
      Sort (Posts);

      Insert (Set, Create_Vector (Documents, "page"));
      Insert (Set, Create_Vector (Posts, "post"));
      Insert (Set, Site_Set);

      Insert (Set,  Assoc ("meta_generator_link", Version.Link));
      Insert (Set,  Assoc ("meta_generator", Version.Name));
      Insert (Set,  Assoc ("meta_generator_version", Version.Current));

      Reset (G); D := Random (G);
      Insert (Set,  Assoc ("meta_cachebuster",
         Ada.Strings.Fixed.Trim (D'Image, Ada.Strings.Both)));

      --  Create RSS feed
      Insert (Set,  Assoc ("atomfeedurl",
         Generator.Rssfeed.Create (Posts,
         Target_Directory, Site_Set))
      );
      --  Create RSS feed
      Insert (Set,  Assoc ("sitemapurl",
         Generator.Sitemap.Create (Posts, Documents,
         Target_Directory, Site_Set))
      );

      --  Process non-static files
      Process_Documents (Documents, Set, Layoutfolder,
         Source_Directory, Target_Directory);

      Process_Documents (Posts, Set, Layoutfolder,
         Blog_Source_Directory, Blog_Target_Directory);

      Disable_All;
      Ada.Text_IO.Put (Value (Indicator));

   end Start;

end Generator;