pragma Ada_2012;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Generator.Frontmatter;
with Generator.Rssfeed;
--with Generator.Markdown;
with Ada.Characters.Conversions;
with Templates_Parser;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Numerics.Discrete_Random;
with Version;
with Globals;
--with Instances;

package body Generator is

   use Templates_Parser;
   use Ada.Directories;
   use Ada.Characters.Conversions;
   use Ada.Text_IO;

 --  Renderer : Instances.Html_Stream.Renderer_Ref;

   subtype Die is Integer range 1 .. 256;
   subtype Dice is Integer range 2*Die'First .. 2*Die'Last;
   package Random_Integer is new Ada.Numerics.Discrete_Random(Die);
   use Random_Integer;

   function "<"(Left, Right : Document) return boolean is
   begin
      return Left.Basename < Right.Basename;
   end;

   ------------------
   -- Process_File --
   ------------------
   procedure Process_File
     (List : out Document_Container.list;
      Filepath : String;
      Targetpath : String;
      Linkpath : String)
   is
      Extension : String := Ada.Characters.Handling.To_Upper(Ada.Directories.Extension(Filepath));
   begin
      if Extension = "MD" or else Extension = "MARKDOWN" then
         List.Append(Generator.Frontmatter.Read(Filepath, Targetpath, Linkpath));
         declare
            Filein : Ada.Streams.Stream_IO.File_Type;
            Fileout : Ada.Text_IO.File_Type;
         begin
         null;
            --Ada.Streams.Stream_IO.Open (Filein, Ada.Streams.Stream_IO.In_File, Filepath);
           -- Ada.Text_IO.Create(Fileout, Ada.Text_IO.Out_File,Targetpath);
           --   Renderer.Set_Output (Ada.Text_IO.Text_Streams.Stream(Fileout));
            --Generator.Markdown.To_HTML(Filein);
         end;
      elsif Extension = "HTML" or else Extension = "HTM" then
         List.Append(Generator.Frontmatter.Read(Filepath, Targetpath, Linkpath));

      elsif Extension = "TMPLT" then
         List.Append(Generator.Frontmatter.Read(Filepath, Targetpath, Linkpath));
      else
         if Ada.Directories.Exists(Targetpath) then
            Ada.Directories.Delete_File(Targetpath);
         end if;
         Copy_File(Filepath, Targetpath);
      end if;
   end Process_File;

   -----------------------
   -- Process_Directory --
   -----------------------
   procedure Process_Directory
     (List              : out Document_Container.list;
     Source_Directory   : String;
      Target_Directory  : String;
      Linkpath          : String)
   is

      Dir : Directory_Entry_Type;
      Dir_Search : Search_Type;
   begin
      if Ada.Directories.Exists(Source_Directory) then

         Start_Search(Search => Dir_Search,
               Directory => Source_Directory,
               Pattern => "*");
         loop
            Get_Next_Entry(Dir_Search, Dir);

            if Simple_Name(Dir) /= "." and then Simple_Name(Dir) /= ".." then
               declare
                  Name : String := Simple_Name(Dir);
                  Fullname : String := Full_Name(Dir);
                  Targetname : String := Compose(Target_Directory, Name);
                  Basename : String := Ada.Directories.Base_Name(Fullname);
                  Process : Boolean :=  Name /= "." and Name /= ".." and
                              Ada.Strings.Fixed.Head(Name,1) /= "_";
               begin
                  if Process then
                     if Debug then
                        Ada.Text_IO.Put_Line(Fullname);
                     end if;

                     if Kind(Dir) = Ordinary_File then
                        Process_File(List, Fullname, Targetname, Linkpath & "/" & Basename & ".html");
                     else
                        if not Exists(Targetname) then
                           Create_Directory(Targetname);
                        end if;
                        Process_Directory(List, Fullname, Targetname, Linkpath & "/" & Name);
                     end if;
                  end if;
               end;
            end if;

            exit when not More_Entries(Dir_Search);
         end loop;

         End_Search(Dir_Search);
      end if;
   end Process_Directory;

   procedure Process_Documents(List : in Document_Container.list; Set:Translate_Set; Layoutfolder: String; Source_Directory : String; Targetpath: String) is
   begin
      for Document of List loop
         if Debug then
            Ada.Text_IO.Put_Line(To_String(To_String(Document.Targetpath)));
         end if;
         if Length(Document.Layout) > 0 then
            declare
               Name : String := To_String(To_String(Document.Layout));
               Base_Name : String := Ada.Directories.Base_Name(Name);
               Extension : String := Ada.Directories.Extension(Name);
               Layoutfile : String := Ada.Directories.Compose(Layoutfolder, Base_Name, Extension);
               Combined_Set : Translate_Set;
               Filename : String := To_String(To_String(Document.Targetpath));
            begin
               Insert(Combined_Set, Set);
               Insert(Combined_Set, Document.T);

               if Ada.Directories.Exists(Layoutfile) then

                  declare
                     F : File_Type;
                     Template : String := Templates_Parser.Parse (Layoutfile, Combined_Set);
                  begin
                     Create (F, Mode => Out_File, Name => Filename);
                     Put (F, Template);
                     Close (F);
                  end;
               else
                  Ada.Text_IO.Put_Line("Layoutfile " & Layoutfile & " does not exist");
               end if;
            end;
         else
            Ada.Text_IO.Put_Line("Layout for " & To_String(To_String(Document.Filepath)) & " is not defined");
         end if;
      end loop;

   end Process_Documents;

   function Create_Vector(List : in Document_Container.list; Prefix: string) return Translate_Set is

      Set : Translate_Set;
      Pagepath : Tag;
      Pagename : Tag;
   begin
      for Document of List loop
         declare
            Assoc : association := Get(Document.T, "title");
            Name : string := Get(Assoc);
            Base_Name : string := To_String(To_String(Document.Basename));
         begin
            Pagepath := Pagepath & To_String(To_String(Document.Linkpath));

            if Name'Length > 0 then
               Pagename := Pagename & Name;
            else
               Pagename := Pagename & Base_Name;
            end if;
         end;
      end loop;
      Insert (Set, Assoc (Prefix & "PATH", Pagepath));
      Insert (Set, Assoc (Prefix & "NAME", Pagename));

      return Set;
   end Create_Vector;

   function Read_From_Set(Set : Translate_Set; Token: string) return string is

    Assoc : association := Get(Set,Token);
    begin
      if Assoc /= Null_Association then
         return Get(Assoc);
      end if;
      return "";
    end Read_From_Set;

   -----------
   -- Start --
   -----------
   procedure Start (Source_Directory : String; Target_Directory: String) is

      Config_Path : String := Ada.Directories.Compose(Source_Directory, Globals.Site_Configuration_Name);
      Layoutfolder: String := Compose(Source_Directory, Globals.Layout_Folder_Name);

      Blog_Source_Directory : string := Compose(Source_Directory, Globals.Posts_Source_Folder_Name);
      Blog_Target_Directory : string := Compose(Target_Directory,Globals.Blog_Target_Folder_Name);

      Documents   : Document_Container.list;
      Posts       : Document_Container.list;

      Set      : Translate_Set;
      Site_Set : Translate_Set;

      G : Random_Integer.Generator;
      D : Dice;
   begin
      Site_Set := Null_Set;
      if Exists(Config_Path) then
         Generator.Frontmatter.Read_Content(Config_Path, Site_Set);
      else
         Ada.Text_IO.Put_Line("No site configuration found at " & Config_Path);
      end if;

      -- Copy static files and directories and create list of pages.
      Process_Directory(Documents, Source_Directory, Target_Directory, "");
      Sort(Documents);

      -- Process blog
      if Exists(Blog_Source_Directory) then
         -- Copy static files and directories and create list of pages.
         if not Exists(Blog_Target_Directory) then
            Create_Directory(Blog_Target_Directory);
         end if;
         Process_Directory(Posts, Blog_Source_Directory, Blog_Target_Directory, Globals.Blog_Target_Folder_Name);
      end if;
      Sort(Posts);

      Insert(Set, Create_Vector(Documents, "PAGE"));
      Insert(Set, Create_Vector(Posts, "POST"));
      Insert(Set, Site_Set);

      Insert(Set,  Assoc ("META_GENERATOR_LINK", Version.Link));
      Insert(Set,  Assoc ("META_GENERATOR", Version.Name));
      Insert(Set,  Assoc ("META_GENERATOR_VERSION", Version.Current));

      Reset (G); D := Random(G);
      Insert(Set,  Assoc ("META_CACHEBUSTER", Ada.Strings.Fixed.Trim(D'image, Ada.strings.Both)));

      -- Create RSS feed
      Insert(Set,  Assoc ("ATOMFEEDURL", Generator.Rssfeed.Create(Posts,Target_Directory,Site_Set)));

      -- Process non-static files
      Process_Documents(Documents, Set, Layoutfolder, Source_Directory, Target_Directory);
      Process_Documents(Posts, Set, Layoutfolder, Blog_Source_Directory,Blog_Target_Directory);


   end Start;

end Generator;