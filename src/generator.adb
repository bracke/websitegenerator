pragma Ada_2012;
with Generator.Configuration;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Generator.Frontmatter;
with Ada.Characters.Conversions;
with Templates_Parser;
package body Generator is

   use Templates_Parser;
   use Ada.Directories;
   use Ada.Characters.Conversions;
   use Ada.Text_IO;

   Site_Set : Translate_Set;

   ------------------
   -- Process_File --
   ------------------
   procedure Process_File(Filepath : string; Targetpath: string; Linkpath : string) is

      Extension : String := Ada.Characters.Handling.To_Upper(Ada.Directories.Extension(Filepath));
   begin
      if Extension = "MD" or Extension = "MARKDOWN" then
         Documents.Append(Generator.Frontmatter.Read(Filepath, Targetpath, Linkpath));
      elsif Extension = "HBS" then
         Documents.Append(Generator.Frontmatter.Read(Filepath, Targetpath, Linkpath));
      else
         Copy_File(Filepath, Targetpath);
      end if;
   end Process_File;

   -----------------------
   -- Process_Directory --
   -----------------------
   procedure Process_Directory(Source_Directory : string; Target_Directory: string; Linkpath: string) is

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
                        Process_File(Fullname, Targetname, Linkpath & "/" & Basename & ".html");
                     else
                        if not Exists(Targetname) then
                           Create_Directory(Targetname);
                        end if;
                        Process_Directory(Fullname, Targetname, Linkpath & "/" & Name);
                     end if;
                  end if;
               end;
            end if;

            exit when not More_Entries(Dir_Search);
         end loop;

         End_Search(Dir_Search);
      end if;
   end Process_Directory;

   procedure Process_Documents(Source_Directory : String; Targetpath: String) is

      Layoutfolder   : String := Compose(Source_Directory, "_layouts");
      Includesfolder : String := Compose(Source_Directory, "_includes");

      Pagepath : Tag;
      Pagename : Tag;
   begin
      -- Create vector with pages
      for Document of Documents loop
         declare
            Assoc : association := Get(Document.T, "name");
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

      for Document of Documents loop
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
               Insert(Combined_Set, Site_Set);
               Insert (Combined_Set, Assoc ("PAGEPATH", Pagepath));
               Insert (Combined_Set, Assoc ("PAGENAME", Pagename));
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

   -----------
   -- Start --
   -----------
   procedure Start (Source_Directory : String; Target_Directory: String) is

      Config_Path : String := Ada.Directories.Compose(Source_Directory, "_site.cfg");
   begin
      Site_Set := Null_Set;
      if Exists(Config_Path) then
         Generator.Frontmatter.Read_Content(Config_Path, Site_Set);
      else
         Ada.Text_IO.Put_Line("No site configuration found at " & Config_Path);
      end if;

      -- Delete result of last run
      if Exists(Target_Directory) then
         Delete_Tree(Target_Directory);
      end if;
      Create_Directory(Target_Directory);

      -- Copy static files and directories and create list of pages.
      Process_Directory(Source_Directory,Target_Directory, "");

      -- Process non-static files
      Process_Documents(Source_Directory,Target_Directory);

   end Start;

end Generator;