with GNAT.Regexp;       use GNAT.Regexp;
with Ada.Directories;
with Ada.Characters.Conversions;
with Ada.Text_IO;
with GNATCOLL.Mmap;
with Templates_Parser;
with GNAT.Strings;
with Ada.Strings.Fixed;
with Globals;
with Ada.Exceptions; use Ada.Exceptions;
with Linereader;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;

package body Generator.Frontmatter is

   use Ada.Characters.Conversions;
   use Ada.Text_IO;
   use GNATCOLL.Mmap;
   use Ada.Strings.Maps;
   use Ada.Strings.Maps.Constants;
   use GNAT.Strings;
   package FIX renames Ada.Strings.Fixed;
   package DIR renames Ada.Directories;
   package CC renames Ada.Characters.Conversions;

   whitespace : constant Character_Set :=
       To_Set (' ' & ASCII.LF & ASCII.HT & ASCII.CR & Character'val(0));

   useless_characters : constant Character_Set := Control_Set and To_Set (' ');

   WW_HT : constant Wide_Wide_Character := To_Wide_Wide_Character (ASCII.HT);
   WW_LF : constant Wide_Wide_Character := To_Wide_Wide_Character (ASCII.LF);
   WW_CR : constant Wide_Wide_Character := To_Wide_Wide_Character (ASCII.CR);
   WW_Colon : constant Wide_Wide_Character := To_Wide_Wide_Character (ASCII.Colon);

   package LR is new Linereader(Ada.Strings.Unbounded.To_String(Globals.Current_Lineending));
   use LR;

   function Read_Excerpt (
      Content : String;
      Excerpt_Separator : String) return String is
   begin
      if Excerpt_Separator /= "" and then
         Ada.Strings.Fixed.Index (Content, Excerpt_Separator) /= 0
      then
         return Ada.Strings.Fixed.Head (Content,
            Ada.Strings.Fixed.Index (Content, Excerpt_Separator));

      elsif Ada.Strings.Fixed.Index (Content,
         Globals.Excerpt_Separator) /= 0
      then
         return Ada.Strings.Fixed.Head (Content,
            Ada.Strings.Fixed.Index (Content, Globals.Excerpt_Separator));
      end if;

      return Ada.Strings.Fixed.Head (Content,
         Ada.Strings.Fixed.Index (Content, "" & ASCII.LF));

   end Read_Excerpt;

   function Read_CreateDate (Filename : String) return String is
      Re : constant Regexp :=
         Compile ("([0-9]{4})-?(1[0-2]|0[1-9])", Glob => True);
   begin
      if Match (Filename, Re) then
         return Filename (Filename'First .. Filename'First + 10);
      end if;
      return "";
   end Read_CreateDate;

   function Is_Frontmatter (Line : String) return Boolean is
      Last : Natural := Line'First+Globals.Front_Matter_Prefix'Length - 1;
      Prefix : String Renames Line(Line'First .. Last);
   begin
      return Prefix'Length >= Globals.Front_Matter_Prefix'Length And then
         Prefix = Globals.Front_Matter_Prefix;
   end Is_Frontmatter;

   procedure Read_Content (Filepath : String; T : in out Translate_Set) is

      Basename      : constant String        := DIR.Base_Name (Filepath);
      Created_Date  : constant String        := Read_CreateDate (Basename);

      Source_File   : constant Mapped_File   := Open_Read (Filepath);
      Source_Region : constant Mapped_Region := Read (File => Source_File,
                                                      Advice => Use_Sequential,
                                                      Mutable => False);

      L             : constant Natural       := Natural (Length (Source_File));
      Source_ptr    : constant Str_Access    := Data (Source_Region);

      R : LR.Reader (Source_ptr, L);
   begin
      loop
         exit when R.End_Of_Input;

         R.Backup;
         declare
            A_Line : constant String := R.Get_Line;
         begin
           exit when not Is_Frontmatter (A_Line);
           declare
              First : Positive := A_Line'First + Globals.Front_Matter_Prefix'Length;
              Frontmatter : String renames A_Line (First .. A_Line'Last);

              Separator_Position : Natural := FIX.Index (Frontmatter, Globals.Front_Matter_Separator);
              -- Index(Source, Maps.To_Set(Space), Outside, Going)
              First_Nonblank_Position  : Natural := FIX.Index_Non_Blank (Frontmatter, Ada.Strings.Forward);
              Last_Nonblank_Position   : Natural := FIX.Index_Non_Blank (Frontmatter, Ada.Strings.Backward);
           begin
              if Separator_Position /= 0 and then
                  First_Nonblank_Position < Separator_Position then

                  Insert
                  (T, Assoc
                     (FIX.Trim(Frontmatter (First_Nonblank_Position ..
                     Separator_Position-1), whitespace,whitespace),

                      FIX.Trim(Frontmatter (Separator_Position+1 ..
                      Last_Nonblank_Position), whitespace,whitespace)
                     )
                  );
              end if;
            end;
         exception
            when E : others =>
               Put_Line (Exception_Message (E));
         end;
      end loop;

      if not R.End_Of_Input then
         R.Restore;
         Insert (T, Assoc ("content", R.Get_Remainder));
      end if;

      Insert (T,
               Assoc ("summary",
                     Read_Excerpt (Generator.Read_From_Set (T, "content"),
                     Generator.Read_From_Set (T, "excerpt_separator"))
               )
            );

      if Generator.Read_From_Set (T, "created") = "" then
         if Created_Date /= "" then
            Insert (T, Assoc ("created", Created_Date));
         end if;
      end if;

      if Generator.Read_From_Set (T, "updated") = "" then
         Insert (T, Assoc ("updated",
            Generator.Read_From_Set (T, "created")
         ));
      end if;

   end Read_Content;

   ----------
   -- Read --
   ----------
   function Read (
      Filepath    : String;
      Targetpath  : String;
      Linkpath    : String) return Document is

      aDocument : Document;

      Containing_Directory : constant String :=
         Ada.Directories.Containing_Directory (Targetpath);

      Base_Name : constant String := Ada.Directories.Base_Name (Targetpath);

      Targetname : constant String :=
         Ada.Directories.Compose (Containing_Directory,
         Base_Name, Globals.HTML_Filetype);

      Filename : constant String :=
         Ada.Directories.Compose ("", Base_Name, "html");
   begin
      aDocument.Filepath   := To_XString (To_Wide_Wide_String (Filepath));
      aDocument.Targetpath := To_XString (To_Wide_Wide_String (Targetname));
      aDocument.Filename   := To_XString (To_Wide_Wide_String (Filename));
      aDocument.Linkpath   := To_XString (To_Wide_Wide_String (Linkpath));
      aDocument.Basename   := To_XString (To_Wide_Wide_String (Base_Name));

      if Ada.Directories.Exists (Filepath) then

         Read_Content (Filepath, aDocument.T);
         declare
            Layout : String := Read_From_Set(aDocument.T, "layout");
         begin
            if Layout'Length = 0 then
               Ada.Text_IO.Put_Line ("File " &
               Layout & " has no layout defined in frontmatter");
            else
               aDocument.Layout := To_XString (
                  To_Wide_Wide_String (Layout));
            end if;
         end;
      else
         Ada.Text_IO.Put_Line ("File " & Filepath &
         " does not exist");
      end if;

      Insert (aDocument.T, Assoc ("basename", Base_Name));
      Insert (aDocument.T, Assoc ("filename", Filename));
      Insert (aDocument.T, Assoc ("targetname", Targetname));
      Insert (aDocument.T, Assoc ("linkpath",
      Ada.Strings.Fixed.Trim (Linkpath, Generator.Slash, Generator.Slash)));
      Insert (aDocument.T, Assoc ("filepath", Filepath));
      return aDocument;

   end Read;

end Generator.Frontmatter;
