pragma Ada_2012;
with GNAT.Regexp;     use GNAT.Regexp;
with Ada.Directories;
with Ada.Characters.Conversions;
with Ada.Text_IO;
with GNATCOLL.Mmap;
with Templates_Parser;
with GNAT.Strings;
with Generator;
with Ada.Strings.Fixed;
with Globals;

package body Generator.Frontmatter is

   use Ada.Characters.Conversions;
   use Ada.Text_IO;
   use GNATCOLL.Mmap;
   use Templates_Parser;
   use GNAT.Strings;

   package DIR renames Ada.Directories;

 --  whitespace : constant Character_Set := To_Set (' ' & ASCII.LF & ASCII.HT & ASCII.CR);

   WW_HT : constant Wide_Wide_Character := To_Wide_Wide_Character (ASCII.HT);
   WW_LF : constant Wide_Wide_Character := To_Wide_Wide_Character (ASCII.LF);
   WW_CR : constant Wide_Wide_Character := To_Wide_Wide_Character (ASCII.CR);

   function Read_Excerpt(Content: string; Excerpt_Separator: string) return string is
   begin
      if Excerpt_Separator /= "" and then
         Ada.Strings.Fixed.Index(Content, Excerpt_Separator) /= 0 then

         return Ada.Strings.Fixed.Head(Content, Ada.Strings.Fixed.Index(Content, Excerpt_Separator));

      elsif Ada.Strings.Fixed.Index(Content, Globals.Excerpt_Separator) /= 0 then

         return Ada.Strings.Fixed.Head(Content, Ada.Strings.Fixed.Index(Content, Globals.Excerpt_Separator));

      end if;

      return Ada.Strings.Fixed.Head(Content, Ada.Strings.Fixed.Index(Content, "" & ASCII.LF));

   end Read_Excerpt;

   function Read_CreateDate(Filename: String) return String is
      Re : constant Regexp := Compile ("([0-9]{4})-?(1[0-2]|0[1-9])", Glob => True);
   begin
      if Match (Filename, Re) then
         return Filename(Filename'First..Filename'First+10);
      end if;
      return "";
   end Read_CreateDate;

   procedure Read_Content(Filepath: String; T: in out Translate_Set) is

      Basename      : constant string           := DIR.Base_Name(Filepath);
      Created_Date  : constant string           := Read_CreateDate(Basename);

      Source_File   : Mapped_File               := Open_Read (Filepath);
      Source_Region : constant Mapped_Region    := Read (Source_File);
      L             : constant Natural          := Natural (Length (Source_File));
      Source_ptr    : constant Str_Access   := Data (Source_Region);
      Source : XString := To_XString(To_Wide_Wide_String(Source_ptr (1 .. L)));
      Position, Start, Equal_Position : Natural;
      Line,Item_Name,Item_Value : XString;
   begin
      -- Read first line
      Start := 1;
      Position := Source.Find(WW_LF,Start);
      Line := Source.Slice(Start, Position);
      Start := Position + 1;

      if To_String(To_String(Line.Head(3))) = Globals.Front_Matter_Deliminator then
         loop
            Position := Source.Find(WW_LF,Start);
            if Position = 0 then
               Position := Length(Line);
               exit;
            end if;

            Line := Source.Slice(Start, Position);
            Start := Position + 1;
            exit when To_String(To_String(Line.Head(3))) = Globals.Front_Matter_Deliminator;

            Equal_Position := Line.Find(":");

            if Equal_Position /= 0 then

               Item_Name := Line.Slice(1, Equal_Position-1);
               Item_Value := Line.Slice(Equal_Position+1, Line.Length);

               Item_Name := Item_Name.Trim;
               Item_Value := Item_Value.Trim;
               Item_Name := Item_Name.Trim(Ada.Strings.Both, WW_LF);
               Item_Value := Item_Value.Trim(Ada.Strings.Both, WW_LF);
               Item_Name := Item_Name.Trim(Ada.Strings.Both, WW_CR);
               Item_Value := Item_Value.Trim(Ada.Strings.Both, WW_CR);
               Item_Name := Item_Name.Trim(Ada.Strings.Both, WW_HT);
               Item_Value := Item_Value.Trim(Ada.Strings.Both, WW_HT);

               Insert (T,
                  Assoc (
                     To_String(To_String(Item_Name)),
                     To_String(To_String(Item_Value))
                  )
               );

            end if;
            Position := Source.Find(WW_LF,Start);
         end loop;

         Insert (T,
                  Assoc (
                     "content",
                     To_String(To_String(
                        Source.Slice(Start, Source.Length).Trim(Ada.Strings.Both, WW_CR).Trim(Ada.Strings.Both, WW_LF)))
                  )
               );
      else
         Insert (T,
                     Assoc (
                        "content",
                        To_String(To_String(Source))
                     )
                  );
      end if;

      Insert (T,
               Assoc ("summary",
                     Read_Excerpt(Generator.Read_From_Set(T, "content"),
                     Generator.Read_From_Set(T, "excerpt_separator"))
               )
            );

      if Generator.Read_From_Set(T, "created") /= "" then

         Insert (T, Assoc ("created",
                  Generator.Read_From_Set(T, "created")
         ));

      elsif Created_Date /= "" then

         Insert (T, Assoc ("created", Created_Date));

      end if;

      if Generator.Read_From_Set(T, "created") = "" then

         Insert (T, Assoc ("updated",
            Generator.Read_From_Set(T, "created")
         ));

      end if;

   end Read_Content;

   ----------
   -- Read --
   ----------
   function Read (Filepath: string; Targetpath: string; Linkpath : string) return Document is

      aDocument : Document;

      Containing_Directory : string := Ada.Directories.Containing_Directory(Targetpath);
      Base_Name : string := Ada.Directories.Base_Name(Targetpath);
      Targetname : string := Ada.Directories.Compose(Containing_Directory,Base_Name, Globals.HTML_Filetype);
      Filename : string := Ada.Directories.Compose("",Base_Name, "html");
   begin
      aDocument.Filepath   := To_XString(To_Wide_Wide_String(Filepath));
      aDocument.Targetpath := To_XString(To_Wide_Wide_String(Targetname));
      aDocument.Filename   := To_XString(To_Wide_Wide_String(Filename));
      aDocument.Linkpath   := To_XString(To_Wide_Wide_String(Linkpath));
      aDocument.Basename   := To_XString(To_Wide_Wide_String(Base_Name));

      if Ada.Directories.Exists(Filepath) then

         Read_Content(Filepath, aDocument.T);
         declare
            Assoc : association := Get(aDocument.T,"layout");
         begin
            if Assoc = Null_Association or Get(Assoc) = "" then
               Ada.Text_IO.Put_Line("File " & Filepath & " has no layout defined in frontmatter");
            else
               aDocument.Layout := To_XString(To_Wide_Wide_String(Get(Assoc)));
            end if;
         end;

      else
         Ada.Text_IO.Put_Line("File " & Filepath & " does not exist");
      end if;

      Insert (aDocument.T,Assoc ("basename",Base_Name));
      Insert (aDocument.T,Assoc ("filename",Filename));
      Insert (aDocument.T,Assoc ("targetname",Targetname));
      Insert (aDocument.T,Assoc ("linkpath",Linkpath));
      Insert (aDocument.T,Assoc ("filepath",Filepath));
      return aDocument;

   end Read;

end Generator.Frontmatter;
