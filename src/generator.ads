with GNATCOLL.Strings_Impl;
with Ada.Wide_Wide_Characters.Handling, Ada.Characters;
use Ada.Wide_Wide_Characters.Handling;
with Ada.Characters.Conversions;
with Ada.Containers.Doubly_Linked_Lists;
with GNATCOLL.Config;
with Templates_Parser;
with Ada.Strings.Maps;

package Generator is

   use Templates_Parser;

   package aString is new GNATCOLL.Strings_Impl.Strings
     (SSize            => GNATCOLL.Strings_Impl.Optimal_String_Size,
      Character_Type   => Wide_Wide_Character,
      Character_String => Wide_Wide_String);

   use Generator.aString;

   type Document is record
      Filepath    : XString := Null_XString;
      Targetpath  : XString := Null_XString;
      Filename    : XString := Null_XString;
      Basename    : XString := Null_XString;
      Linkpath    : XString := Null_XString;
      Layout      : XString := Null_XString;
      Content     : XString := Null_XString;
      T : Translate_Set;
   end record;

   package Document_Container is new Ada.Containers.Doubly_Linked_Lists
     (Document);
   use Document_Container;

   function "<" (Left, Right : Document) return Boolean;

   package Document_List_Sorting is new Document_Container.Generic_Sorting;
   use Document_List_Sorting;

   Slash : constant Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps.To_Set ("/");

   Debug : Boolean := False;

   procedure Process_File
     (List        : out Document_Container.List;
      Filepath    : String;
      Targetpath  : String;
      Linkpath    : String);

   procedure Process_Directory
     (List              : out Document_Container.List;
      Source_Directory  : String;
      Target_Directory  : String;
      LinkpathIn        : String);

   procedure Process_Documents (
      List           : Document_Container.List;
      Set            : Translate_Set;
      Layoutfolder   : String;
      Source_Directory : String;
      Targetpath     : String);

   function Create_Vector (
      List : Document_Container.List;
      Prefix : String) return Translate_Set;

   function Get_Nav_Links (
      Document : Cursor;
      List : Document_Container.List) return Translate_Set;

   function Read_From_Set (Set : Translate_Set; Token : String) return String;

   procedure Start (Source_Directory : String; Target_Directory : String);

private

end Generator;