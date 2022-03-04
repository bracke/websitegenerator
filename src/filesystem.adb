with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;
with Templates_Parser.Utils;
with Ada.Directories.Hierarchical_File_Names;

package body Filesystem is

   package IO renames Ada.Text_IO;
   package HF renames Ada.Directories.Hierarchical_File_Names;

   function Read_Directory (Path : String; Recursive : Boolean) return
      AAA.Strings.Vector is

      Description : AAA.Strings.Vector := AAA.Strings.Empty_Vector;

      Filter : constant Filter_Type :=
      (
         Ordinary_File => True,
         Special_File  => False,
         Directory     => True
      );

      procedure Item (Search_Item : Directory_Entry_Type);
      procedure Item (Search_Item : Directory_Entry_Type) is

         Path    : constant String :=
            Full_Name (Directory_Entry => Search_Item);

         Simple  : constant String := Simple_Name (Path);
      begin
         if Filesystem.Is_Valid_File (Path) then
            if Kind (Directory_Entry => Search_Item) = Directory then

               Append (Description, Simple);

               if Recursive then
                  Append (Description, Read_Directory (Path, Recursive));
               end if;
            else
               Append (Description, Simple);
            end if;
         end if;
      end Item;
   begin
      Search
         (Directory => Path, Pattern => "", Filter => Filter,
         Process   => Item'Access);

      return Description;

   end Read_Directory;

   function Count_Files (Path : String) return Natural is

      Count    : Natural := 0;
      Filter   : constant Filter_Type :=
      (
         Ordinary_File => True,
         Special_File  => False,
         Directory     => True
      );
      procedure Item (Search_Item : Directory_Entry_Type);

      procedure Item (Search_Item : Directory_Entry_Type) is

         Path : constant String := Full_Name (Directory_Entry => Search_Item);
      begin
         if Filesystem.Is_Valid_File (Path) then
            if Kind (Directory_Entry => Search_Item) = Directory then
               Count := Count + Count_Files (Path);
            else
               Count := Count + 1;
            end if;
         end if;
      end Item;
   begin
      Search
         (Directory => Path,
            Pattern => "",
            Filter => Filter,
            Process => Item'Access);

      return Count;
   end Count_Files;

   function Is_Valid_File (Name : String) return Boolean is
      Simple : constant String := Simple_Name (Name);
   begin
      return Simple /= ".." and then Simple /= ".";
   end Is_Valid_File;

   function Get_Executable_Path return String is
   begin
      return Templates_Parser.Utils.Get_Program_Directory;
   end Get_Executable_Path;

   function Is_Subfolder (Parent : String; Sub : String) return Boolean is

         Parent_Full : constant String := Full_Name (Parent);
         Sub_Full    : constant String := Full_Name (Sub);

   begin
         if Parent_Full = Sub_Full then
            return True;
         end if;

         if HF.Is_Root_Directory_Name (Sub_Full) then
            return False;
         end if;

         return Is_Subfolder (Parent, Containing_Directory (Sub_Full));

   end Is_Subfolder;

end Filesystem;