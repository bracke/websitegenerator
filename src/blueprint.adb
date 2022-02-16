with Ada.Text_IO;
with Ada.Directories;
with Ada.Command_Line;
with Templates_Parser;
with CLIC.TTY;
with Filesystem;
with Commands;

package body Blueprint is

  package IO renames Ada.Text_IO;
  package TT renames CLIC.TTY;
  package FS renames Filesystem;

  use Ada.Directories;

  Errors : Boolean := false;

  function Get_Blueprint_Folder return String is
    Executable_Location : String := FS.Get_Executable_Path;
    Root : String :=
     Containing_Directory(Containing_Directory (Executable_Location));
    Blueprint_Folder : String := Compose (Root, "blueprints");
  begin
    return Blueprint_Folder;
  end Get_Blueprint_Folder;

  procedure Process_File
   (Target : String; Search_Item : in Directory_Entry_Type; Todo: Action) is

    Source_File       : String := Full_Name (Directory_Entry => Search_Item);
    Processed_Content : String :=
     Templates_Parser.Parse (Source_File, Commands.Translations);
    File_Handle       : IO.File_Type;
  begin
    IO.Put_Line (TT.Emph ("Create") & " " & Target);
    if Exists(Target) then
      Errors := true;
      IO.Put_Line
       (TT.Error ("A file allready exists at ") & " " & TT.Bold (Target) & " " & TT.Warn("Ignored"));
    else
      if Todo = Write then
        IO.Create (File_Handle, IO.Out_File, Target);
        IO.Put (File_Handle, Processed_Content);
        IO.Close (File_Handle);
      end if;
    end if;
  end Process_File;

  procedure Iterate(Blueprint_Folder: String; Path: String; Todo: Action) is

    Filter : constant Filter_Type :=
    (
      Ordinary_File => True,
      Special_File  => False,
      Directory     => True
    );

    procedure Item (Search_Item : in Directory_Entry_Type) is
      Name             : String := Simple_Name (Directory_Entry => Search_Item);
      Processed_Name   : String := Templates_Parser.Translate (Name, Commands.Translations);
      Blueprint_Folder : String := Full_Name (Directory_Entry => Search_Item);
      Target           : String := Compose (Path, Processed_Name);
    begin
      if Filesystem.Is_Valid_File (Name) then
        case Todo is
        when Write =>
          if Kind (Directory_Entry => Search_Item) = Directory then
            IO.Put_Line(TT.Emph ("Create") & " " & Target);
            if Exists(Target) then
              if Kind (Directory_Entry => Search_Item) = ORDINARY_FILE then
                IO.Put_Line
                (TT.Error ("A file allready exists at ") & " " &
                  TT.Bold (Target) & " " & TT.Warn ("Ignored"));
                Errors := True;
              else
                Iterate (Blueprint_Folder, Target, Todo);
              end if;
            else
              Create_Directory (Target);
              Iterate (Blueprint_Folder, Target, Todo);
            end if;
          else
            Process_File (Target, Search_Item, Todo);
          end if;

        when Delete =>
          if Kind (Directory_Entry => Search_Item) = Directory then
            if Exists(Target) then
              Iterate (Blueprint_Folder, Target, Todo);

              if FS.Count_Files(Target) = 0 then
                IO.Put_Line(TT.Emph ("Delete") & " " & Target);
                Delete_Tree(Target);
              end if;

            end if;

          elsif Kind (Directory_Entry => Search_Item) = ORDINARY_FILE then
            IO.Put_Line(TT.Emph ("Delete") & " " & Target);
            Delete_File (Target);
          end if;

        when DryRun =>
          if Kind (Directory_Entry => Search_Item) = Directory then
            IO.Put_Line(TT.Emph ("Create") & " " & Target);
            if Exists(Target) then
              if Kind (Directory_Entry => Search_Item) = ORDINARY_FILE then
                IO.Put_Line
                (TT.Error ("A file allready exists at ") & " " &
                  TT.Bold (Target) & " " & TT.Warn ("Ignored"));
                Errors := True;
              else
                Iterate (Blueprint_Folder, Target, Todo);
              end if;
            else
              Iterate (Blueprint_Folder, Target, Todo);
            end if;
          else
            Process_File (Target, Search_Item, Todo);
          end if;
        end case;
      end if;
    end Item;
  begin
    Search
        (Directory => Blueprint_Folder, Pattern => "", Filter => Filter,
          Process   => Item'Access);
  end Iterate;

end Blueprint;