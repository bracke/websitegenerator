with AAA.Strings; use AAA.Strings;

package Filesystem is

  function Read_Directory(Path: String; Recursive: Boolean) return AAA.Strings.Vector;

  function Count_Files(Path : String) return Natural;

  function Is_Valid_File(Name : String) return Boolean;

  function Get_Executable_Path return String;

private

end Filesystem;