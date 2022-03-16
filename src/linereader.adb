with Ada.Text_IO;

package body Linereader is

   SL : constant Natural := Separator_Sequence'Length;

   function End_Of_Input(Self : Reader) return Boolean is
   begin
      return Self.End_Of_Input;
   end End_Of_Input;

   procedure Restore (Self : in out Reader) is
   begin
      Self.Position := Self.Backup;
   end Restore;

   procedure Backup (Self : in out Reader) is
   begin
      Self.Backup := Self.Position;
   end Backup;

   function Get_Remainder (Self : in out Reader) return String is
      Result : constant String := Self.Source(Self.Position .. Self.Last);
      subtype NiceString is String (1 .. Self.Last - Self.Position + 1);
   begin
      Self.End_Of_Input := True;
      return NiceString(Result);
   end Get_Remainder;

   function Separator_Position(Self: Reader) return Natural is
      pragma Inline(Separator_Position);
      K : Natural := Self.Position;
   begin
      while Self.Source(K) /= Separator_Sequence(Separator_Sequence'First) loop
         K := K + 1;
         exit when K > Self.Len;
      end loop;
      return K;
   end Separator_Position;

   function Get_Line(Self: in out Reader) return String is
      Next_Separator : Natural;
   begin
      if Self.End_Of_Input then
         raise End_Of_Input_Exception;
      end if;
      Next_Separator := Separator_Position(Self);
      if Next_Separator > Self.Last then
         declare
            Result : constant String := Self.Source(Self.Position .. Self.Last);
            subtype NiceString is String (1 .. Self.Last - Self.Position);
         begin
            Self.End_Of_Input := True;
            return NiceString(Result);
         end;
      else
         declare
            Result : String renames Self.Source(Self.Position .. Next_Separator);
            subtype NiceString is String (1 .. Next_Separator - Self.Position);
         begin
            Self.Position := Next_Separator + SL;
            return NiceString(Result);
         end;

      end if;

   end Get_Line;

end Linereader;