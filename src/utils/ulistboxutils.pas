unit UListboxUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, UId;

procedure ClearListboxWithId(AListbox: TListBox);

implementation

procedure ClearListboxWithId(AListbox: TListBox);
var
  i: Integer;
begin
  for i:= 0 to AListbox.Items.Count - 1 do
  begin
    TId(AListbox.Items.Objects[i]).Free;
  end;
  AListbox.Clear;

end;

end.

