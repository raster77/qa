unit UId;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  TId = class
  private
    FValue: Integer;
  public
    constructor Create(const AValue: Integer);
    property Value: Integer read FValue;
  end;

implementation

constructor TId.Create(const AValue: Integer);
begin
  FValue:= AValue;
end;

end.

