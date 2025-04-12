unit UTag;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  TTag = class
  private
    FId: Integer;
    FTagLabel: String;
  public
    constructor Create(AId: Integer; ATagLabel: String);
    property Id: Integer read FId write FId;
    property TagLabel: string read FTagLabel write FTagLabel;
  end;

  TTagList = specialize TFPGObjectList<TTag>;

implementation

constructor TTag.Create(AId: Integer; ATagLabel: String);
begin
  FId:= AId;
  FTagLabel:= ATagLabel;
end;

end.

