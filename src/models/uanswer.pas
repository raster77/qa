unit UAnswer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  TAnswer = class
  private
    FId: Integer;
    FAnswer: string;
  public
    constructor Create(const AId: Integer; const AAnswer: String);
    property Id: Integer read FId;
    property Answer: string read FAnswer;
  end;

  TAnswerList = specialize TFPGObjectList<TAnswer>;

implementation

constructor TAnswer.Create(const AId: Integer; const AAnswer: String);
begin
  FId:= AId;
  FAnswer:= AAnswer;
end;

end.

