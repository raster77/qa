unit UQuestion;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  TQuestion = class
  private
    FId: Integer;
    FQuestion: string;
  public
    constructor Create(const AId: Integer; const AQuestion: String);
    property Id: Integer read FId;
    property Question: string read FQuestion;
  end;

  TQuestionList = specialize TFPGObjectList<TQuestion>;

implementation

constructor TQuestion.Create(const AId: Integer; const AQuestion: String);
begin
  FId:= AId;
  FQuestion:= AQuestion;
end;

end.

