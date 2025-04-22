unit UFrmInfos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  UDataModuleQa;

type

  { TFrmInfos }

  TFrmInfos = class(TForm)
    BitBtnOk: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblTotalAnswers: TLabel;
    lblTotalQuestions: TLabel;
    lblDbSize: TLabel;
    lblDbPath: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FrmInfos: TFrmInfos;

implementation

uses UQuestionRepository, UAnswerRepository;

{$R *.lfm}

{ TFrmInfos }

procedure TFrmInfos.FormCreate(Sender: TObject);
Var
  FileInfo: TSearchRec;
  QuestionRepository: TQuestionRepository;
  AnswerRepository: TAnswerRepository;
  Total: Integer;
  TempStr: string;
begin
  lblDbSize.Caption:= '-';
  Total:= 0;
  TempStr:= ' question';

  lblDbPath.Caption:= DataModuleQa.DBConn.DatabaseName;
  if FileExists(DataModuleQa.DBConn.DatabaseName) then
  begin
    If FindFirst(DataModuleQa.DBConn.DatabaseName, 0, FileInfo)= 0 then
      lblDbSize.Caption:= IntToStr(FileInfo.Size DIV 1024) + ' kb';
    FindClose(FileInfo);
  end;
  QuestionRepository:= TQuestionRepository.Create(DataModuleQa.DBConn, DataModuleQa.DefaultTransaction);
  Total:= QuestionRepository.Count;
  if Total > 1 then
    TempStr:= TempStr + 's';
  lblTotalQuestions.Caption:= IntToStr(Total) + TempStr;
  QuestionRepository.Free;

  Total:= 0;
  TempStr:= ' answer';
  AnswerRepository:= TAnswerRepository.Create(DataModuleQa.DBConn, DataModuleQa.DefaultTransaction);
  Total:= AnswerRepository.Count;
  if Total > 1 then
    TempStr:= TempStr + 's';
  lblTotalAnswers.Caption:= IntToStr(Total) + TempStr;
  AnswerRepository.Free;
end;

end.

