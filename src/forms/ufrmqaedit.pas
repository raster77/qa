unit UFrmQAEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  SQLDB, UQuestionRepository, UQuestion, UAnswerRepository, UAnswer;

type

  { TFrmQAEdit }

  TFrmQAEdit = class(TForm)
    BitBtnClose: TBitBtn;
    BitBtnSave: TBitBtn;
    BitBtnCancel: TBitBtn;
    EdtQuestion: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MemoAnswer: TMemo;
    procedure BitBtnCancelClick(Sender: TObject);
    procedure BitBtnCloseClick(Sender: TObject);
    procedure BitBtnSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSQLConnection: TSQLConnection;
    FSQLTransaction: TSQLTransaction;
    FIdQuestion: Integer;
    QuestionRepository: TQuestionRepository;
    AnswerRepository: TAnswerRepository;
    Question: TQuestion;
    Answer: TAnswer;
    procedure CreateQa;
    procedure UpdateQa;
    procedure SetIdQuestion(AId: Integer);
  public
    constructor Create(AOwner: TComponent; AConnection: TSQLConnection; ATransaction: TSQLTransaction); reintroduce;
    property IdQuestion: Integer read FIdQuestion write SetIdQuestion;
  end;

var
  FrmQAEdit: TFrmQAEdit;

implementation

uses LCLType;

{$R *.lfm}

{ TFrmQAEdit }

procedure TFrmQAEdit.SetIdQuestion(AId: Integer);
begin
  FrmQAEdit.Caption:= 'Update QA';
  Question:= QuestionRepository.Load(AId);
  EdtQuestion.Text:= Question.Question;
  Answer:= AnswerRepository.GetAnswerByQuestionId(AId);
  MemoAnswer.Lines.Add(Answer.Answer);
end;

procedure TFrmQAEdit.CreateQa;
var
  IdQ: Integer;
begin
  IdQ:= QuestionRepository.Add(EdtQuestion.Text);
  AnswerRepository.Add(IdQ, MemoAnswer.Text);
end;

procedure TFrmQAEdit.UpdateQa;
begin
  QuestionRepository.Update(Question.Id, EdtQuestion.Text);
  AnswerRepository.Update(Answer.Id, MemoAnswer.Text);
end;

procedure TFrmQAEdit.BitBtnSaveClick(Sender: TObject);
begin
  if Trim(EdtQuestion.Text) = '' then
  begin
    Application.MessageBox('Question is mandatory', 'Error', MB_ICONERROR + MB_OK);
    EdtQuestion.SetFocus;
    Exit;
  end;

  if Trim(MemoAnswer.Text) = '' then
  begin
    Application.MessageBox('Answer is mandatory', 'Error', MB_ICONERROR + MB_OK);
    MemoAnswer.SetFocus;
    Exit;
  end;

  if FIdQuestion >= 0 then
    UpdateQa
  else
    CreateQa;

  FrmQAEdit.Caption:= 'Create QA';
  FIdQuestion:= -1;
  EdtQuestion.Text:= '';
  MemoAnswer.Clear;
end;

procedure TFrmQAEdit.BitBtnCancelClick(Sender: TObject);
begin
  FrmQAEdit.Caption:= 'Create QA';
  FIdQuestion:= -1;
  EdtQuestion.Clear;
  MemoAnswer.Clear;
end;

procedure TFrmQAEdit.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmQAEdit.FormDestroy(Sender: TObject);
begin
  if Assigned(Question) then
    Question.Free;
  if Assigned(Answer) then
    Answer.Free;
  AnswerRepository.Free;
  QuestionRepository.Free;
end;

constructor TFrmQAEdit.Create(AOwner: TComponent; AConnection: TSQLConnection; ATransaction: TSQLTransaction);
begin
  inherited Create(AOwner);
  FIdQuestion:= -1;
  FSQLConnection:= AConnection;
  FSQLTransaction:= ATransaction;
  QuestionRepository:= TQuestionRepository.Create(FSQLConnection, FSQLTransaction);
  AnswerRepository:= TAnswerRepository.Create(FSQLConnection, FSQLTransaction);
end;

end.

