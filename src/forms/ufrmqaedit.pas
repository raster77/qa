unit UFrmQAEdit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  SQLDB, UQuestionRepository, UQuestion, UAnswerRepository, UAnswer;

type

  { TFrmQAEdit }

  TFrmQAEdit = class(TForm)
    BitBtnRemove: TBitBtn;
    BitBtnAdd: TBitBtn;
    BitBtnClose: TBitBtn;
    BitBtnSave: TBitBtn;
    BitBtnCancel: TBitBtn;
    EdtQuestion: TEdit;
    gbTags: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    LbTags: TListBox;
    LbQuestionTags: TListBox;
    MemoAnswer: TMemo;
    procedure BitBtnRemoveClick(Sender: TObject);
    procedure BitBtnAddClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
    procedure BitBtnCloseClick(Sender: TObject);
    procedure BitBtnSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LbQuestionTagsDblClick(Sender: TObject);
    procedure LbTagsDblClick(Sender: TObject);
  private
    FSQLConnection: TSQLConnection;
    FSQLTransaction: TSQLTransaction;
    FIdQuestion: Integer;
    QuestionRepository: TQuestionRepository;
    AnswerRepository: TAnswerRepository;
    Question: TQuestion;
    Answer: TAnswer;
    TagsList: TIntegerList;
    TagsListToDelete: TIntegerList;
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

uses LCLType, UTagRepository, UTag, UId, UListboxUtils;

{$R *.lfm}

{ TFrmQAEdit }

procedure TFrmQAEdit.SetIdQuestion(AId: Integer);
var
  TagList: TTagList;
  i: Integer;
begin
  FrmQAEdit.Caption:= 'Update QA';
  Question:= QuestionRepository.Load(AId);
  FIdQuestion:= AId;
  EdtQuestion.Text:= Question.Question;
  Answer:= AnswerRepository.GetAnswerByQuestionId(AId);
  MemoAnswer.Lines.Add(Answer.Answer);
  TagList:= QuestionRepository.GetTags(AId);
  for i:= 0 to TagList.Count-1 do
  begin
    LbQuestionTags.AddItem(TagList[i].TagLabel, TId.create(TagList[i].Id));
  end;
  TagList.Free;
end;

procedure TFrmQAEdit.CreateQa;
var
  IdQ: Integer;
begin
  IdQ:= QuestionRepository.Add(EdtQuestion.Text);
  QuestionRepository.SetTags(IdQ, TagsList);
  AnswerRepository.Add(IdQ, MemoAnswer.Text);
  TagsListToDelete.Clear;
end;

procedure TFrmQAEdit.UpdateQa;
begin
  QuestionRepository.Update(Question.Id, EdtQuestion.Text);
  QuestionRepository.DeleteTags(Question.Id, TagsListToDelete);
  QuestionRepository.SetTags(Question.Id, TagsList);
  AnswerRepository.Update(Answer.Id, MemoAnswer.Text);
  TagsListToDelete.Clear;
end;

procedure TFrmQAEdit.BitBtnSaveClick(Sender: TObject);
var
  i: Integer;
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

  for i:= 0 to LbQuestionTags.Count -1 do
  begin
    TagsList.Add(TId(LbQuestionTags.Items.Objects[i]).Value);
  end;

  if FIdQuestion >= 0 then
    UpdateQa
  else
    CreateQa;

  FrmQAEdit.Caption:= 'Create QA';
  FIdQuestion:= -1;
  EdtQuestion.Text:= '';
  MemoAnswer.Clear;
  TagsList.Clear;
  TagsListToDelete.Clear;
  for i:= 0 to LbQuestionTags.Count -1 do
  begin
    TId(LbQuestionTags.Items.Objects[i]).Free;
  end;
  LbQuestionTags.Clear;
end;

procedure TFrmQAEdit.BitBtnCancelClick(Sender: TObject);
begin
  FrmQAEdit.Caption:= 'Create QA';
  FIdQuestion:= -1;
  EdtQuestion.Clear;
  MemoAnswer.Clear;
  ClearListboxWithId(LbQuestionTags);
end;

procedure TFrmQAEdit.BitBtnAddClick(Sender: TObject);
Var
  i: Integer;
  Found: Boolean;
begin
  Found:= False;
  for i:= 0 to LbQuestionTags.Count -1 do
  begin
    Found:= LbTags.Items[LbTags.ItemIndex] = LbQuestionTags.Items[i];
    if Found Then
      Exit;
  end;
  if not Found then
  begin
    LbQuestionTags.AddItem(LbTags.Items[LbTags.ItemIndex],
                           TId.create(TId(LbTags.Items.Objects[LbTags.ItemIndex]).Value));
  end;

end;

procedure TFrmQAEdit.BitBtnRemoveClick(Sender: TObject);
begin
  if LbQuestionTags.ItemIndex > -1 then
  begin
    TagsListToDelete.Add(TId(LbQuestionTags.Items.Objects[LbQuestionTags.ItemIndex]).Value);
    TId(LbQuestionTags.Items.Objects[LbQuestionTags.ItemIndex]).Free;
    LbQuestionTags.Items.Delete(LbQuestionTags.ItemIndex);
  end;
end;

procedure TFrmQAEdit.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmQAEdit.FormDestroy(Sender: TObject);
begin
  ClearListboxWithId(LbTags);
  ClearListboxWithId(LbQuestionTags);

  TagsList.Free;
  TagsListToDelete.Free;
  if Assigned(Question) then
    Question.Free;
  if Assigned(Answer) then
    Answer.Free;
  AnswerRepository.Free;
  QuestionRepository.Free;
end;

procedure TFrmQAEdit.LbQuestionTagsDblClick(Sender: TObject);
begin
  BitBtnRemoveClick(Sender);
end;

procedure TFrmQAEdit.LbTagsDblClick(Sender: TObject);
begin
  BitBtnAddClick(sender);
end;

constructor TFrmQAEdit.Create(AOwner: TComponent; AConnection: TSQLConnection; ATransaction: TSQLTransaction);
Var
  TagList: TTagList;
  i: Integer;
begin
  inherited Create(AOwner);
  TagsList:= TIntegerList.Create;
  TagsListToDelete:= TIntegerList.Create;
  FIdQuestion:= -1;
  FSQLConnection:= AConnection;
  FSQLTransaction:= ATransaction;
  QuestionRepository:= TQuestionRepository.Create(FSQLConnection, FSQLTransaction);
  AnswerRepository:= TAnswerRepository.Create(FSQLConnection, FSQLTransaction);
  With TTagRepository.Create(FSQLConnection, FSQLTransaction) do
  begin
    TagList:= GetTags;
    for i:= 0 to TagList.Count -1 do
    begin
      LbTags.AddItem(TagList[i].TagLabel, TId.create(TagList[i].Id));
    end;
    TagList.Free;
    Free;
  end;
end;

end.

