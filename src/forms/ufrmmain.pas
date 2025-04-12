unit UFrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, DBCtrls, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, Menus, UId, UAnswer, UQuestion, UTag,
  UQuestionRepository, UAnswerRepository, UAppParameters, UTagRepository;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    BitBtnSearch: TBitBtn;
    EdtSearch: TEdit;
    ImageList1: TImageList;
    LbQuestions: TListBox;
    MemoAnswer: TMemo;
    MenuItemOpen: TMenuItem;
    MenuItemNew: TMenuItem;
    OpenDialogDb: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenuDb: TPopupMenu;
    SaveDialogDb: TSaveDialog;
    SbInfo: TStatusBar;
    Splitter2: TSplitter;
    TbMain: TToolBar;
    TbSettings: TToolButton;
    tbAddEdit: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    TbExit: TToolButton;
    tbClear: TToolButton;
    tbDb: TToolButton;
    ToolButton5: TToolButton;
    procedure BitBtnSearchClick(Sender: TObject);
    procedure EdtSearchChange(Sender: TObject);
    procedure EdtSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LbQuestionsSelectionChange(Sender: TObject; User: boolean);
    procedure MenuItemNewClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure TbExitClick(Sender: TObject);
    procedure TbSettingsClick(Sender: TObject);
    procedure tbAddEditClick(Sender: TObject);
    procedure tbClearClick(Sender: TObject);
  private
    QuestionRepository: TQuestionRepository;
    AnswerRepository: TAnswerRepository;
    Tags: TTagList;
    AppParams: TAppParameters;
    procedure InitRepositories;
    procedure ClearAll;
    procedure ClearQuestions;
    procedure setSbInfo(const AQuestionsCount: Integer);
    procedure Search;
  public

  end;

var
  FrmMain: TFrmMain;

implementation

uses LCLType, UDataModuleQa, UFrmSettings, UFrmQAEdit;

{$R *.lfm}

{ TFrmMain }

procedure TFrmMain.Search;
Var
  Questions: TQuestionList;
  i: Integer;
begin
  if Trim(EdtSearch.Text) <> '' then
  begin
    ClearQuestions;
    MemoAnswer.Clear;
    Questions:= QuestionRepository.FtsSearch(Trim(EdtSearch.Text));
    LbQuestions.Items.BeginUpdate;
    setSbInfo(Questions.Count);
    for i:= 0 to Questions.Count -1 do
    begin
      LbQuestions.AddItem(Questions[i].Question, TId.create(Questions[i].Id));
    end;
    if Questions.Count = 1 then
      LbQuestions.ItemIndex:= 0;
    LbQuestions.Items.EndUpdate;
    Questions.Free;
  end;
end;

procedure TFrmMain.EdtSearchChange(Sender: TObject);
begin
  BitBtnSearch.Enabled:= Length(Trim(EdtSearch.Text)) >= AppParams.MinCharSize;
end;

procedure TFrmMain.BitBtnSearchClick(Sender: TObject);
begin
  Search;
end;

procedure TFrmMain.EdtSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (BitBtnSearch.Enabled) then
  begin
    Search;
    key:= 0;
  end;
end;

procedure TFrmMain.InitRepositories;
begin
  if Assigned(QuestionRepository) then
    FreeAndNil(QuestionRepository);
  QuestionRepository:= TQuestionRepository.Create(DataModuleQa.DBConn,
                                                  DataModuleQa.DefaultTransaction);

  if Assigned(AnswerRepository) then
    FreeAndNil(AnswerRepository);
  AnswerRepository:= TAnswerRepository.Create(DataModuleQa.DBConn, DataModuleQa.DefaultTransaction);
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
  DbPath: String;
begin
  AppParams:= TAppParameters.Create;
  with AppParams do
  begin
    if Load then
    begin
      DbPath:= Database;
      self.Position:= poDesigned;
      self.Left:= Position.X;
      self.Top:= Position.Y;
      self.Width:= Size.X;
      self.Height:= Size.Y;
    end;
  end;

  //'C:\Users\DIJOUXT\Dev\fpc\qa\db\qa.db3'
  if DataModuleQa.Open(DbPath) then
  begin
    InitRepositories;
  end;

  EdtSearch.Text:= '';
  MemoAnswer.Clear;
  ClearQuestions;
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
  begin
    With AppParams do
    begin
      Position:= TPoint.Create(Self.Left, Self.Top);
      Size:= TPoint.Create(Self.Width, Self.Height);
      Save;
    end;
  end;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  ClearQuestions;
  Tags.Free;
  if Assigned(QuestionRepository) then
    QuestionRepository.Free;
  if Assigned(AnswerRepository) then
    AnswerRepository.Free;
  if Assigned(AppParams) then
    AppParams.Free;
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  if not DataModuleQa.DBConn.Connected then
    Application. MessageBox('Failed to open database.', 'DB Error',
                            MB_ICONERROR + MB_OK);
end;

procedure TFrmMain.LbQuestionsSelectionChange(Sender: TObject; User: boolean);
Var
  Answer: TAnswer;
begin
  MemoAnswer.Clear;
  Answer:= AnswerRepository.GetAnswerByQuestionId(TId(LbQuestions.Items.Objects[LbQuestions.ItemIndex]).Value);

  MemoAnswer.Lines.BeginUpdate;
  MemoAnswer.Lines.Add(Answer.Answer);
  MemoAnswer.Lines.EndUpdate;
  Answer.Free;
  tbAddEdit.ImageIndex:= 5;
  tbAddEdit.Hint:= 'Edit QA';
end;

procedure TFrmMain.MenuItemNewClick(Sender: TObject);
begin
  if SaveDialogDb.Execute then
  begin
    DataModuleQa.CreateDb(SaveDialogDb.FileName);
    AppParams.Database:= SaveDialogDb.FileName;
    AppParams.Save;
  end;
end;

procedure TFrmMain.MenuItemOpenClick(Sender: TObject);
begin
  if OpenDialogDb.Execute then
  begin
    DataModuleQa.Open(OpenDialogDb.FileName);
    AppParams.Database:= SaveDialogDb.FileName;
    AppParams.Save;
  end;
end;

procedure TFrmMain.TbExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmMain.TbSettingsClick(Sender: TObject);
var
  mrResult: Integer;
begin
  with TFrmSettings.Create(self) do
  begin
    AppParams:= self.AppParams;
    mrResult:= ShowModal;
    if mrResult = mrOk then
    begin
      DataModuleQa.DBConn.Close(True);
      if DataModuleQa.Open(self.AppParams.Database) then
      begin
        InitRepositories;
      end;

      EdtSearch.Text:= '';
      MemoAnswer.Clear;
      ClearQuestions;
    end;
    Free;
  end;
end;

procedure TFrmMain.tbAddEditClick(Sender: TObject);
var
  id: Integer;
begin
  id:= -1;
  FrmQAEdit:= TFrmQAEdit.Create(nil, DataModuleQa.DBConn, DataModuleQa.DefaultTransaction);
  if LbQuestions.ItemIndex >= 0 then
    FrmQAEdit.IdQuestion:= TId(LbQuestions.Items.Objects[LbQuestions.ItemIndex]).Value;
  FrmQAEdit.ShowModal;
  FrmQAEdit.Free;
end;

procedure TFrmMain.tbClearClick(Sender: TObject);
begin
  ClearAll;
  tbAddEdit.ImageIndex:= 6;
  tbAddEdit.Hint:= 'Add QA';
end;

procedure TFrmMain.setSbInfo(const AQuestionsCount: Integer);
Var
  QuestionStr: String;
begin
  QuestionStr:= 'question';
  if AQuestionsCount > 1 then
    QuestionStr:= QuestionStr + 's';
  SbInfo.SimpleText:= IntToStr(AQuestionsCount) + ' ' + QuestionStr + ' found';
end;

procedure TFrmMain.ClearQuestions;
var
  i: Integer;
begin
  for i:= 0 to LbQuestions.Items.Count - 1 do
  begin
    TId(LbQuestions.Items.Objects[i]).Free;
  end;
  LbQuestions.Clear;
end;

procedure TFrmMain.ClearAll;
begin
  ClearQuestions;
  EdtSearch.Clear;
  MemoAnswer.Clear;
end;

end.

