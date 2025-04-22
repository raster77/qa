unit UFrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Forms, Controls, Graphics, Dialogs, DBCtrls, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, Menus, UId, UAnswer, UQuestion, UTag,
  UQuestionRepository, UAnswerRepository, UAppParameters;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    BitBtnAdd: TBitBtn;
    BitBtnRemove: TBitBtn;
    BitBtnSearch: TBitBtn;
    EdtSearch: TEdit;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    LbQuestions: TListBox;
    LbTags: TListBox;
    LbQuestionTags: TListBox;
    MemoAnswer: TMemo;
    MenuItemRestore: TMenuItem;
    Panel3: TPanel;
    Separator1: TMenuItem;
    MenuItemBackup: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemNew: TMenuItem;
    OpenDialogDb: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenuDb: TPopupMenu;
    SaveDialogDb: TSaveDialog;
    SbInfo: TStatusBar;
    sbAdvancedSearch: TSpeedButton;
    Splitter2: TSplitter;
    TbMain: TToolBar;
    TbSettings: TToolButton;
    tbAddEdit: TToolButton;
    TbExit: TToolButton;
    tbClear: TToolButton;
    tbDb: TToolButton;
    tbInfos: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    tbDelete: TToolButton;
    tbTags: TToolButton;
    procedure BitBtnAddClick(Sender: TObject);
    procedure BitBtnRemoveClick(Sender: TObject);
    procedure BitBtnSearchClick(Sender: TObject);
    procedure EdtSearchChange(Sender: TObject);
    procedure EdtSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LbQuestionsSelectionChange(Sender: TObject; User: boolean);
    procedure LbQuestionTagsDblClick(Sender: TObject);
    procedure LbTagsDblClick(Sender: TObject);
    procedure MenuItemBackupClick(Sender: TObject);
    procedure MenuItemNewClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemRestoreClick(Sender: TObject);
    procedure sbAdvancedSearchClick(Sender: TObject);
    procedure tbDbClick(Sender: TObject);
    procedure TbExitClick(Sender: TObject);
    procedure tbInfosClick(Sender: TObject);
    procedure TbSettingsClick(Sender: TObject);
    procedure tbAddEditClick(Sender: TObject);
    procedure tbClearClick(Sender: TObject);
    procedure SetToolbarButtonState;
    procedure tbTagsClick(Sender: TObject);
  private
    AdvancedSearch: Boolean;
    QuestionRepository: TQuestionRepository;
    AnswerRepository: TAnswerRepository;
    Tags: TTagList;
    AppParams: TAppParameters;
    procedure InitRepositories;
    procedure ClearAll;
    procedure ClearQuestions;
    procedure setSbInfo(const AQuestionsCount: Integer);
    procedure Search;
    procedure ExpandSearchPanel;
  public

  end;

var
  FrmMain: TFrmMain;

implementation

uses LCLType, UDataModuleQa, UFrmSettings, UFrmQAEdit, UFrmInfos, UFrmTags,
  UTagRepository, UListboxUtils;

{$R *.lfm}

{ TFrmMain }

procedure TFrmMain.SetToolbarButtonState;
begin

end;

procedure TFrmMain.tbTagsClick(Sender: TObject);
begin
  with TFrmTags.Create(nil, DataModuleQa.DBConn, DataModuleQa.DefaultTransaction) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TFrmMain.Search;
Var
  Questions: TQuestionList;
  i: Integer;
  IdTags: TIntegerList;
begin
  if (Trim(EdtSearch.Text) <> '') or (LbQuestionTags.Items.Count > 0) then
  begin
    IdTags:= TIntegerList.Create;
    for i:= 0 to LbQuestionTags.Count -1 do
    begin
      IdTags.Add(TId(LbQuestionTags.Items.Objects[i]).Value);
    end;
    ClearQuestions;
    MemoAnswer.Clear;
    Questions:= QuestionRepository.FtsSearch(Trim(EdtSearch.Text), IdTags);
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
    IdTags.Free;
  end;
  tbClear.Enabled:= LbQuestions.Items.Count - 1 > 0;
end;

procedure TFrmMain.EdtSearchChange(Sender: TObject);
begin
  BitBtnSearch.Enabled:= (Length(Trim(EdtSearch.Text)) >= AppParams.MinCharSize) or
                         (LbQuestionTags.Items.Count -1 > 0);
end;

procedure TFrmMain.BitBtnSearchClick(Sender: TObject);
begin
  Search;
end;

procedure TFrmMain.BitBtnAddClick(Sender: TObject);
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
    BitBtnSearch.Enabled:= True;
  end;
end;

procedure TFrmMain.BitBtnRemoveClick(Sender: TObject);
begin
  if LbQuestionTags.ItemIndex > -1 then
  begin
    TId(LbQuestionTags.Items.Objects[LbQuestionTags.ItemIndex]).Free;
    LbQuestionTags.Items.Delete(LbQuestionTags.ItemIndex);
  end;
  BitBtnSearch.Enabled:= (Length(Trim(EdtSearch.Text)) >= AppParams.MinCharSize) or
                         (LbQuestionTags.Items.Count -1 > 0);
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
Var
  TagList: TTagList;
  i: Integer;
begin
  if Assigned(QuestionRepository) then
    FreeAndNil(QuestionRepository);
  QuestionRepository:= TQuestionRepository.Create(DataModuleQa.DBConn,
                                                  DataModuleQa.DefaultTransaction);

  if Assigned(AnswerRepository) then
    FreeAndNil(AnswerRepository);
  AnswerRepository:= TAnswerRepository.Create(DataModuleQa.DBConn, DataModuleQa.DefaultTransaction);

  With TTagRepository.Create(DataModuleQa.DBConn, DataModuleQa.DefaultTransaction) do
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

procedure TFrmMain.ExpandSearchPanel;
begin
  if AdvancedSearch then
  begin
    Panel1.Height:= 150;
    sbAdvancedSearch.ImageIndex:= 15;
    sbAdvancedSearch.Hint:= 'Hide advanced search';
  end
  else
  begin
    Panel1.Height:= 26;
    sbAdvancedSearch.ImageIndex:= 16;
    sbAdvancedSearch.Hint:= 'Show advanced search';
  end;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
  DbPath: String;
begin
  AdvancedSearch:= False;
  DbPath:= '';
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
  ClearListboxWithId(LbTags);
  ClearListboxWithId(LbQuestionTags);
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
    Application.MessageBox('Failed to open database.', 'DB Error',
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

  tbDelete.Enabled:= LbQuestions.ItemIndex >= 0;
end;

procedure TFrmMain.LbQuestionTagsDblClick(Sender: TObject);
begin
  BitBtnRemoveClick(Sender);
end;

procedure TFrmMain.LbTagsDblClick(Sender: TObject);
begin
  BitBtnAddClick(sender);
end;

procedure TFrmMain.MenuItemBackupClick(Sender: TObject);
var
  Res: Boolean;
  Message: string;
  Flags: LongInt;
begin
  with TSaveDialog.Create(nil) do
  begin
    if Execute then
    begin
      Res:= DataModuleQa.Backup(FileName);
      if res then
      begin
        Message:= 'Backup done successfully';
        Flags:= MB_ICONINFORMATION + MB_OK;
      end
      else
      begin
        Message:= 'An error occured';
        Flags:= MB_ICONERROR + MB_OK;
      end;
      Application.MessageBox(Pchar(Message), 'Backup', Flags);
    end;
    Free;
  end;
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

procedure TFrmMain.MenuItemRestoreClick(Sender: TObject);
var
  Res: Boolean;
  Message: string;
  Flags: LongInt;
begin
  with TOpenDialog.Create(nil) do
  begin
    if Execute then
    begin
      Res:= DataModuleQa.Restore(FileName);
      if res then
      begin
        Message:= 'Restore done successfully';
        Flags:= MB_ICONINFORMATION + MB_OK;
      end
      else
      begin
        Message:= 'An error occured';
        Flags:= MB_ICONERROR + MB_OK;
      end;
      Application.MessageBox(Pchar(Message), 'Restore', Flags);
    end;
    Free;
  end;

end;

procedure TFrmMain.sbAdvancedSearchClick(Sender: TObject);
begin
  AdvancedSearch:= not AdvancedSearch;
  ExpandSearchPanel;
end;

procedure TFrmMain.tbDbClick(Sender: TObject);
var
  Point: TPoint;
begin
  Point:= tbDb.ClientToScreen(TPoint.Create(0, tbDb.Height));
  PopupMenuDb.PopUp(Point.X, Point.Y);
end;

procedure TFrmMain.TbExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmMain.tbInfosClick(Sender: TObject);
begin
  with TFrmInfos.Create(nil) do
  begin
    ShowModal;
    Free;
  end;
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
      self.AppParams.Load;
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
begin
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
  tbClear.Enabled:= false;
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

