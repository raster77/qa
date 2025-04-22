unit UFrmTags;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, SQLDB, UTagRepository;

type

  { TFrmTags }

  TFrmTags = class(TForm)
    BitBtnAdd: TBitBtn;
    BitBtnClose: TBitBtn;
    BitBtnSave: TBitBtn;
    BitBtnDelete: TBitBtn;
    EditTag: TEdit;
    LabelTag: TLabel;
    ListBoxTags: TListBox;
    PanelMain: TPanel;
    procedure BitBtnAddClick(Sender: TObject);
    procedure BitBtnSaveClick(Sender: TObject);
    procedure BitBtnDeleteClick(Sender: TObject);
    procedure BitBtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxTagsSelectionChange(Sender: TObject; User: boolean);
  private
    TagId: Integer;
    FSQLConnection: TSQLConnection;
    FSQLTransaction: TSQLTransaction;
    TagRepository: TTagRepository;
    procedure GetTags;
  public
    constructor Create(AOwner: TComponent; AConnection: TSQLConnection; ATransaction: TSQLTransaction); reintroduce;

  end;

var
  FrmTags: TFrmTags;

implementation

uses LCLType, UTag, UId, UListboxUtils;

{$R *.lfm}

{ TFrmTags }

procedure TFrmTags.GetTags;
Var
  TagList: TTagList;
  i: Integer;
begin
  ClearListboxWithId(ListBoxTags);
  TagList:= TagRepository.GetTags;
  for i:= 0 to TagList.Count -1 do
  begin
    ListBoxTags.AddItem(TagList[i].TagLabel, TId.create(TagList[i].Id));
  end;
  TagList.Free;
end;

constructor TFrmTags.Create(AOwner: TComponent; AConnection: TSQLConnection; ATransaction: TSQLTransaction);
begin
  inherited Create(AOwner);
  TagId:= -1;
  FSQLConnection:= AConnection;
  FSQLTransaction:= ATransaction;
  TagRepository:= TTagRepository.Create(FSQLConnection, FSQLTransaction);
end;

procedure TFrmTags.FormCreate(Sender: TObject);
begin
  GetTags;
end;

procedure TFrmTags.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmTags.BitBtnAddClick(Sender: TObject);
begin
  TagId:= -1;
  EditTag.Clear;
  EditTag.SetFocus;
end;

procedure TFrmTags.BitBtnSaveClick(Sender: TObject);
Var
  CreatedTag: TTag;
begin
  if TagId >= 0 then
  begin
    TagRepository.Update(TagId, EditTag.Text);
  end
  else
  begin
    CreatedTag:= TagRepository.Add(EditTag.Text);
    CreatedTag.Free;
  end;
  TagId:= -1;
  GetTags
end;

procedure TFrmTags.BitBtnDeleteClick(Sender: TObject);
Var
  Msg: string;
begin

  if TagId >= 0 then
  begin
    Msg:= 'Delete tag "' + ListBoxTags.Items[ListBoxTags.ItemIndex] + '" ?';
    if Application.MessageBox(PChar(Msg), 'Delete tag', MB_ICONQUESTION + MB_YESNO) = IDYES then
    begin
      TagRepository.Delete(TagId);
      TagId:= -1;
      GetTags;
    end;
  end;
end;

procedure TFrmTags.FormDestroy(Sender: TObject);
begin
  ClearListboxWithId(ListBoxTags);
  TagRepository.Free;
end;

procedure TFrmTags.ListBoxTagsSelectionChange(Sender: TObject; User: boolean);
begin
  if ListBoxTags.ItemIndex >= 0 then
  begin
    TagId:= TId(ListBoxTags.Items.Objects[ListBoxTags.ItemIndex]).Value;
    EditTag.Text:= ListBoxTags.Items[ListBoxTags.ItemIndex];
  end;
end;

end.

