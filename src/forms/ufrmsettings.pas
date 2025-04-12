unit UFrmSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  SpinEx, UAppParameters;

type

  { TFrmSettings }

  TFrmSettings = class(TForm)
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    Button1: TButton;
    ChBxDarkTheme: TCheckBox;
    EdtDbPath: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialogDb: TOpenDialog;
    SpinEditExMinCharsSearch: TSpinEditEx;
    procedure BitBtnOkClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FAppParams: TAppParameters;
  public
    Property AppParams: TAppParameters read FAppParams write FAppParams;
  end;

var
  FrmSettings: TFrmSettings;

implementation

{$R *.lfm}

{ TFrmSettings }

procedure TFrmSettings.Button1Click(Sender: TObject);
begin
  if OpenDialogDb.Execute then
  begin
    EdtDbPath.Text:= OpenDialogDb.FileName;
  end;
end;

procedure TFrmSettings.BitBtnOkClick(Sender: TObject);
begin
  with FAppParams do
  begin
    Database:= EdtDbPath.Text;
    if ChBxDarkTheme.Checked then
      Theme:= 'Dark'
    else
      Theme:= 'Light';
    MinCharSize:= SpinEditExMinCharsSearch.Value;
    Save;
  end;
  Close;
end;

procedure TFrmSettings.FormCreate(Sender: TObject);
begin

end;

procedure TFrmSettings.FormShow(Sender: TObject);
begin
  if Assigned(FAppParams) then
  begin
    EdtDbPath.Text:= FAppParams.Database;
    ChBxDarkTheme.Checked:= FAppParams.Theme = 'Dark';
    SpinEditExMinCharsSearch.Value:= FAppParams.MinCharSize;
  end;
end;

end.

