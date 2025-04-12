unit UAppParameters;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

Type
  TAppParameters = class
    private
      FPosition: TPoint;
      FSize: TPoint;
      FTheme: string;
      FFile: string;
      FDatabase: string;
      FMinCharSize: Integer;
    public
      constructor Create;
      procedure Save;
      function Load: Boolean;
      Property Position: TPoint read FPosition write FPosition;
      Property Size: TPoint read FSize write FSize;
      Property Theme: string read FTheme write FTheme;
      Property Database: string read FDatabase write FDatabase;
      Property MinCharSize: Integer read FMinCharSize write FMinCharSize;
  end;

implementation

constructor TAppParameters.Create;
begin
  if not DirectoryExists(GetAppConfigDir(false)) then
    CreateDir(GetAppConfigDir(false));
  FFile:= GetAppConfigDir(false) + 'Params';
  FTheme:= '';
  FDatabase:= '';
  FMinCharSize:= 3;
end;

function TAppParameters.Load: Boolean;
begin
  Result:= False;
  if FileExists(FFile) then
  begin
    with TStringList.Create do
    begin
      LoadFromFile(FFile);
      FPosition:= TPoint.Create(StrToInt(Values['x']), StrToInt(Values['y']));
      FSize:= TPoint.Create(StrToInt(Values['w']), StrToInt(Values['h']));
      FTheme:= Values['theme'];
      FDatabase:= Values['db'];
      if Values['minCharSize'] <> '' then
        FMinCharSize:= StrToInt(Values['minCharSize']);
      Free;
    end;
    Result:= True;
  end;
end;

procedure TAppParameters.Save;
begin
  with TStringList.Create do
  begin
    Add('x=' + IntToStr(FPosition.X));
    Add('y=' + IntToStr(FPosition.Y));
    Add('w=' + IntToStr(FSize.X));
    Add('h=' + IntToStr(FSize.Y));
    Add('theme=' + FTheme);
    Add('db=' + FDatabase);
    Add('minCharSize=' + IntToStr(FMinCharSize));
    SaveToFile(FFile);
    Free;
  end;
end;

end.

