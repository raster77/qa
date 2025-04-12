program qa;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  SysUtils,
  LResources,// for including lrs themes
  Forms, lazcontrols, uDarkStyleParams, uMetaDarkStyle, uDarkStyleSchemes,
  uDarkStyleSchemesLoader, uDarkStyleSchemesAdditional, UDataModuleQa,
  UAppParameters, UFrmMain;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;

  with TAppParameters.Create do
  begin
    Load;
    if Theme = 'Dark' then
    begin
      PreferredAppMode:= pamAllowDark;
    end
    else
    begin
      PreferredAppMode:= pamForceLight;
    end;
    uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
    Free;
  end;

  Application.Initialize;
  Application.CreateForm(TDataModuleQa, DataModuleQa);
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.

