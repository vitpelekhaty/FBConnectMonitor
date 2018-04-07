program FBConnectMonitor;

uses
  EMemLeaks,
  EResLeaks,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  EDebugExports,
  EDebugJCL,
  EAppVCL,
  ExceptionLog7,
  WinApi.Windows,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Controls,
  AppInfo in 'units\AppInfo.pas',
  DateTimeHelper in 'units\DateTimeHelper.pas',
  Firebird.Tools in 'units\Firebird.Tools.pas',
  GTTimer in 'units\GTTimer.pas',
  SpecialImageLists in 'units\SpecialImageLists.pas',
  VerInfo in 'units\VerInfo.pas',
  Windows.Info in 'units\Windows.Info.pas',
  Firebird.Monitors in 'units\Firebird.Monitors.pas',
  frmMain in 'forms\frmMain.pas' {MainForm},
  frmDatabaseEditor in 'forms\frmDatabaseEditor.pas' {DatabaseEditor},
  frmOptions in 'forms\frmOptions.pas' {OptionsForm},
  frmAbout in 'forms\frmAbout.pas' {AboutForm},
  frmAlert in 'forms\frmAlert.pas' {AlertForm};

{$R *.res}
{$R handpoint.res}

begin
  Randomize;
  Screen.Cursors[crHandpoint] := LoadCursor(hInstance, 'MODERNHANDPOINT');

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'FBConnectMonitor';
  TStyleManager.TrySetStyle('Carbon');
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
