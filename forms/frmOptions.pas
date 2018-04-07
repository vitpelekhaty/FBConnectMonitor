///	<summary>
///	  ������ �������� �������� ������ ���� ��������� ���������� �������� �
///	  �����, ������������ ����������
///	</summary>
unit frmOptions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ImgList;

type
  ///	<summary>
  ///	  �������� ��� ������� ������ "�������" �������� ���� ���������
  ///	</summary>
  TMainFormCloseAction = (
    ///	<summary>
    ///	  ��������� ������ ��������� (������� ������� ���� ���������)
    ///	</summary>
    fcaClose,

    ///	<summary>
    ///	  �������� ������� ���� ��������� � ��������� ����
    ///	</summary>
    fcaHideToTray
  );

type
  ///	<summary>
  ///	  ����� ���� ��������� ���������� ��������
  ///	</summary>
  TOptionsForm = class(TForm)
    bClose: TButton;
    bSave: TButton;
    leInterval: TLabeledEdit;
    cbAutoStartMonitor: TCheckBox;
    cbLogEnabled: TCheckBox;
    Bevel1: TBevel;
    beLogFilename: TButtonedEdit;
    OpenDialog: TOpenDialog;
    ImageList: TImageList;
    lLogFilename: TLabel;
    lCloseAction: TLabel;
    rbTerminate: TRadioButton;
    rbHideToTray: TRadioButton;
    lIntervalComment: TLabel;
    cbEnableAlerts: TCheckBox;

    procedure bSaveClick(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure beLogFilenameRightButtonClick(Sender: TObject);
    procedure cbLogEnabledClick(Sender: TObject);
    procedure leIntervalChange(Sender: TObject);
  private
    function CheckFilenameValid(const AFilename: string): Boolean;
    procedure CheckSaving;

    function GetAutoStartMonitor: Boolean;
    function GetCloseAction: TMainFormCloseAction;
    function GetInterval: Cardinal;
    function GetLogEnabled: Boolean;
    function GetLogFilename: string;
    procedure SetAutoStartMonitor(const Value: Boolean);
    procedure SetCloseAction(const Value: TMainFormCloseAction);
    procedure SetInterval(const Value: Cardinal);
    procedure SetLogEnabled(const Value: Boolean);
    procedure SetLogFilename(const Value: string);
    function GetEnableAlerts: Boolean;
    procedure SetEnableAlerts(const Value: Boolean);
  public
    ///	<summary>
    ///	  �������� ������� ����� ����������� � API ����������� �� �������� ��
    ///	</summary>
    property Interval: Cardinal read GetInterval write SetInterval;

    ///	<summary>
    ///	  ���� ��������������
    ///	</summary>
    property LogEnabled: Boolean read GetLogEnabled write SetLogEnabled;

    ///	<summary>
    ///	  ���� ����
    ///	</summary>
    property LogFilename: string read GetLogFilename write SetLogFilename;

    ///	<summary>
    ///	  ���� ����������� �������� ��� ������ ���������
    ///	</summary>
    property AutoStartMonitor: Boolean read GetAutoStartMonitor
      write SetAutoStartMonitor;

    ///	<summary>
    ///	  ���� ���������� ����������� �����������
    ///	</summary>
    property EnableAlerts: Boolean read GetEnableAlerts write SetEnableAlerts;

    ///	<summary>
    ///	  �������� �� ��������� ��� ������� ������ "�������"��������� ����
    ///	  ���������
    ///	</summary>
    property CloseAction: TMainFormCloseAction read GetCloseAction
      write SetCloseAction;
  end;

///	<summary>
///	  ������� �������� �������� ���������� ��������
///	</summary>
///	<param name="Interval">
///	  �������� ������� ����� ����������� � API ����������� �� �������� ��
///	</param>
///	<param name="LogEnabled">
///	  ���� ��������������
///	</param>
///	<param name="AutoStartMonitor">
///	  ���� ����������� �������� ��� ������ ���������
///	</param>
///	<param name="EnableAlerts">
///	  ���� ���������� ����������� �����������
///	</param>
///	<param name="LogFilename">
///	  ���� ����
///	</param>
///	<param name="CloseAction">
///	  �������� �� ��������� ��� ������� ������ "�������"��������� ���� ���������
///	</param>
///	<remarks>
///	  ���������� True, ���� ������������ ������ ��������� ��������� ��������
///	  ���������� ��������
///	</remarks>
function ShowParamDialog(var Interval: Cardinal; var LogEnabled,
  AutoStartMonitor, EnableAlerts: Boolean; var LogFilename: string;
  var CloseAction: TMainFormCloseAction): Boolean;

implementation

uses System.IOUtils;

function ShowParamDialog(var Interval: Cardinal; var LogEnabled,
  AutoStartMonitor, EnableAlerts: Boolean; var LogFilename: string;
  var CloseAction: TMainFormCloseAction): Boolean;
var
  Dlg: TOptionsForm;
begin
  Dlg := TOptionsForm.Create(Application);
  try
    Dlg.Interval := Interval;
    Dlg.LogEnabled := LogEnabled;
    Dlg.LogFilename := LogFilename;
    Dlg.AutoStartMonitor := AutoStartMonitor;
    Dlg.EnableAlerts := EnableAlerts;
    Dlg.CloseAction := CloseAction;

    Result := Dlg.ShowModal = mrOK;

    if Result then
    begin
      Interval := Dlg.Interval;
      LogEnabled := Dlg.LogEnabled;
      LogFilename := Dlg.LogFilename;
      AutoStartMonitor := Dlg.AutoStartMonitor;
      EnableAlerts := Dlg.EnableAlerts;
      CloseAction := Dlg.CloseAction;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

{$R *.dfm}

procedure TOptionsForm.bCloseClick(Sender: TObject);
begin
  ModalResult := mrClose;
end;

procedure TOptionsForm.beLogFilenameRightButtonClick(Sender: TObject);
begin
  if TDirectory.Exists(TPath.GetDirectoryName(LogFilename)) then
  begin
    OpenDialog.InitialDir := TPath.GetDirectoryName(LogFilename);
    OpenDialog.FileName := TPath.GetFileName(LogFilename);
  end;

  if OpenDialog.Execute then
    LogFilename := OpenDialog.FileName;
end;

procedure TOptionsForm.bSaveClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TOptionsForm.cbLogEnabledClick(Sender: TObject);
begin
  lLogFilename.Enabled := LogEnabled;
  beLogFilename.Enabled := LogEnabled;

  CheckSaving;
end;

function TOptionsForm.CheckFilenameValid(const AFilename: string): Boolean;
begin
  Result := not TPath.HasValidFileNameChars(AFilename, False);

  if Result then
    Result := Result and TDirectory.Exists(TPath.GetDirectoryName(AFilename));
end;

procedure TOptionsForm.CheckSaving;
var
  CanContinue: Boolean;
begin
  CanContinue := (Interval >= 5000) and (Interval <= 3600000);

  if LogEnabled then
    CanContinue := CanContinue and CheckFilenameValid(LogFilename);

  bSave.Enabled := CanContinue;
end;

function TOptionsForm.GetAutoStartMonitor: Boolean;
begin
  Result := cbAutoStartMonitor.Checked;
end;

function TOptionsForm.GetCloseAction: TMainFormCloseAction;
begin
  Result := fcaClose;

  if rbHideToTray.Checked then
    Result := fcaHideToTray;
end;

function TOptionsForm.GetEnableAlerts: Boolean;
begin
  Result := cbEnableAlerts.Checked;
end;

function TOptionsForm.GetInterval: Cardinal;
begin
  Result := StrToIntDef(leInterval.Text, 15) * 1000;
end;

function TOptionsForm.GetLogEnabled: Boolean;
begin
  Result := cbLogEnabled.Checked;
end;

function TOptionsForm.GetLogFilename: string;
begin
  Result := EmptyStr;

  if CheckFilenameValid(beLogFilename.Text) then
    Result := beLogFilename.Text;
end;

procedure TOptionsForm.leIntervalChange(Sender: TObject);
begin
  CheckSaving;
end;

procedure TOptionsForm.SetAutoStartMonitor(const Value: Boolean);
begin
  cbAutoStartMonitor.Checked := Value;
end;

procedure TOptionsForm.SetCloseAction(const Value: TMainFormCloseAction);
begin
  rbTerminate.Checked := Value = fcaClose;
  rbHideToTray.Checked := Value = fcaHideToTray;
end;

procedure TOptionsForm.SetEnableAlerts(const Value: Boolean);
begin
  cbEnableAlerts.Checked := Value;
end;

procedure TOptionsForm.SetInterval(const Value: Cardinal);
var
  Temp: Cardinal;
begin
  Temp := Value;

  if not ((Temp >= 5000) and (Temp <= 3600000)) then
    Temp := 15000;

  leInterval.Text := IntToStr(Temp div 1000);
end;

procedure TOptionsForm.SetLogEnabled(const Value: Boolean);
begin
  cbLogEnabled.Checked := Value;
  lLogFilename.Enabled := Value;
  beLogFilename.Enabled := Value;
end;

procedure TOptionsForm.SetLogFilename(const Value: string);
begin
  beLogFilename.Text := Value;
end;

end.
