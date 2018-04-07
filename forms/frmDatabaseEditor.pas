///	<summary>
///	  Модуль содержит описание класса окна редактора регистрационной информации
///	  базы данных
///	</summary>
unit frmDatabaseEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ImgList, Firebird.Monitors;

type
  ///	<summary>
  ///	  Класс окна редактора регистрационной информации базы данных
  ///	</summary>
  TDatabaseEditor = class(TForm)
    bClose: TButton;
    bSave: TButton;
    leDescription: TLabeledEdit;
    gbServer: TGroupBox;
    lServerVersion: TLabel;
    cbServerVersion: TComboBox;
    lClientLib: TLabel;
    beClientLib: TButtonedEdit;
    ImageList: TImageList;
    OpenDialog: TOpenDialog;
    leDatabase: TLabeledEdit;
    bTestConnection: TButton;
    gbUser: TGroupBox;
    leUser: TLabeledEdit;
    lePassword: TLabeledEdit;
    leRole: TLabeledEdit;

    procedure beClientLibRightButtonClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure leDatabaseChange(Sender: TObject);
    procedure leDescriptionChange(Sender: TObject);
    procedure beClientLibChange(Sender: TObject);
    procedure leUserChange(Sender: TObject);
    procedure lePasswordChange(Sender: TObject);
    procedure leRoleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbServerVersionChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure bTestConnectionClick(Sender: TObject);
  strict private
    FDatabase: TFBDatabase;
    FDatabaseInfo: TStrings;
  private
    procedure DoLoadFBVersionList;

    procedure DoCheckSaveAndTest;
    procedure DoSetUIValues;

    procedure DoTestConnection;

    function GetDatabaseInfo: TStrings;
    procedure SetDatabaseInfo(const Value: TStrings);
    function GetClientLib: string;
    function GetDatabase: string;
    function GetDescription: string;
    function GetPassword: string;
    function GetRole: string;
    function GetServerVersion: TFBServerVersion;
    function GetUser: string;
    procedure SetClientLib(const Value: string);
    procedure SetDatabase(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetRole(const Value: string);
    procedure SetServerVersion(const Value: TFBServerVersion);
    procedure SetUser(const Value: string);
  protected
    property UIDescription: string read GetDescription write SetDescription;
    property UIServerVersion: TFBServerVersion read GetServerVersion
      write SetServerVersion;
    property UIDatabase: string read GetDatabase write SetDatabase;
    property UIUser: string read GetUser write SetUser;
    property UIPassword: string read GetPassword write SetPassword;
    property UIRole: string read GetRole write SetRole;
    property UIClientLib: string read GetClientLib write SetClientLib;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    ///	<summary>
    ///	  Сведения о базе данных в виде "параметр=значение"
    ///	</summary>
    property DatabaseInfo: TStrings read GetDatabaseInfo write SetDatabaseInfo;
  end;

///	<summary>
///	  Функция вызывает редактор регистрационной информации базы данных
///	</summary>
///	<param name="Info">
///	  Сведения о базе данных в виде "параметр=значение"
///	</param>
///	<remarks>
///	  Функция возвращает True, если пользователь указал сохранить
///	  регистрационную информацию о базе
///	</remarks>
function EditDatabaseInfo(Info: TStrings): Boolean;

implementation

uses System.IOUtils, System.StrUtils, System.UITypes;

resourcestring
  RsSuccessConnect = 'Соединение установлено успешно!';
  RsUnSuccessConnect = 'Ошибка соединения: %s';

function EditDatabaseInfo(Info: TStrings): Boolean;
var
  Dlg: TDatabaseEditor;
begin
  Dlg := TDatabaseEditor.Create(Application);
  try
    Dlg.DatabaseInfo := Info;
    Result := Dlg.ShowModal = mrOK;

    if Result then
      Info.Assign(Dlg.DatabaseInfo);
  finally
    FreeAndNil(Dlg);
  end;
end;

{$R *.dfm}

procedure TDatabaseEditor.bCloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TDatabaseEditor.beClientLibChange(Sender: TObject);
begin
  FDatabase.ClientLib := UIClientLib;
  DoCheckSaveAndTest;
end;

procedure TDatabaseEditor.beClientLibRightButtonClick(Sender: TObject);
begin
  if TDirectory.Exists(TPath.GetDirectoryName(UIClientLib)) then
  begin
    OpenDialog.InitialDir := TPath.GetDirectoryName(UIClientLib);
    OpenDialog.FileName := TPath.GetFileName(UIClientLib);
  end;

  if OpenDialog.Execute then
  begin
    FDatabase.ClientLib := OpenDialog.FileName;
    UIClientLib := FDatabase.ClientLib;
  end;
end;

procedure TDatabaseEditor.bSaveClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TDatabaseEditor.bTestConnectionClick(Sender: TObject);
begin
  DoTestConnection;
end;

procedure TDatabaseEditor.cbServerVersionChange(Sender: TObject);
begin
  FDatabase.ServerVersion := UIServerVersion;
  UIClientLib := FDatabase.ClientLib;

  DoCheckSaveAndTest;
end;

constructor TDatabaseEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDatabase := TFBDatabase.Create;
  FDatabaseInfo := TStringList.Create;
end;

destructor TDatabaseEditor.Destroy;
begin
  FreeAndNil(FDatabase);
  FreeAndNil(FDatabaseInfo);

  inherited Destroy;
end;

procedure TDatabaseEditor.DoCheckSaveAndTest;
begin
  bSave.Enabled := (FDatabase.Description.Trim <> EmptyStr) and
    (FDatabase.Database.Trim <> EmptyStr) and
    TFile.Exists(FDatabase.ClientLib) and (FDatabase.User.Trim <> EmptyStr);

  bTestConnection.Enabled := (FDatabase.Database.Trim <> EmptyStr) and
    TFile.Exists(FDatabase.ClientLib) and (FDatabase.User.Trim <> EmptyStr) and
    (FDatabase.Password.Trim <> EmptyStr);
end;

procedure TDatabaseEditor.DoLoadFBVersionList;
var
  FBVer: TFBServerVersion;
begin
  cbServerVersion.Clear;

  for FBVer := Low(TFBServerVersion) to High(TFBServerVersion) do
    cbServerVersion.Items.Add(SFBVersions[FBVer]);
end;

procedure TDatabaseEditor.DoSetUIValues;
begin
  UIDescription := FDatabase.Description;
  UIDatabase := FDatabase.Database;
  UIClientLib := FDatabase.ClientLib;
  UIUser := FDatabase.User;
  UIPassword := FDatabase.Password;
  UIRole := FDatabase.Role;
  UIServerVersion := FDatabase.ServerVersion;
end;

procedure TDatabaseEditor.DoTestConnection;
begin
  Screen.Cursor := crHourGlass;
  try
    if FDatabase.TestConnection then
      MessageDlg(RsSuccessConnect, mtInformation, [mbOK], 0)
    else
      MessageDlg(Format(RsUnSuccessConnect, [FDatabase.LastErrorMsg]),
        mtError, [mbOK], 0);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TDatabaseEditor.FormCreate(Sender: TObject);
begin
  DoLoadFBVersionList;
end;

procedure TDatabaseEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

function TDatabaseEditor.GetClientLib: string;
begin
  Result := beClientLib.Text;
end;

function TDatabaseEditor.GetDatabase: string;
begin
  Result := leDatabase.Text;
end;

function TDatabaseEditor.GetDatabaseInfo: TStrings;
begin
  FDatabaseInfo.Clear;

  FDatabase.SaveToStrings(FDatabaseInfo);
  Result := FDatabaseInfo;
end;

function TDatabaseEditor.GetDescription: string;
begin
  Result := leDescription.Text;
end;

function TDatabaseEditor.GetPassword: string;
begin
  Result := lePassword.Text;
end;

function TDatabaseEditor.GetRole: string;
begin
  Result := leRole.Text;
end;

function TDatabaseEditor.GetServerVersion: TFBServerVersion;
var
  FBVer: string;
begin
  FBVer := cbServerVersion.Text;

  case IndexText(FBVer, SFBVersions) of
    1: Result := fb21;
    2: Result := fb25;
  else
    Result := fbUnknown;
  end;
end;

function TDatabaseEditor.GetUser: string;
begin
  Result := leUser.Text;
end;

procedure TDatabaseEditor.leDatabaseChange(Sender: TObject);
begin
  FDatabase.Database := UIDatabase;
  DoCheckSaveAndTest;
end;

procedure TDatabaseEditor.leDescriptionChange(Sender: TObject);
begin
  FDatabase.Description := UIDescription;
  DoCheckSaveAndTest;
end;

procedure TDatabaseEditor.lePasswordChange(Sender: TObject);
begin
  FDatabase.Password := UIPassword;
  DoCheckSaveAndTest;
end;

procedure TDatabaseEditor.leRoleChange(Sender: TObject);
begin
  FDatabase.Role := UIRole;
  DoCheckSaveAndTest;
end;

procedure TDatabaseEditor.leUserChange(Sender: TObject);
begin
  FDatabase.User := UIUser;
  DoCheckSaveAndTest;
end;

procedure TDatabaseEditor.SetClientLib(const Value: string);
begin
  beClientLib.Text := Value;
end;

procedure TDatabaseEditor.SetDatabase(const Value: string);
begin
  leDatabase.Text := Value;
end;

procedure TDatabaseEditor.SetDatabaseInfo(const Value: TStrings);
begin
  FDatabaseInfo.Clear;

  if Assigned(Value) then
    FDatabaseInfo.Assign(Value);

  FDatabase.LoadFromStrings(FDatabaseInfo);
  DoSetUIValues;
end;

procedure TDatabaseEditor.SetDescription(const Value: string);
begin
  leDescription.Text := Value;
end;

procedure TDatabaseEditor.SetPassword(const Value: string);
begin
  lePassword.Text := Value;
end;

procedure TDatabaseEditor.SetRole(const Value: string);
begin
  leRole.Text := Value;
end;

procedure TDatabaseEditor.SetServerVersion(const Value: TFBServerVersion);
var
  FBVer: string;
  Index: Integer;
begin
  FBVer := SFBVersions[Value];
  Index := cbServerVersion.Items.IndexOf(FBVer);

  cbServerVersion.ItemIndex := Index;
end;

procedure TDatabaseEditor.SetUser(const Value: string);
begin
  leUser.Text := Value;
end;

end.
