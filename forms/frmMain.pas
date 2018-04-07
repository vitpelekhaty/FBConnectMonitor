///	<summary>
///	  Модуль содержит описания классов главного окна программы, хранилища
///	  параметров монитора и журналирования
///	</summary>
unit frmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.PlatformDefaultStyleActnCtrls, System.Actions, Vcl.ActnList, Vcl.ActnMan,
  Vcl.ImgList, Vcl.ActnCtrls, Vcl.ToolWin, Vcl.ActnMenus, VirtualTrees,
  GTTimer, Firebird.Monitors, Vcl.ExtCtrls, Vcl.Menus, Vcl.ActnPopup,
  Vcl.ComCtrls, frmOptions, frmAbout, frmAlert;

type
  ///	<summary>
  ///	  Класс параметров монитора
  ///	</summary>
  TMonitorParams = record
  private
    const
      DefaultInterval = 15000;
  public
    class var
      ///	<summary>
      ///	  Интервал времени между обращениями к API мониторинга на серверах БД
      ///	</summary>
      Interval: Cardinal;

      ///	<summary>
      ///	  Флаг журналирования
      ///	</summary>
      ///	<remarks>
      ///	  Если флаг установлен в True, то, при условии, что корректно указан
      ///	  путь к файлу лога, будет вестись журналирование событий монитора
      ///	</remarks>
      LogEnabled: Boolean;

      ///	<summary>
      ///	  Файл лога
      ///	</summary>
      LogFilename: string;

      ///	<summary>
      ///	  Флаг автозапуска монитора при старте программы
      ///	</summary>
      AutoStartMonitor: Boolean;

      ///	<summary>
      ///	  Действие по умолчанию при нажатии кнопки "Закрыть" формы
      ///	</summary>
      CloseAction: TMainFormCloseAction;

      ///	<summary>
      ///	  Флаг разрешения всплывающий уведомлений
      ///	</summary>
      EnableAlerts: Boolean;

    ///	<summary>
    ///	  "Конструктор" класса
    ///	</summary>
    class function Create: TMonitorParams; static;

    ///	<summary>
    ///	  Метод вызывает редактор параметров монитора
    ///	</summary>
    procedure ShowParamDialog;

    ///	<summary>
    ///	  Метод загружает параметры монитора из файла
    ///	</summary>
    ///	<param name="Filename">
    ///	  Имя файла конфигурации
    ///	</param>
    procedure LoadFromFile(const Filename: string);

    ///	<summary>
    ///	  Метод сохраняет параметры монитора в файл
    ///	</summary>
    ///	<param name="Filename">
    ///	  Файл конфигурации
    ///	</param>
    procedure SaveToFile(const Filename: string);
  end;

type
  ///	<summary>
  ///	  Класс журналирования событий монитора
  ///	</summary>
  TLogger = record
  private
    const
      NativeSpace = ' ';

    class function CheckFilenameValid(const AFilename: string): Boolean; static;
  public
    class var
      ///	<summary>
      ///	  Флаг включения "службы" журналирования
      ///	</summary>
      ///	<remarks>
      ///	  Если флаг установлен в True, то, при условии, что корректно указан
      ///	  путь к файлу лога, будет вестись журналирование событий монитора
      ///	</remarks>
      Enabled: Boolean;

      ///	<summary>
      ///	  Имя файла лога
      ///	</summary>
      LogFilename: string;

    ///	<summary>
    ///	  "Конструктор" класса
    ///	</summary>
    ///	<param name="AEnabled">
    ///	  Флаг журналирования
    ///	</param>
    ///	<param name="ALogFilename">
    ///	  Имя файла лога
    ///	</param>
    class function Create(const AEnabled: Boolean;
      const ALogFilename: string): TLogger; static;

    ///	<summary>
    ///	  Метод записывает событие в лог
    ///	</summary>
    ///	<param name="Database">
    ///	  База данных, в которой монитор заметил изменения в подключениях
    ///	</param>
    ///	<param name="Msg">
    ///	  Сообщение о событии
    ///	</param>
    ///	<remarks>
    ///	  Если Database = nil, то событие приписывается не к базе, а к всему
    ///	  приложению
    ///	</remarks>
    procedure WriteEvent(const Database: TFBDatabase; const Msg: string);
  end;

type
  ///	<summary>
  ///	  Класс главного окна программы
  ///	</summary>
  TMainForm = class(TForm)
    ActionManager: TActionManager;
    ActionImageList: TImageList;
    TreeImageList: TImageList;
    ActionMainMenuBar: TActionMainMenuBar;
    ActionToolBar: TActionToolBar;
    vstUsers: TVirtualStringTree;
    aDatabase_Edit: TAction;
    aDatabase_Add: TAction;
    aDatabase_Delete: TAction;
    aDatabase_Exit: TAction;
    aMonitor_Start: TAction;
    aMonitor_Stop: TAction;
    aMonitor_Params: TAction;
    aHelp_About: TAction;
    TrayIcon: TTrayIcon;
    TrayPopupActionBar: TPopupActionBar;
    itemStartMonitor: TMenuItem;
    itemStopMonitor: TMenuItem;
    Separator1: TMenuItem;
    itemExit: TMenuItem;
    StatusBar: TStatusBar;
    aMonitor_Show: TAction;
    N1: TMenuItem;
    Separator2: TMenuItem;
    aMonitor_Shutdown: TAction;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure aDatabase_ExitExecute(Sender: TObject);
    procedure aMonitor_StartUpdate(Sender: TObject);
    procedure aMonitor_StopUpdate(Sender: TObject);
    procedure aMonitor_StartExecute(Sender: TObject);
    procedure aMonitor_StopExecute(Sender: TObject);
    procedure aDatabase_AddExecute(Sender: TObject);
    procedure vstUsersFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstUsersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstUsersPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure vstUsersGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure aDatabase_EditUpdate(Sender: TObject);
    procedure aDatabase_EditExecute(Sender: TObject);
    procedure vstUsersDblClick(Sender: TObject);
    procedure aDatabase_DeleteExecute(Sender: TObject);
    procedure vstUsersCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstUsersExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure aMonitor_ParamsExecute(Sender: TObject);
    procedure aMonitor_ShowExecute(Sender: TObject);
    procedure aHelp_AboutExecute(Sender: TObject);
    procedure aMonitor_ShutdownUpdate(Sender: TObject);
    procedure aMonitor_ShutdownExecute(Sender: TObject);
    procedure vstUsersFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure vstUsersCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstUsersHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
  strict private
    FDisabledActionImages: TCustomImageList;
    FDatabases: TFBDatabaseList;
    FParams: TMonitorParams;
    FLogger: TLogger;
    FStarted: Boolean;

    FTimer: TGTTimer;
    FAboutForm: TAboutForm;

    FCurrentAttachmentId: Integer;

    FAlertManager: TAlertManager;
  private
    procedure DoUpdateActions;

    procedure DoStartMonitor;
    procedure DoStopMonitor;

    procedure DoCreateUserTree;
    procedure DoAppendItem(Database: TFBDatabase; User: TFBAttachment);
    procedure DoDeleteItem(Database: TFBDatabase; User: TFBAttachment);
    function GetItemNode(Database: TFBDatabase;
      User: TFBAttachment = nil): PVirtualNode;

    function DoEditDatabaseInfo(Database: TFBDatabase): Boolean;

    procedure DoTimerEvent(Sender: TObject);
    procedure DoConnectAttachment(Sender: TFBDatabase;
      Attachment: TFBAttachment);
    procedure DoDisConnectAttachment(Sender: TFBDatabase;
      Attachment: TFBAttachment);

    function DurationToString(const TotalSeconds: Cardinal): string;

    function IsStarted: Boolean;
  protected
    property Started: Boolean read IsStarted;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.IOUtils, System.UITypes, System.TimeSpan, System.IniFiles,
  System.DateUtils, System.StrUtils,  WinApi.UxTheme, Vcl.Themes, Vcl.Styles,
  SpecialImageLists, Windows.Info, frmDatabaseEditor, System.Math;

type
  TUserTreeNode = record
    Database: TFBDatabase;
    User: TFBAttachment;
    Expanded: Boolean;
  end;
  PUserTreeNode = ^TUserTreeNode;

var
  DefaultConfig: string;

resourcestring
  RsDeleteDatabase = 'Действительно удалить регистрационную информацию базы ' +
    '"%s"?';
  RsUserOnline = 'Пользователь %s [%s] подключен к базе';
  RsUserOffline = 'Пользователь %s [%s] завершил работу с базой';
  RsShortDay = 'д';
  RsShortHour = 'ч';
  RsShortMinute = 'мин';
  RsShortSecond = 'сек';
  RsMonitoring = 'Мониторинг';
  RsStarted = 'Новый сеанс...';
  RsMonitorStarted = 'Монитор запущен...';
  RsTerminated = 'Работа программы завершена!';
  RsMonitorStoped = 'Монитор остановлен!';
  RsShutdownPrompt = 'Принудительный разрыв соединения может привести к ' +
    'многочисленным сбоям на стороне подключенного клиента! Продолжить?';

function IsPortable: Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 1 to ParamCount do
  begin
    Result := Result or (IndexText(ParamStr(1), ['--p', '--portable']) >= 0);

    if Result then
      Exit;
  end;
end;

{$R *.dfm}

{ TMainForm }

procedure TMainForm.aDatabase_AddExecute(Sender: TObject);
var
  CanContinue, HasBeenStarted: Boolean;
  Database: TFBDatabase;
begin
  HasBeenStarted := Started;

  if HasBeenStarted then
    DoStopMonitor;

  CanContinue := False;
  Database := TFBDatabase.Create;
  try
    CanContinue := DoEditDatabaseInfo(Database);

    if CanContinue then
      CanContinue := not FDatabases.DatabaseExists(Database.Database);

    if CanContinue then
    begin
      FDatabases.Add(Database);
      FDatabases.SaveToFile(DefaultConfig);

      DoAppendItem(Database, nil);
    end;
  finally
    if not CanContinue then
      FreeAndNil(Database);

    if HasBeenStarted then
      DoStartMonitor;
  end;
end;

procedure TMainForm.aDatabase_DeleteExecute(Sender: TObject);
var
  Msg: string;
  Node: PVirtualNode;
  Data: PUserTreeNode;
  Database: TFBDatabase;
  CanContinue, HasBeenStarted: Boolean;
begin
  if vstUsers.TotalCount = 0 then Exit;

  HasBeenStarted := Started;

  if HasBeenStarted then
    DoStopMonitor;
  try

    Node := vstUsers.GetFirstSelected();
    Data := vstUsers.GetNodeData(Node);

    if Assigned(Data) then
    begin
      Database := Data^.Database;

      if Assigned(Database) then
      begin
        Msg := Format(RsDeleteDatabase, [Database.Description]);
        CanContinue := MessageDlg(Msg, mtConfirmation, mbYesNo, 0, mbNo) = mrYes;

        if CanContinue then
        begin
          DoDeleteItem(Database, nil);
          FDatabases.Remove(Database);

          FDatabases.SaveToFile(DefaultConfig);
        end;
      end;
    end;
  finally
    if HasBeenStarted then
      DoStartMonitor;
  end;
end;

procedure TMainForm.aDatabase_EditExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: PUserTreeNode;
  Database: TFBDatabase;
  CanContinue, HasBeenStarted: Boolean;
begin
  if vstUsers.TotalCount = 0 then Exit;

  HasBeenStarted := Started;

  if HasBeenStarted then
    DoStopMonitor;
  try
    Node := vstUsers.GetFirstSelected();
    Data := vstUsers.GetNodeData(Node);

    if Assigned(Data) then
    begin
      Database := Data^.Database;

      if Assigned(Database) then
      begin
        CanContinue := DoEditDatabaseInfo(Database);

        if CanContinue then
          FDatabases.SaveToFile(DefaultConfig);
      end;
    end;
  finally
    if HasBeenStarted then
      DoStartMonitor;
  end;
end;

procedure TMainForm.aDatabase_EditUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := (FDatabases.Count > 0) and
      (vstUsers.SelectedCount > 0);
end;

procedure TMainForm.aDatabase_ExitExecute(Sender: TObject);
begin
  case FParams.CloseAction of
    fcaClose: Close;

    fcaHideToTray:
      begin
        DoStopMonitor;

        FLogger.WriteEvent(nil, RsTerminated);
        TrayIcon.Visible := False;

        Application.Terminate;
      end;
  end;
end;

procedure TMainForm.aHelp_AboutExecute(Sender: TObject);
begin
  if not TAboutForm.InstanceExists(FAboutForm) then
    FAboutForm := TAboutForm.ShowAboutForm
  else
    ShowWindow(FAboutForm.Handle, SW_NORMAL);
end;

procedure TMainForm.aMonitor_ParamsExecute(Sender: TObject);
var
  HasBeenStarted: Boolean;
begin
  HasBeenStarted := Started;

  if HasBeenStarted then
    DoStopMonitor;
  try
    FParams.ShowParamDialog;

    FTimer.Interval := FParams.Interval;

    FLogger.Enabled := FParams.LogEnabled;
    FLogger.LogFilename := FParams.LogFilename;
  finally
    if HasBeenStarted then
      DoStartMonitor;
  end;
end;

procedure TMainForm.aMonitor_ShowExecute(Sender: TObject);
begin
  Show;
end;

procedure TMainForm.aMonitor_ShutdownExecute(Sender: TObject);
var
  HasBeenStarted: Boolean;
  Node: PVirtualNode;
  Data: PUserTreeNode;
  CanContinue: Boolean;
begin
  if FCurrentAttachmentId <= 0 then Exit;

  HasBeenStarted := Started;

  if HasBeenStarted then
    DoStopMonitor;
  try
    CanContinue := MessageDlg(RsShutdownPrompt, mtWarning, mbYesNo, 0,
      mbNo) = mrYes;

    if CanContinue then
    begin
      Node := vstUsers.GetFirstSelected();
      Data := vstUsers.GetNodeData(Node);

      if Assigned(Data) then
        Data^.Database.ShutdownAttachment(FCurrentAttachmentId);
    end;
  finally
    if HasBeenStarted then
      DoStartMonitor;
  end;
end;

procedure TMainForm.aMonitor_ShutdownUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := FCurrentAttachmentId > 0;
end;

procedure TMainForm.aMonitor_StartExecute(Sender: TObject);
begin
  DoStartMonitor;
end;

procedure TMainForm.aMonitor_StartUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := not Started;
end;

procedure TMainForm.aMonitor_StopExecute(Sender: TObject);
begin
  DoStopMonitor;
end;

procedure TMainForm.aMonitor_StopUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Enabled := Started;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FStarted := False;

  FParams := TMonitorParams.Create;
  FParams.LoadFromFile(DefaultConfig);

  FLogger := TLogger.Create(FParams.LogEnabled, FParams.LogFilename);

  FDatabases := TFBDatabaseList.Create(True);
  FDatabases.LoadFromFile(DefaultConfig);

  FTimer := TGTTimer.Create(self);
  FTimer.Interval := FParams.Interval;
  FTimer.OnTimer := DoTimerEvent;

  FAlertManager := TAlertManager.Create;

  FAboutForm := nil;

  FCurrentAttachmentId := -1;
end;

destructor TMainForm.Destroy;
begin
  FParams.SaveToFile(DefaultConfig);

  FreeAndNil(FTimer);
  FreeAndNil(FDisabledActionImages);
  FreeAndNil(FDatabases);
  FreeAndNil(FAlertManager);

  inherited Destroy;
end;

procedure TMainForm.DoAppendItem(Database: TFBDatabase; User: TFBAttachment);
var
  DatabaseNode, UserNode: PVirtualNode;
  DatabaseData, Data: PUserTreeNode;
  Expanded: Boolean;
begin
  Expanded := True;

  vstUsers.BeginUpdate;
  try
    DatabaseNode := GetItemNode(Database);

    if not Assigned(DatabaseNode) then
    begin
      DatabaseNode := vstUsers.AddChild(nil);

      DatabaseData := vstUsers.GetNodeData(DatabaseNode);

      if Assigned(DatabaseData) then
      begin
        DatabaseData^.Expanded := True;
        DatabaseData^.User := nil;
        DatabaseData^.Database := Database;
      end;
    end
    else
    begin
      DatabaseData := vstUsers.GetNodeData(DatabaseNode);

      if Assigned(DatabaseData) then
        Expanded := DatabaseData^.Expanded;
    end;

    if Assigned(User) then
    begin
      UserNode := vstUsers.AddChild(DatabaseNode);
      Data := vstUsers.GetNodeData(UserNode);

      if Assigned(Data) then
      begin
        Data^.Database := Database;
        Data^.User := User;
        Data^.Expanded := False;
      end;
    end;

    if (vstUsers.ChildCount[DatabaseNode] > 0) and Expanded then
      vstUsers.FullExpand(DatabaseNode);
  finally
    vstUsers.EndUpdate;
  end;
end;

procedure TMainForm.DoConnectAttachment(Sender: TFBDatabase;
  Attachment: TFBAttachment);
var
  Msg: string;
begin
  if Assigned(Sender) and Assigned(Attachment) then
  begin
    Msg := Format(RsUserOnline, [Attachment.User, Attachment.RemoteAddress]);
    DoAppendItem(Sender, Attachment);

    FLogger.WriteEvent(Sender, Msg);

    if FParams.EnableAlerts then
      FAlertManager.RegisterAlert(Sender.Description, Msg,
        Application.Icon.Handle);
  end;
end;

procedure TMainForm.DoCreateUserTree;
var
  Db: TFBDatabase;
begin
  vstUsers.Clear;
  if FDatabases.Count = 0 then Exit;

  for Db in FDatabases do
    DoAppendItem(Db, nil);
end;

procedure TMainForm.DoDeleteItem(Database: TFBDatabase; User: TFBAttachment);
var
  Node: PVirtualNode;
begin
  Node := GetItemNode(Database, User);
  vstUsers.DeleteNode(Node);
end;

procedure TMainForm.DoDisConnectAttachment(Sender: TFBDatabase;
  Attachment: TFBAttachment);
var
  Msg: string;
begin
  if Assigned(Sender) and Assigned(Attachment) then
  begin
    Msg := Format(RsUserOffline, [Attachment.User, Attachment.RemoteAddress]);
    DoDeleteItem(Sender, Attachment);

    FLogger.WriteEvent(Sender, Msg);

    if FParams.EnableAlerts then
      FAlertManager.RegisterAlert(Sender.Description, Msg,
        Application.Icon.Handle);
  end;
end;

function TMainForm.DoEditDatabaseInfo(Database: TFBDatabase): Boolean;
var
  Info: TStrings;
begin
  Result := False;
  if not Assigned(Database) then Exit;

  Info := TStringList.Create;
  try
    Database.SaveToStrings(Info);

    Result := EditDatabaseInfo(Info);

    if Result then
      Database.LoadFromStrings(Info);
  finally
    FreeAndNil(Info);
  end;
end;

procedure TMainForm.DoStartMonitor;
begin
  try
    FDatabases.OnConnectAttachment := DoConnectAttachment;
    FDatabases.OnDisConnectAttachment := DoDisConnectAttachment;

    if FDatabases.Count > 0 then
    begin
      FDatabases.CheckAttachments;
      FTimer.Enabled := True;
    end;
  finally
    FLogger.WriteEvent(nil, RsMonitorStarted);
    DoUpdateActions;
  end;
end;

procedure TMainForm.DoStopMonitor;
begin
  try
    FTimer.Enabled := False;

    FDatabases.OnConnectAttachment := nil;
    FDatabases.OnDisConnectAttachment := nil;
  finally
    FLogger.WriteEvent(nil, RsMonitorStoped);
    DoUpdateActions;
  end;
end;

procedure TMainForm.DoTimerEvent(Sender: TObject);
begin
  FDatabases.CheckAttachments;
  vstUsers.Invalidate;
end;

procedure TMainForm.DoUpdateActions;
var
  i: Integer;
  Action: TBasicAction;
begin
  for i := 0 to ActionManager.ActionCount - 1 do
  begin
    Action := ActionManager.Actions[i];

    if Assigned(Action) then
      Action.Update;
  end;

  StatusBar.Invalidate;
end;

function TMainForm.DurationToString(const TotalSeconds: Cardinal): string;
var
  Days, Hours, Minutes, Seconds: Integer;
  TS: TTimeSpan;
  SB: TStringBuilder;
const
  NativeSpace = ' ';
begin
  SB := TStringBuilder.Create;
  try
    TS := TTimeSpan.FromSeconds(TotalSeconds);

    Days := Trunc(TS.TotalDays);
    Hours := Trunc(TS.TotalHours);
    Minutes := Trunc(TS.Minutes);
    Seconds := Trunc(TS.TotalSeconds - (
      (Minutes * 60) + (Hours * 60 * 60) + (Days * 24 * 60 * 60))
    );

    if Days > 0 then
    begin
      SB.AppendFormat('%d%s', [Days, RsShortDay]);
      SB.Append(NativeSpace);
    end;

    if Hours > 0 then
    begin
      SB.AppendFormat('%d%s', [Hours, RsShortHour]);
      SB.Append(NativeSpace);
    end;

    if Minutes > 0 then
    begin
      SB.AppendFormat('%d%s', [Minutes, RsShortMinute]);
      SB.Append(NativeSpace);
    end;

    SB.AppendFormat('%d%s', [Seconds, RsShortSecond]);

    Result := SB.ToString;
  finally
    FreeAndNil(SB);
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  case FParams.CloseAction of
    fcaClose:
      begin
        DoStopMonitor;

        FLogger.WriteEvent(nil, RsTerminated);
        TrayIcon.Visible := False;

        Action := caFree;
      end;

    fcaHideToTray:
      begin
        Hide;
        Action := caNone;
      end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDisabledActionImages := CreateSpecialImageList(ActionImageList, 0, 0, 1, 0.8);
  ActionManager.DisabledImages := FDisabledActionImages;

  vstUsers.DoubleBuffered := False;
  vstUsers.NodeDataSize := SizeOf(TUserTreeNode);

  TrayIcon.Hint := Application.Title;
  TrayIcon.Icon := Application.Icon;
  TrayIcon.Visible := True;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if not FStarted then
  begin
    DoCreateUserTree;
    FLogger.WriteEvent(nil, RsStarted);

    if FParams.AutoStartMonitor then
      DoStartMonitor;
  end;

  FStarted := True;
end;

function TMainForm.GetItemNode(Database: TFBDatabase;
  User: TFBAttachment): PVirtualNode;
var
  i: Integer;
  Node: PVirtualNode;
  Data: PUserTreeNode;
  CanContinue: Boolean;
begin
  if vstUsers.TotalCount = 0 then Exit(nil);

  Node := nil;

  for i := 0 to vstUsers.TotalCount - 1 do
  begin
    if i <> 0 then
      Node := vstUsers.GetNext(Node)
    else
      Node := vstUsers.GetFirst();

    Data := vstUsers.GetNodeData(Node);

    if Assigned(Data) then
    begin
      CanContinue := Data^.Database = Database;

      if Assigned(User) then
        CanContinue := CanContinue and (Data^.User = User);

      if CanContinue then
        Exit(Node);
    end;
  end;

  Result := nil;
end;

function TMainForm.IsStarted: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TMainForm.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  Msg: string;
  TextRect: TRect;
  Details: TThemedElementDetails;
  SBServices: TCustomStyleServices;
const
  SMonitorStatus: array [Boolean] of string = (
    'OFF', 'ON'
  );
begin
  SBServices := StyleServices;
  Details := SBServices.GetElementDetails(tsPane);

  if Panel.Index = 0 then
  begin
    Msg := Format('%s: %s', [RsMonitoring, SMonitorStatus[Started]]);
    TextRect := Rect;

    OffsetRect(TextRect, 4, 0);

    SBServices.DrawText(StatusBar.Canvas.Handle, Details, Msg,
      TextRect, [tfLeft, tfVerticalCenter]);
  end;
end;

procedure TMainForm.vstUsersCollapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PUserTreeNode;
begin
  Data := Sender.GetNodeData(Node);

  if Assigned(Data) then
    Data^.Expanded := False;
end;

procedure TMainForm.vstUsersCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PUserTreeNode;
begin
  Result := 0;

  case Column of
    2:
      begin
        Data1 := Sender.GetNodeData(Node1);
        Data2 := Sender.GetNodeData(Node2);

        if Assigned(Data1) and Assigned(Data2) then
        begin
          if Assigned(Data1^.User) and Assigned(Data2^.User) then
            Result := System.Math.CompareValue(Data1^.User.Connected,
              Data2^.User.Connected);
        end
      end;

    3:
      begin
        Data1 := Sender.GetNodeData(Node1);
        Data2 := Sender.GetNodeData(Node2);

        if Assigned(Data1) and Assigned(Data2) then
        begin
          if Assigned(Data1^.User) and Assigned(Data2^.User) then
            Result := System.Math.CompareValue(Data1^.User.Duration,
              Data2^.User.Duration);
        end
      end;
  else
    Result := AnsiCompareText(TVirtualStringTree(Sender).Text[Node1, Column],
      TVirtualStringTree(Sender).Text[Node2, Column]);
  end;
end;

procedure TMainForm.vstUsersDblClick(Sender: TObject);
var
  mp, cp: TPoint;
  Node: PVirtualNode;
begin
  if aDatabase_Edit.Enabled then
  begin
    if GetCursorPos(mp) then
    begin
      cp := vstUsers.ScreenToClient(mp);
      Node := vstUsers.GetNodeAt(cp.X, cp.Y);

      if Assigned(Node) then
        aDatabase_EditExecute(Sender);
    end;
  end;
end;

procedure TMainForm.vstUsersExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PUserTreeNode;
begin
  Data := Sender.GetNodeData(Node);

  if Assigned(Data) then
    Data^.Expanded := True;
end;

procedure TMainForm.vstUsersFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PUserTreeNode;
begin
  FCurrentAttachmentId := -1;
  Data := Sender.GetNodeData(Node);

  if Assigned(Data) then
  begin
    if Assigned(Data^.User) then
      FCurrentAttachmentId := Data^.User.Id;
  end;
end;

procedure TMainForm.vstUsersFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PUserTreeNode;
begin
  Data := Sender.GetNodeData(Node);

  if Assigned(Data) then
  begin
    Data^.Database := nil;
    Data^.User := nil;
  end;
end;

procedure TMainForm.vstUsersGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PUserTreeNode;
begin
  ImageIndex := -1;

  if Column = 0 then
  begin
    Data := Sender.GetNodeData(Node);

    if Assigned(Data) then
    begin
      if Assigned(Data^.Database) then
      begin
        if Assigned(Data^.User) then
          ImageIndex := 1   // узел пользователя
        else
          ImageIndex := 0;  // узел базы данных
      end;
    end;
  end;
end;

procedure TMainForm.vstUsersGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: PUserTreeNode;
begin
  CellText := EmptyStr;
  Data := Sender.GetNodeData(Node);

  if not Assigned(Data) then Exit;
  if not Assigned(Data^.Database) then Exit;

  case TextType of
    ttNormal:
      begin
        if Assigned(Data^.User) then // узел пользователя
        begin
          case Column of
            0: CellText := Data^.User.User;
            1: CellText := Data^.User.Role;
            2: CellText := FormatDateTime('dd.mm.yyyy hh:nn:ss', Data^.User.Connected);
            3: CellText := DurationToString(Data^.User.Duration);
            4: CellText := SFBConnectionStates[Data^.User.State];
            5: CellText := Data^.User.RemoteAddress;
            6: CellText := Data^.User.RemoteProcess;
          end;
        end
        else // узел базы данных
        begin
          if Column = 0 then
            CellText := Data^.Database.Description;
        end;
      end;

    ttStatic:
      begin
        if not Assigned(Data^.User) then // узел базы данных
        begin
          if Column = 1 then
            CellText := Data^.Database.LastErrorMsg;
        end;
      end;
  end;
end;

procedure TMainForm.vstUsersHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Button = mbLeft then
  begin
    Sender.SortColumn := HitInfo.Column;

    if Sender.SortDirection = sdAscending then
      Sender.SortDirection := sdDescending
    else
      Sender.SortDirection := sdAscending;

    Sender.Treeview.SortTree(HitInfo.Column, Sender.SortDirection);
  end;
end;

procedure TMainForm.vstUsersPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data: PUserTreeNode;
begin
  case TextType of
    ttNormal:
      begin
        Data := Sender.GetNodeData(Node);

        if Assigned(Data) then
        begin
          if Assigned(Data^.Database) then
          begin
            if not Assigned(Data^.User) then // узел базы данных
            begin
              if not (vsSelected in Node.States) then
                TargetCanvas.Font.Color := clYellow;
            end;
          end;
        end;
      end;

    ttStatic:
      TargetCanvas.Font.Color := clGrayText;
  end;
end;

const
  SParamsFilename = '%s\FBConnectMonitor\params.conf';
  SPortableParamsFilename = '%s\params.conf';

{ TParams }

class function TMonitorParams.Create: TMonitorParams;
begin
  Interval := DefaultInterval;
  LogEnabled := False;
  LogFilename := Format('%s\fb-connect-monitor.log', [
    ExcludeTrailingPathDelimiter(TPath.GetTempPath)
    ]);

  AutoStartMonitor := False;
  EnableAlerts := True;

  CloseAction := fcaClose;
end;

procedure TMonitorParams.LoadFromFile(const Filename: string);
var
  Cfg: TIniFile;
begin
  if not TFile.Exists(Filename) then Exit;

  Cfg := TIniFile.Create(Filename);
  try
    Interval := Cfg.ReadInteger('General', 'Interval', DefaultInterval);
    LogEnabled := Cfg.ReadBool('Log', 'Enabled', False);
    LogFilename := Cfg.ReadString('Log', 'Filename', self.LogFilename);
    AutoStartMonitor := Cfg.ReadBool('General', 'AutoStartMonitor', False);
    EnableAlerts := Cfg.ReadBool('General', 'EnableAlerts', True);
    CloseAction := TMainFormCloseAction(
      Cfg.ReadInteger('General', 'CloseAction', 0));
  finally
    FreeAndNil(Cfg);
  end;
end;

procedure TMonitorParams.SaveToFile(const Filename: string);
var
  Cfg: TIniFile;
begin
  Cfg := TIniFile.Create(Filename);
  try
    Cfg.WriteInteger('General', 'Interval', Interval);
    Cfg.WriteBool('Log', 'Enabled', LogEnabled);
    Cfg.WriteString('Log', 'Filename', LogFilename);
    Cfg.WriteBool('General', 'AutoStartMonitor', AutoStartMonitor);
    Cfg.WriteBool('General', 'EnableAlerts', EnableAlerts);
    Cfg.WriteInteger('General', 'CloseAction', Integer(CloseAction));
  finally
    FreeAndNil(Cfg);
  end;
end;

procedure TMonitorParams.ShowParamDialog;
begin
  frmOptions.ShowParamDialog(Interval, LogEnabled, AutoStartMonitor,
    EnableAlerts, LogFilename, CloseAction);
end;

{ TLogger }

procedure TLogger.WriteEvent(const Database: TFBDatabase; const Msg: string);
var
  Writer: TStreamWriter;
  SB: TStringBuilder;
  STime: string;
begin
  if not Enabled then Exit;
  if not CheckFilenameValid(LogFilename) then Exit;

  Writer := nil;

  if TFile.Exists(LogFilename) then
    Writer := TFile.AppendText(LogFilename)
  else
    Writer := TFile.CreateText(LogFilename);

  if not Assigned(Writer) then Exit;

  SB := TStringBuilder.Create;
  try
    STime := FormatDateTime('[dd.mm.yyyy hh:nn:ss]', Now);

    if Assigned(Database) then
    begin
      SB.AppendFormat('%s %s [%s]', [
        STime, Database.Description, Database.Database
      ]).Append(sLineBreak);
      SB.Append(NativeSpace, Length(STime) + 1).Append(Msg);
    end
    else
      SB.AppendFormat('%s %s', [
        STime, Msg
      ]);

    SB.AppendLine(sLineBreak);
    Writer.WriteLine(SB.ToString);
  finally
    if Assigned(Writer) then
      FreeAndNil(Writer);

    FreeAndNil(SB);
  end;
end;

class function TLogger.CheckFilenameValid(const AFilename: string): Boolean;
begin
  Result := not TPath.HasValidFileNameChars(AFilename, False);

  if Result then
    Result := Result and TDirectory.Exists(TPath.GetDirectoryName(AFilename));
end;

class function TLogger.Create(const AEnabled: Boolean;
  const ALogFilename: string): TLogger;
begin
  Enabled := AEnabled;

  if CheckFilenameValid(ALogFilename) then
    LogFilename := ALogFilename;
end;

initialization
  if not IsPortable then
  begin
    DefaultConfig := Format(SParamsFilename, [
      ExcludeTrailingPathDelimiter(TSystemDir.GetLocalAppDataDir)
      ]);

    if not TDirectory.Exists(TPath.GetDirectoryName(DefaultConfig)) then
      TDirectory.CreateDirectory(TPath.GetDirectoryName(DefaultConfig));
  end
  else
    DefaultConfig := Format(SPortableParamsFilename, [
      ExcludeTrailingPathDelimiter(TPath.GetDirectoryName(ParamStr(0)))
      ]);
end.
