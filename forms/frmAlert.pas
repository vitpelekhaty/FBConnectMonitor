unit frmAlert;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Generics.Collections, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TAlertManager = class;

  ///	<summary>
  ///	  Окно уведомления
  ///	</summary>
  TAlertForm = class(TForm)
    LiveTimer: TTimer;
    Image: TImage;
    lTitle: TLabel;
    lMsg: TLabel;

    procedure LiveTimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    FOwner: TAlertManager;
  private
    procedure SetMessage(const Value: string);
    procedure SetTitle(const Value: string);
    function GetMessage: string;
    function GetTitle: string;
    procedure SetIcon(const Value: HICON);
  public
    ///	<summary>
    ///	  Конструктор окна
    ///	</summary>
    ///	<param name="AOwner">
    ///	  Менеджер уведомлений
    ///	</param>
    ///	<param name="ATitle">
    ///	  Заголовок уведомления
    ///	</param>
    ///	<param name="AMsg">
    ///	  Текст уведомления
    ///	</param>
    ///	<param name="AInterval">
    ///	  Время жизни уведомления
    ///	</param>
    constructor Create(AOwner: TAlertManager; const ATitle: string = '';
      const AMsg: string = ''; const AInterval: Int64 = 10000); reintroduce;

    ///	<summary>
    ///	  Заголовок уведомления
    ///	</summary>
    property Title: string read GetTitle write SetTitle;

    ///	<summary>
    ///	  Текст уведомления
    ///	</summary>
    property Msg: string read GetMessage write SetMessage;

    ///	<summary>
    ///	  Дескриптор иконки в окне уведомления
    ///	</summary>
    property Icon: HICON write SetIcon;
  end;

  ///	<summary>
  ///	  Менеджер уведомлений
  ///	</summary>
  TAlertManager = class
  strict private
    FAlerts: TList<HWND>;
  private
    procedure UpdateAlertPos;
  protected
    procedure UnregisterAlert(const hAlert: THandle);
  public
    constructor Create;
    destructor Destroy; override;

    ///	<summary>
    ///	  Метод создает и регистрирует уведомление
    ///	</summary>
    ///	<param name="ATitle">
    ///	  Заголовок уведомления
    ///	</param>
    ///	<param name="AMsg">
    ///	  Текст уведомления
    ///	</param>
    ///	<param name="AIcon">
    ///	  Дескриптор иконки в окне уведомления
    ///	</param>
    procedure RegisterAlert(const ATitle, AMsg: string; const AIcon: HICON);
  end;

implementation

{$R *.dfm}

{ TAlertForm }

constructor TAlertForm.Create(AOwner: TAlertManager; const ATitle, AMsg: string;
  const AInterval: Int64);
begin
  inherited Create(Application);

  FOwner := AOwner;

  LiveTimer.Interval := AInterval;
  LiveTimer.Enabled := True;

  self.Title := ATitle;
  self.Msg := AMsg;
end;

procedure TAlertForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FOwner) then
    FOwner.UnregisterAlert(self.Handle);
end;

function TAlertForm.GetMessage: string;
begin
  Result := lMsg.Caption;
end;

function TAlertForm.GetTitle: string;
begin
  Result := lTitle.Caption;
end;

procedure TAlertForm.LiveTimerTimer(Sender: TObject);
begin
  LiveTimer.Enabled := False;

  while self.AlphaBlendValue > 0 do
  begin
    self.AlphaBlendValue := self.AlphaBlendValue - 5;
    Sleep(5);

    Application.ProcessMessages;
  end;

  self.Destroy;
end;

procedure TAlertForm.SetIcon(const Value: HICON);
begin
  Image.Picture.Icon.Handle := Value;
end;

procedure TAlertForm.SetMessage(const Value: string);
begin
  lMsg.Caption := Value;
end;

procedure TAlertForm.SetTitle(const Value: string);
begin
  lTitle.Caption := Value;
end;

{ TAlertManager }

constructor TAlertManager.Create;
begin
  inherited Create;
  FAlerts := TList<HWND>.Create;
end;

destructor TAlertManager.Destroy;
begin
  FreeAndNil(FAlerts);
  inherited Destroy;
end;

procedure TAlertManager.RegisterAlert(const ATitle, AMsg: string;
  const AIcon: HICON);
var
  Alert: TAlertForm;
begin
  Alert := TAlertForm.Create(self, ATitle, AMsg);
  Alert.Icon := AIcon;

  FAlerts.Add(Alert.Handle);
  UpdateAlertPos;
end;

procedure TAlertManager.UnregisterAlert(const hAlert: THandle);
begin
  if FAlerts.Contains(hAlert) then
  begin
    FAlerts.Extract(hAlert);
    UpdateAlertPos;
  end;
end;

procedure TAlertManager.UpdateAlertPos;
var
  x, y, cx, cy: Integer;
  hAlert: HWND;
  Rect: TRect;
begin
  x := Screen.WorkAreaWidth;
  y := Screen.WorkAreaHeight;

  for hAlert in FAlerts do
  begin
    if GetWindowRect(hAlert, Rect) then
    begin
      cx := Rect.Width;
      cy := Rect.Height;

      Dec(y, Rect.Height + 2);

      SetWindowPos(hAlert, HWND_TOPMOST, x - Rect.Width - 10, y, cx, cy,
        SWP_SHOWWINDOW + SWP_NOACTIVATE);
    end;
  end;
end;

end.
