{                                                       }
{       GT Delphi Components                            }
{       GT Threaded Timer                               }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }

unit GTTimer;

interface

uses
  System.Classes;

type
  TgtTimer = class;

  TgtTimerThread = class(TThread)
  private
    FTimer: TgtTimer;
  protected
    procedure DoTimer;
  public
    constructor Create(ATimer: TgtTimer);
    procedure Execute; override;
  end;

  TgtTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;

    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
  protected
    FTimerThread: TgtTimerThread;

    procedure UpdateTimer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: Cardinal read FInterval write SetInterval;
  published
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

uses
  WinApi.Windows, System.SysUtils;

{ TgtTimerThread }

constructor TgtTimerThread.Create(ATimer: TgtTimer);
begin
  inherited Create(True);

  FreeOnTerminate := True;
  FTimer := ATimer;
end;

procedure TgtTimerThread.DoTimer;
begin
  if Assigned(FTimer.OnTimer) then
    FTimer.OnTimer(FTimer);
end;

procedure TgtTimerThread.Execute;
begin
  while not Self.Terminated and FTimer.Enabled do
  begin
    WaitForSingleObject(self.Handle, FTimer.Interval);
    Synchronize(DoTimer);
  end;
end;

{ TgtTimer }

constructor TgtTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEnabled  := False;
  FInterval := 1000;
end;

destructor TgtTimer.Destroy;
begin
  if Enabled then
    FTimerThread.Terminate;

  inherited Destroy;
end;

procedure TgtTimer.UpdateTimer;
begin
  if Assigned(FTimerThread) then
  begin
    FTimerThread.Terminate;
    FTimerThread := nil;
  end;

  if Enabled then
  begin
    if FInterval > 0 then
    begin
      FTimerThread := TgtTimerThread.Create(Self);
      FTimerThread.Resume;
    end
    else
      Enabled := False;
  end;
end;

procedure TgtTimer.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  UpdateTimer;
end;

procedure TgtTimer.SetInterval(const Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

end.
