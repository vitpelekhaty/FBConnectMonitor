///	<summary>
///	  ������ �������� �������� �������, ����������� ��� ���������� ��������
///	  ����������� � ��
///	</summary>
unit Firebird.Monitors;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections;

type
  ///	<summary>
  ///	  ������ ������� Firebird
  ///	</summary>
  TFBServerVersion = (
    ///	<summary>
    ///	  �� �������
    ///	</summary>
    fbUnknown,

    ///	<summary>
    ///	  Firebird 2.1
    ///	</summary>
    fb21,

    ///	<summary>
    ///	  Firebird 2.5
    ///	</summary>
    fb25
  );

  ///	<summary>
  ///	  ��������� �����������
  ///	</summary>
  TFBConnectionState = (
    ///	<summary>
    ///	  ���������� �����������
    ///	</summary>
    fbcsIdle,

    ///	<summary>
    ///	  �������� �����������
    ///	</summary>
    fbcsActive,

    fbcsStalled
  );

type
  ///	<summary>
  ///	  ����� ��������� ������� ������ �������� ����������� � ���� ������
  ///	  Firebird
  ///	</summary>
  TFBDatabaseAttribute = class(TCustomAttribute)
  strict private
    FName: string;
  public
    constructor Create(const AName: string); virtual;

    ///	<summary>
    ///	  ��� ��������� � ������������ ����������� � ������� Firebird
    ///	</summary>
    property Name: string read FName write FName;
  end;

type
  ///	<summary>
  ///	  ����� ����������� � ���� ������ Firebird
  ///	</summary>
  TFBAttachment = class(TPersistent)
  strict private
    FId: Cardinal;
    FState: TFBConnectionState;
    FUser: string;
    FRole: string;
    FRemoteAddress: string;
    FConnected: TDateTime;
    FDuration: Cardinal;
    FRemoteProcess: string;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;

    ///	<summary>
    ///	  ������������� �����������
    ///	</summary>
    property Id: Cardinal read FId write FId;

    ///	<summary>
    ///	  ��������� �����������
    ///	</summary>
    property State: TFBConnectionState read FState write FState;

    ///	<summary>
    ///	  ��� ������������
    ///	</summary>
    property User: string read FUser write FUser;

    ///	<summary>
    ///	  ���� ������������
    ///	</summary>
    property Role: string read FRole write FRole;

    ///	<summary>
    ///	  ����� ������������� ������������
    ///	</summary>
    property RemoteAddress: string read FRemoteAddress write FRemoteAddress;

    ///	<summary>
    ///	  ���� � ����� �����������
    ///	</summary>
    property Connected: TDateTime read FConnected write FConnected;

    ///	<summary>
    ///	  ����������������� �����������
    ///	</summary>
    property Duration: Cardinal read FDuration write FDuration;

    ///	<summary>
    ///	  ��� � ���� � �������� ������������
    ///	</summary>
    property RemoteProcess: string read FRemoteProcess write FRemoteProcess;
  end;

  ///	<summary>
  ///	  ������ ����������� � ���� ������ Firebird
  ///	</summary>
  TFBAttachments = class(TObjectList<TFBAttachment>)
  private
    function GetAttachment(ID: Cardinal): TFBAttachment;
  public
    ///	<summary>
    ///	  ����� ���������, ������������� � ������ ���������� � ����������� �
    ///	  ���� ������ Firebird � ��������� ���������������
    ///	</summary>
    ///	<param name="AttachmentId">
    ///	  ������������� �����������
    ///	</param>
    ///	<remarks>
    ///	  ���������� True, ���� �������� � ����������� ���������� � ������
    ///	</remarks>
    function Exists(const AttachmentId: Cardinal): Boolean;

    ///	<summary>
    ///	  ����� ������� �� ������ �������� � ����������� � ���������
    ///	  ���������������
    ///	</summary>
    procedure RemoveAttachment(const AttachmentId: Cardinal);

    ///	<summary>
    ///	  ������� ������, ���������� �������� � ����������� � ���������
    ///	  ���������������
    ///	</summary>
    property Attachment[ID: Cardinal]: TFBAttachment read GetAttachment;
  end;

type
  TFBDatabase = class;

  ///	<summary>
  ///	  ��� �������, ������������ ��� ��������� � ������ �������� ����������� �
  ///	  ���� ������ Firebird
  ///	</summary>
  TAttachmentListChangeEvent = procedure(Sender: TFBDatabase;
    Attachment: TFBAttachment) of object;

  ///	<summary>
  ///	  ����� �������� �������� ����������� � ���� ������ Firebird
  ///	</summary>
  TFBDatabase = class
  strict private
    FParams: TStrings;
    FLastErrorMsg: string;
    FAttachments: TFBAttachments;

    FOnConnectAttachment: TAttachmentListChangeEvent;
    FOnDisConnectAttachment: TAttachmentListChangeEvent;
  private
    procedure DoInitParams;
    function DoCheckAttachments(AAttachments: TFBAttachments): Boolean;
    procedure DoShutdownAttachment(AAttachment: Integer);

    procedure DoAttachListChanged(Sender: TObject; const Item: TFBAttachment;
      Action: TCollectionNotification);

    procedure SetServerVersion(const Value: TFBServerVersion);
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
    procedure SetUser(const Value: string);
    function GetAttributeName(APropName: string): string;
    function GetPropertyValue(APropName: string): string;
    procedure SetPropertyValue(APropName: string; const Value: string);
  protected
    procedure DoConnectAttachment(AAttachment: TFBAttachment); dynamic;
    procedure DoDisConnectAttachment(AAttachment: TFBAttachment); dynamic;

    property AttributeName[APropName: string]: string read GetAttributeName;
    property Value[APropName: string]: string read GetPropertyValue
      write SetPropertyValue;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    ///	<summary>
    ///	  ����� ������������������ ���������� � ����� ������ �� ���������
    ///	  ��������� �����
    ///	</summary>
    ///	<param name="ASource">
    ///	  ��������� �����, �������� ��������� ���������� � ����� ������ � ����
    ///	  ��� "��������=��������"
    ///	</param>
    procedure LoadFromStrings(ASource: TStrings);

    ///	<summary>
    ///	  ����� ������������������ ���������� � ����� �������� ���������
    ///	  ��������� �����
    ///	</summary>
    ///	<param name="ADest">
    ///	  ��������� �����, ��������������� ������������ ���������� ���������� �
    ///	  ����� ������ � ���� ��� "��������=��������"
    ///	</param>
    procedure SaveToStrings(ADest: TStrings);

    ///	<summary>
    ///	  ����� ��������� ����������� ����������� � ���� ������ � ����������
    ///	  ����������� ����������
    ///	</summary>
    ///	<remarks>
    ///	  ����� ���������� True, ���� ������� ������������ � ��������� ����
    ///	  ������ � ���������� ����������� ����������
    ///	</remarks>
    function TestConnection: Boolean;

    ///	<summary>
    ///	  ����� �������� API ����������� ��������� �� �� ������� � ���������
    ///	  ��������� �������� �� �������� ������������ ����������� �������
    ///	</summary>
    ///	<remarks>
    ///	  <para>
    ///	    � �������� ����� ��� �������� � ��������� ���������� ����������
    ///	    �������:
    ///	  </para>
    ///	  <para>
    ///	    select<br />� MON$ATTACHMENTS.MON$ATTACHMENT_ID as ID,<br />�
    ///	    MON$ATTACHMENTS.MON$STATE as STATE,<br />� MON$ATTACHMENTS.MON$USER
    ///	    as DB_USER,<br />� MON$ATTACHMENTS.MON$ROLE as USER_ROLE,<br />�
    ///	    MON$ATTACHMENTS.MON$REMOTE_ADDRESS as REMOTE_ADDRESS,<br />�
    ///	    MON$ATTACHMENTS.MON$TIMESTAMP as CONNECTED,<br />�
    ///	    (current_timestamp - MON$ATTACHMENTS.MON$TIMESTAMP)*24*60*60 as
    ///	    DURATION,<br />��MON$ATTACHMENTS.MON$REMOTE_PROCESS as
    ///	    REMOTE_PROCESS<br />
    ///	  </para>
    ///	  <para>
    ///	    from MON$ATTACHMENTS<br />
    ///	  </para>
    ///	  <para>
    ///	    where<br />� MON$ATTACHMENTS.MON$ATTACHMENT_ID &lt;&gt;
    ///	    current_connection
    ///	  </para>
    ///	</remarks>
    procedure CheckAttachments;

    ///	<summary>
    ///	  ����� ��������� �������������� ������ ���������� �����������
    ///	</summary>
    ///	<param name="Attachment">
    ///	  ID �����������
    ///	</param>
    procedure ShutdownAttachment(const Attachment: Integer);

    ///	<summary>
    ///	  �������� ���� ������
    ///	</summary>
    [TFBDatabaseAttribute('Description')]
    property Description: string read GetDescription write SetDescription;

    ///	<summary>
    ///	  ������ ������� Firebird
    ///	</summary>
    ///	<remarks>
    ///	  ������ ������� Firebird ����� ������ ��� ���������������, ��
    ///	  �����������, ����������� ���� � ���������� ���������� Firebird
    ///	  fbclient.dll
    ///	</remarks>
    [TFBDatabaseAttribute('ServerVersion')]
    property ServerVersion: TFBServerVersion read GetServerVersion
      write SetServerVersion;

    ///	<summary>
    ///	  ������ ����������� � ���� ������
    ///	</summary>
    [TFBDatabaseAttribute('Database')]
    property Database: string read GetDatabase write SetDatabase;

    ///	<summary>
    ///	  ��� ������������, �� ���� �������� ����������� ����������
    ///	</summary>
    ///	<remarks>
    ///	  �������� �� ��������� - SYSDBA
    ///	</remarks>
    [TFBDatabaseAttribute('User_Name')]
    property User: string read GetUser write SetUser;

    ///	<summary>
    ///	  ������ ������������, �� ���� �������� ����������� ����������
    ///	</summary>
    [TFBDatabaseAttribute('Password')]
    property Password: string read GetPassword
      write SetPassword;

    ///	<summary>
    ///	  ���� ������������, �� ���� �������� ����������� ����������
    ///	</summary>
    [TFBDatabaseAttribute('Role')]
    property Role: string read GetRole write SetRole;

    ///	<summary>
    ///	  ���� � ���������� ���������� Firebird fbclient.dll
    ///	</summary>
    [TFBDatabaseAttribute('VendorLib')]
    property ClientLib: string read GetClientLib write SetClientLib;

    ///	<summary>
    ///	  ��������� � ��������� ������, ������������ ��� ��������� ��������� �
    ///	  ���� ������
    ///	</summary>
    ///	<remarks>
    ///	  ���� ������ �� ��������, �� �������� �������� "������" ������
    ///	</remarks>
    property LastErrorMsg: string read FLastErrorMsg;

    ///	<summary>
    ///	  ������ �������� �� �������� ������������ � ���� ������
    ///	</summary>
    property Attachments: TFBAttachments read FAttachments;

    ///	<summary>
    ///	  ��������-�������, ����������� � ������ ����������� ������ �����������
    ///	  � ���� ������
    ///	</summary>
    property OnConnectAttachment: TAttachmentListChangeEvent
      read FOnConnectAttachment write FOnConnectAttachment;

    ///	<summary>
    ///	  ��������-�������, ����������� � ������ �������������������
    ///	  ����������� � ���� ������
    ///	</summary>
    property OnDisConnectAttachment: TAttachmentListChangeEvent
      read FOnDisConnectAttachment write FOnDisConnectAttachment;
  end;

  ///	<summary>
  ///	  ������ ��������������� ������ ��� ������
  ///	</summary>
  TFBDatabaseList = class(TObjectList<TFBDatabase>)
  private
    FOnConnectAttachment: TAttachmentListChangeEvent;
    FOnDisConnectAttachment: TAttachmentListChangeEvent;

    procedure SetOnConnectAttachment(const Value: TAttachmentListChangeEvent);
    procedure SetOnDisConnectAttachment(
      const Value: TAttachmentListChangeEvent);
  protected
    procedure Notify(const Value: TFBDatabase;
      Action: TCollectionNotification); override;
  public
    ///	<summary>
    ///	  ����� ���������, ���������� �� � ������ ��������������� ���������� �
    ///	  ���� � ��������� ������� �����������
    ///	</summary>
    ///	<param name="ADatabase">
    ///	  ������ ����������� � ������� ���� ������
    ///	</param>
    ///	<remarks>
    ///	  ���������� True, ���� ��������������� ���������� ������� ����
    ///	  ���������� � ������
    ///	</remarks>
    function DatabaseExists(const ADatabase: string): Boolean;

    ///	<summary>
    ///	  ����� ��������� ��������������� �������� � ����� ������ � ����
    ///	</summary>
    ///	<param name="AFilename">
    ///	  ��� ����� ������������
    ///	</param>
    procedure SaveToFile(const AFilename: string);

    ///	<summary>
    ///	  ����� ��������� ������ ��������������� �������� � ����� ������ ��
    ///	  �����
    ///	</summary>
    ///	<param name="AFilename">
    ///	  ���� ������������
    ///	</param>
    procedure LoadFromFile(const AFilename: string);

    ///	<summary>
    ///	  ����� ���������� ���������� �������� ����������� � ����� ������ ��
    ///	  ������
    ///	</summary>
    procedure CheckAttachments;

    ///	<summary>
    ///	  ��������-�������, ����������� � ������ ����������� ������ �����������
    ///	  � ���� ������
    ///	</summary>
    property OnConnectAttachment: TAttachmentListChangeEvent
      read FOnConnectAttachment write SetOnConnectAttachment;

    ///	<summary>
    ///	  ��������-�������, ����������� � ������ �������������������
    ///	  ����������� � ���� ������
    ///	</summary>
    property OnDisConnectAttachment: TAttachmentListChangeEvent
      read FOnDisConnectAttachment write SetOnDisConnectAttachment;
  end;

const
  SFBVersions: array [TFBServerVersion] of string = (
    'Unknown', 'Firebird 2.1', 'Firebird 2.5'
  );

  SFBConnectionStates: array [TFBConnectionState] of string = (
    'Idle', 'Active', 'Stalled'
  );

implementation

uses zlib, System.Rtti, System.TypInfo, System.StrUtils, System.IOUtils,
  System.IniFiles, Data.DBXFirebird, Data.DB, Data.DBXCommon, Data.SqlExpr,
  Data.SqlTimSt, Firebird.Tools;

function GetRandomBytes(const ALength: Word): TBytes;
var
  Ch: Byte;
  i: Integer;
begin
  SetLength(Result, ALength);

  if ALength > 0 then
  begin
    for i := 0 to ALength - 1 do
    begin
      Ch := 0;

      while not (Ch in [48..122]) do
        Ch := Random(123);

      Result[i] := Ch;
    end;
  end;
end;

function Encrypt(const AValue: string): TBytes;
var
  EncValue, Prefix, Suffix: TBytes;

  function ConcatBytes(const Bytes1, Bytes2: TBytes): TBytes;
  var
    i: Integer;
  begin
    Result := Bytes1;

    for i := 0 to Length(Bytes2) - 1 do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := Bytes2[i];
    end;
  end;

begin
  SetLength(Result, 0);
  SetLength(EncValue, 0);

  if AValue <> EmptyStr then
    EncValue := ZCompressStr(AValue);

  Prefix := GetRandomBytes(7);
  Suffix := GetRandomBytes(8);

  Result := ConcatBytes(Result, Prefix);
  Result := ConcatBytes(Result, EncValue);
  Result := ConcatBytes(Result, Suffix);
end;

function Decrypt(const AValue: TBytes): string;
var
  i: Integer;
  DecValue: TBytes;
begin
  Result := EmptyStr;

  if Length(AValue) >= 15 then
  begin
    SetLength(DecValue, 0);

    for i := 7 to Length(AValue) - 8 do
    begin
      SetLength(DecValue, Length(DecValue) + 1);
      DecValue[Length(DecValue) - 1] := AValue[i]
    end;

    Result := ZDecompressStr(DecValue);
  end;
end;

{ TFBDatabase }

procedure TFBDatabase.DoAttachListChanged(Sender: TObject;
  const Item: TFBAttachment; Action: TCollectionNotification);
begin
  case Action of
    cnAdded: DoConnectAttachment(Item);
    cnRemoved, cnExtracted: DoDisConnectAttachment(Item);
  end;
end;

function TFBDatabase.DoCheckAttachments(AAttachments: TFBAttachments): Boolean;
var
  SQLConnect: TSQLConnection;
  DBXTrans: TDBXTransaction;
  Attach: TFBAttachment;
  Query: TSQLQuery;
const
  SQuery =
    'select' + sLineBreak +
    '  MON$ATTACHMENTS.MON$ATTACHMENT_ID as ID,' + sLineBreak +
    '  MON$ATTACHMENTS.MON$STATE as STATE,' + sLineBreak +
    '  MON$ATTACHMENTS.MON$USER as DB_USER,' + sLineBreak +
    '  MON$ATTACHMENTS.MON$ROLE as USER_ROLE,' + sLineBreak +
    '  MON$ATTACHMENTS.MON$REMOTE_ADDRESS as REMOTE_ADDRESS,' + sLineBreak +
    '  MON$ATTACHMENTS.MON$TIMESTAMP as CONNECTED,' + sLineBreak +
    '  (current_timestamp - MON$ATTACHMENTS.MON$TIMESTAMP)*24*60*60 as DURATION,' + sLineBreak +
    '  MON$ATTACHMENTS.MON$REMOTE_PROCESS as REMOTE_PROCESS' + sLineBreak +
    '' + sLineBreak +
    'from MON$ATTACHMENTS' + sLineBreak +
    '' + sLineBreak +
    'where' + sLineBreak +
    '  MON$ATTACHMENTS.MON$ATTACHMENT_ID <> current_connection';

begin
  if not Assigned(AAttachments) then Exit(False);
  AAttachments.Clear;

  DBXTrans := nil;
  Query := TSQLQuery.Create(nil);
  SQLConnect := TSQLConnection.Create(nil);
  try
    try
      FLastErrorMsg := EmptyStr;

      SQLConnect.ConnectionName := 'Firebird';
      SQLConnect.DriverName := 'Firebird';
      SQLConnect.LoginPrompt := False;

      SaveToStrings(SQLConnect.Params);

      SQLConnect.Connected := True;
      Result := SQLConnect.Connected;

      if Result then
      begin
        DBXTrans := SQLConnect.BeginTransaction(TDBXIsolations.ReadCommitted);

        Query.SQLConnection := SQLConnect;
        Query.SQL.Text := SQuery;

        Query.Open;
        Result := Query.Active;

        if Result then
        begin
          while not Query.Eof do
          begin
            Attach := TFBAttachment.Create;

            Attach.Id := Query.FieldByName('ID').AsInteger;
            Attach.State := TFBConnectionState(
              Query.FieldByName('STATE').AsInteger);
            Attach.User := Trim(Query.FieldByName('DB_USER').AsString);
            Attach.Role := Trim(Query.FieldByName('USER_ROLE').AsString);
            Attach.RemoteAddress := Trim(
              Query.FieldByName('REMOTE_ADDRESS').AsString);
            Attach.Connected := SQLTimeStampToDateTime(
              Query.FieldByName('CONNECTED').AsSQLTimeStamp);
            Attach.Duration := Trunc(Query.FieldByName('DURATION').AsFloat);
            Attach.RemoteProcess := Trim(UTF8ToString(
              Query.FieldByName('REMOTE_PROCESS').AsString));

            AAttachments.Add(Attach);

            Query.Next;
          end;
        end;

        Query.Close;
      end;

      if Assigned(DBXTrans) then
        SQLConnect.CommitFreeAndNil(DBXTrans);
    except
      on E: Exception do
      begin
        Result := False;
        FLastErrorMsg := E.Message;

        if Assigned(DBXTrans) then
          SQLConnect.RollbackFreeAndNil(DBXTrans);
      end;
    end;
  finally
    SQLConnect.Connected := False;

    FreeAndNil(Query);
    FreeAndNil(SQLConnect);
  end;
end;

procedure TFBDatabase.DoConnectAttachment(AAttachment: TFBAttachment);
begin
  if Assigned(FOnConnectAttachment) then
    FOnConnectAttachment(self, AAttachment);
end;

procedure TFBDatabase.DoDisConnectAttachment(AAttachment: TFBAttachment);
begin
  if Assigned(FOnDisConnectAttachment) then
    FOnDisConnectAttachment(self, AAttachment);
end;

procedure TFBDatabase.CheckAttachments;
var
  Id: Integer;
  Buff: TFBAttachments;
  Item, NewItem: TFBAttachment;
  Disconnected: TList<Integer>;
begin
  Buff := TFBAttachments.Create(True);
  Disconnected := TList<Integer>.Create;
  try
    if DoCheckAttachments(Buff) then
    begin
      if Buff.Count > 0 then
      begin
        for Item in Buff do  // ���������/��������� �������� �� �������� ������������
        begin
          if FAttachments.Exists(Item.Id) then
            FAttachments.Attachment[Item.Id].Assign(Item)
          else
          begin
            NewItem := TFBAttachment.Create;
            NewItem.Assign(Item);

            FAttachments.Add(NewItem)
          end;
        end;

        for Item in FAttachments do // �������� Id �����������, ������������ ���� �������������
        begin
          if not Buff.Exists(Item.Id) then
            Disconnected.Add(Item.Id);
        end;

        if Disconnected.Count > 0 then // ������� �������� � ����������� �����������
        begin
          for id in Disconnected do
            FAttachments.RemoveAttachment(id);
        end;
      end
      else
        FAttachments.Clear;
    end;
  finally
    FreeAndNil(Buff);
    FreeAndNil(Disconnected);
  end;
end;

constructor TFBDatabase.Create;
begin
  inherited Create;

  FParams := TStringList.Create;
  DoInitParams;

  FAttachments := TFBAttachments.Create(True);
  FAttachments.OnNotify := DoAttachListChanged;

  User := 'SYSDBA';
  ServerVersion := fbUnknown;

  FLastErrorMsg := EmptyStr;
end;

destructor TFBDatabase.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FAttachments);

  inherited Destroy;
end;

procedure TFBDatabase.DoInitParams;
var
  RttiCtx: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  Attr: TCustomAttribute;
  AttrName: string;
begin
  FParams.Clear;

  RttiCtx := TRttiContext.Create;
  try
    RttiType := RttiCtx.GetType(self.ClassType);

    for RttiProp in RttiType.GetProperties do
    begin
      for Attr in RttiProp.GetAttributes do
      begin
        if Attr is TFBDatabaseAttribute then
        begin
          AttrName := TFBDatabaseAttribute(Attr).Name;
          FParams.Values[AttrName] := EmptyStr;
        end;
      end;
    end;
  finally
    RttiCtx.Free;
  end;
end;

procedure TFBDatabase.DoShutdownAttachment(AAttachment: Integer);
var
  SQLConnect: TSQLConnection;
  DBXTrans: TDBXTransaction;
  Query: TSQLQuery;
const
  SQuery = 'delete from MON$ATTACHMENTS where MON$ATTACHMENTS.MON$ATTACHMENT_ID = %d';
begin
  if AAttachment <= 0 then Exit;

  DBXTrans := nil;
  Query := TSQLQuery.Create(nil);
  SQLConnect := TSQLConnection.Create(nil);
  try
    try
      FLastErrorMsg := EmptyStr;

      SQLConnect.ConnectionName := 'Firebird';
      SQLConnect.DriverName := 'Firebird';
      SQLConnect.LoginPrompt := False;

      SaveToStrings(SQLConnect.Params);

      SQLConnect.Connected := True;

      if SQLConnect.Connected then
      begin
        DBXTrans := SQLConnect.BeginTransaction(TDBXIsolations.ReadCommitted);

        Query.SQLConnection := SQLConnect;
        Query.SQL.Text := Format(SQuery, [AAttachment]);

        Query.ExecSQL;
      end;

      if Assigned(DBXTrans) then
        SQLConnect.CommitFreeAndNil(DBXTrans);
    except
      on E: Exception do
      begin
        FLastErrorMsg := E.Message;

        if Assigned(DBXTrans) then
          SQLConnect.RollbackFreeAndNil(DBXTrans);
      end;
    end;
  finally
    SQLConnect.Connected := False;

    FreeAndNil(Query);
    FreeAndNil(SQLConnect);
  end;
end;

function TFBDatabase.GetAttributeName(APropName: string): string;
var
  RttiCtx: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
  Attr: TCustomAttribute;
begin
  RttiCtx := TRttiContext.Create;
  try
    RttiType := RttiCtx.GetType(self.ClassType);

    for RttiProp in RttiType.GetProperties do
    begin
      if AnsiCompareText(RttiProp.Name, APropName) = 0 then
      begin
        for Attr in RttiProp.GetAttributes do
        begin
          if Attr is TFBDatabaseAttribute then
            Result := TFBDatabaseAttribute(Attr).Name;
        end;
      end;
    end;
  finally
    RttiCtx.Free;
  end;
end;

function TFBDatabase.GetClientLib: string;
begin
  Result := self.Value['ClientLib'];
end;

function TFBDatabase.GetDatabase: string;
begin
  Result := self.Value['Database'];
end;

function TFBDatabase.GetDescription: string;
begin
  Result := self.Value['Description'];
end;

function TFBDatabase.GetPassword: string;
begin
  Result := self.Value['Password'];
end;

function TFBDatabase.GetPropertyValue(APropName: string): string;
var
  ParamName: string;
begin
  ParamName := AttributeName[APropName];
  Result := FParams.Values[ParamName];
end;

function TFBDatabase.GetRole: string;
begin
  Result := self.Value['Role'];
end;

function TFBDatabase.GetServerVersion: TFBServerVersion;
var
  SFBVer: string;
  FBVer: TFBServerVersion;
begin
  SFBVer := self.Value['ServerVersion'];

  for FBVer := Low(TFBServerVersion) to High(TFBServerVersion) do
  begin
    if AnsiCompareText(SFBVer, SFBVersions[FBVer]) = 0 then
      Exit(FBVer);
  end;

  Result := fbUnknown;
end;

function TFBDatabase.GetUser: string;
begin
  Result := self.Value['User'];
end;

procedure TFBDatabase.LoadFromStrings(ASource: TStrings);
var
  i: Integer;
  ParamName, ParamValue: string;
begin
  FParams.Clear;

  if not Assigned(ASource) then Exit;
  if ASource.Count = 0 then Exit;

  for i := 0 to ASource.Count - 1 do
  begin
    ParamName := ASource.Names[i];
    ParamValue := ASource.Values[ParamName];

    FParams.Values[ParamName] := ParamValue;
  end;
end;

procedure TFBDatabase.SaveToStrings(ADest: TStrings);
var
  i: Integer;
  ParamName, ParamValue: string;
begin
  if not Assigned(ADest) then Exit;

  for i := 0 to FParams.Count - 1 do
  begin
    ParamName := FParams.Names[i];
    ParamValue := FParams.Values[ParamName];

    ADest.Values[ParamName] := ParamValue;
  end;
end;

procedure TFBDatabase.SetClientLib(const Value: string);
begin
  self.Value['ClientLib'] := Value;
end;

procedure TFBDatabase.SetDatabase(const Value: string);
begin
  self.Value['Database'] := Value;
end;

procedure TFBDatabase.SetDescription(const Value: string);
begin
  self.Value['Description'] := Value;
end;

procedure TFBDatabase.SetPassword(const Value: string);
begin
  self.Value['Password'] := Value;
end;

procedure TFBDatabase.SetPropertyValue(APropName: string; const Value: string);
var
  ParamName: string;
begin
  ParamName := AttributeName[APropName];
  FParams.Values[ParamName] := Value;
end;

procedure TFBDatabase.SetRole(const Value: string);
begin
  self.Value['Role'] := Value;
end;

procedure TFBDatabase.SetServerVersion(const Value: TFBServerVersion);
begin
  self.Value['ServerVersion'] := SFBVersions[Value];

  if ClientLib.Trim = EmptyStr then
  begin
    case Value of
      fb21: ClientLib := TFirebird.FBClientLib('2.1');
      fb25: ClientLib := TFirebird.FBClientLib('2.5');
    end;
  end;
end;

procedure TFBDatabase.SetUser(const Value: string);
begin
  self.Value['User'] := Value;
end;

procedure TFBDatabase.ShutdownAttachment(const Attachment: Integer);
begin
  DoShutdownAttachment(Attachment);
end;

function TFBDatabase.TestConnection: Boolean;
var
  SQLConnect: TSQLConnection;
begin
  SQLConnect := TSQLConnection.Create(nil);
  try
    try
      FLastErrorMsg := EmptyStr;

      SQLConnect.ConnectionName := 'Firebird';
      SQLConnect.DriverName := 'Firebird';
      SQLConnect.LoginPrompt := False;

      SaveToStrings(SQLConnect.Params);

      SQLConnect.Connected := True;
      Result := SQLConnect.Connected;
    except
      on E: Exception do
      begin
        Result := False;
        FLastErrorMsg := E.Message;
      end;
    end;
  finally
    SQLConnect.Connected := False;
    FreeAndNil(SQLConnect);
  end;
end;

{ TFBDatabaseAttribute }

constructor TFBDatabaseAttribute.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

{ TFBDatabaseList }

procedure TFBDatabaseList.CheckAttachments;
var
  Db: TFBDatabase;
begin
  for Db in self do
    Db.CheckAttachments;
end;

function TFBDatabaseList.DatabaseExists(const ADatabase: string): Boolean;
var
  Db: TFBDatabase;
begin
  if self.Count = 0 then Exit(False);
  if ADatabase = EmptyStr then Exit(False);

  for Db in self do
  begin
    if AnsiCompareText(Db.Database.Trim, ADatabase.Trim) = 0 then
      Exit(True);
  end;

  Result := False;
end;

procedure TFBDatabaseList.LoadFromFile(const AFilename: string);
var
  Cfg: TIniFile;
  Info: TStrings;
  Passwd: TBytes;
  Db: TFBDatabase;
  Section, Password: string;
  BStream: TBytesStream;
  i, DatabaseCount: Integer;
begin
  self.Clear;
  if not TFile.Exists(AFilename) then Exit;

  Info := TStringList.Create;
  Cfg := TIniFile.Create(AFilename);
  try
    DatabaseCount := Cfg.ReadInteger('General', 'DatabaseCount', 0);

    for i := 1 to DatabaseCount do
    begin
      Section := Format('Database_%d', [i]);

      Info.Clear;
      Cfg.ReadSectionValues(Section, Info);

      BStream := TBytesStream.Create();
      try
        Cfg.ReadBinaryStream(Section, 'Password', BStream);

        if BStream.Size > 0 then
        begin
          BStream.Position := 0;
          Passwd := BStream.Bytes;

          SetLength(Passwd, BStream.Size);
          Password := DeCrypt(Passwd);

          Info.Values['Password'] := Password;
        end;

        Db := TFBDatabase.Create;
        Db.LoadFromStrings(Info);

        self.Add(Db);
      finally
        FreeAndNil(BStream);
      end;
    end;
  finally
    FreeAndNil(Info);
    FreeAndNil(Cfg);
  end;
end;

procedure TFBDatabaseList.Notify(const Value: TFBDatabase;
  Action: TCollectionNotification);
begin
  if Action = cnAdded then
  begin
    Value.OnConnectAttachment := FOnConnectAttachment;
    Value.OnDisConnectAttachment := FOnDisConnectAttachment;
  end;

  inherited;
end;

procedure TFBDatabaseList.SaveToFile(const AFilename: string);
var
  i, j: Integer;
  Cfg: TIniFile;
  Info: TStrings;
  Db: TFBDatabase;
  BStream: TBytesStream;
  Section, ParamName, ParamValue: string;
begin
  if TFile.Exists(AFilename) then
    TFile.Delete(AFilename);

  Info := TStringList.Create;
  Cfg := TIniFile.Create(AFilename);
  try
    Cfg.WriteInteger('General', 'DatabaseCount', self.Count);

    for i := 0 to self.Count - 1 do
    begin
      Section := Format('Database_%d', [i + 1]);
      Db := self.Items[i];

      Info.Clear;
      Db.SaveToStrings(Info);

      for j := 0 to Info.Count - 1 do
      begin
        ParamName := Info.Names[j];
        ParamValue := Info.Values[ParamName];

        if AnsiCompareText(ParamName, 'password') <> 0 then
          Cfg.WriteString(Section, ParamName, ParamValue)
        else
        begin
          BStream := TBytesStream.Create(Encrypt(ParamValue));
          try
            Cfg.WriteBinaryStream(Section, ParamName, BStream);
          finally
            FreeAndNil(BStream);
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(Cfg);
    FreeAndNil(Info);
  end;
end;

procedure TFBDatabaseList.SetOnConnectAttachment(
  const Value: TAttachmentListChangeEvent);
var
  Item: TFBDatabase;
begin
  FOnConnectAttachment := Value;

  for Item in self do
    Item.OnConnectAttachment := FOnConnectAttachment;
end;

procedure TFBDatabaseList.SetOnDisConnectAttachment(
  const Value: TAttachmentListChangeEvent);
var
  Item: TFBDatabase;
begin
  FOnDisConnectAttachment := Value;

  for Item in self do
    Item.OnDisConnectAttachment := FOnDisConnectAttachment;
end;

{ TFBAttachments }

function TFBAttachments.Exists(const AttachmentId: Cardinal): Boolean;
begin
  Result := Assigned(Attachment[AttachmentId]);
end;

function TFBAttachments.GetAttachment(ID: Cardinal): TFBAttachment;
var
  Attach: TFBAttachment;
begin
  Result := nil;

  for Attach in self do
  begin
    if Attach.Id = ID then
      Exit(Attach);
  end;
end;

procedure TFBAttachments.RemoveAttachment(const AttachmentId: Cardinal);
var
  Item: TFBAttachment;
begin
  for Item in self do
  begin
    if Item.Id = AttachmentId then
    begin
      self.Remove(Item);
      Exit;
    end;
  end;
end;

{ TFBAttachment }

procedure TFBAttachment.Assign(Source: TPersistent);
begin
  if Source is TFBAttachment then
  begin
    Id := TFBAttachment(Source).Id;
    State := TFBAttachment(Source).State;
    User := TFBAttachment(Source).User;
    Role := TFBAttachment(Source).Role;
    RemoteAddress := TFBAttachment(Source).RemoteAddress;
    Connected := TFBAttachment(Source).Connected;
    Duration := TFBAttachment(Source).Duration;
    RemoteProcess := TFBAttachment(Source).RemoteProcess;
  end
  else
    inherited Assign(Source);
end;

constructor TFBAttachment.Create;
begin
  inherited Create;

  FId := 0;
  FState := fbcsActive;
  FUser := EmptyStr;
  FRole := EmptyStr;
  FRemoteAddress := EmptyStr;
  FConnected := 0;
  FDuration := 0;
end;

end.
