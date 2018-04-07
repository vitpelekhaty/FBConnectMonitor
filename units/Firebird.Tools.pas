unit Firebird.Tools;

interface

uses System.SysUtils, System.Classes;

type
  ///	<summary>
  ///	  Статический класс, предоставляющий API для получения служебной
  ///	  информации об установленном в системе сервере/клиенте Firebird
  ///	</summary>
  TFirebird = record
  private
    class function GetInstances(AInstances: TStrings): Boolean; static;
    class function GetInstance(const Version: string): string; static;
    class function GetFBLibVersion(const AFBLib: string): string; static;
  public
    ///	<summary>
    ///	  Метод проверяет, установлена ли запрашиваемая версия Firebird на
    ///	  системе
    ///	</summary>
    ///	<param name="Version">
    ///	  Запрашиваемая версия Firebird
    ///	</param>
    ///	<remarks>
    ///	  Метод не разделяет серверную и клиентскую части Firebird. Считается,
    ///	  что Firebird установлен, если найдена библиотека fbclient.dll
    ///	  требуемой версии
    ///	</remarks>
    class function Exists(const Version: string): Boolean; static;

    ///	<summary>
    ///	  Метод возвращает путь к клиентской библиотеке fbclient.dll требуемой
    ///	  версии Firebird
    ///	</summary>
    ///	<param name="Version">
    ///	  Требуемая версия Firebird
    ///	</param>
    ///	<remarks>
    ///	  Метод вернет пустую строку, если требуемая версия Firebird не
    ///	  установлена в системе
    ///	</remarks>
    class function FBClientLib(const Version: string): string; static;

    ///	<summary>
    ///	  Метод создает строку соединения с сервером базы данных под
    ///	  управлением СУБД Firebird, используя имя (IP-адрес) сервера, номер
    ///	  порта и псевдоним (путь к файлу) базы данных
    ///	</summary>
    ///	<param name="Host">
    ///	  Имя (IP-адрес) сервера
    ///	</param>
    ///	<param name="FilenameOrAlias">
    ///	  Псевдоним (путь к файлу) базы данных
    ///	</param>
    ///	<param name="Port">
    ///	  Порт (по умолчанию - 3050)
    ///	</param>
    class function ConnectionString(const Host, FilenameOrAlias: string;
      const Port: Word = 3050): string; static;

    ///	<summary>
    ///	  Метод выполняет разбор строки соединения с базой данных под
    ///	  управлением СУБД Firebird, и, если разбор выполнен успешно,
    ///	  возвращает хост, порт и псведоним (путь к файлу) базы данных
    ///	</summary>
    ///	<param name="ConnectionString">
    ///	  Строка соединения
    ///	</param>
    ///	<param name="Host">
    ///	  Хост
    ///	</param>
    ///	<param name="FilenameOrAlias">
    ///	  Псевдоним (путь к файлу) базы данных
    ///	</param>
    ///	<param name="Port">
    ///	  Порт
    ///	</param>
    ///	<remarks>
    ///	  <para>
    ///	    Возвращает True, если ConnectionString содержит корректную строку
    ///	    соединения.
    ///	  </para>
    ///	  <para>
    ///	    Если метод не вернул имя (IP-адрес) хоста, то считается, что сервер
    ///	    базы данных работает на той же машине, что и приложение.
    ///	  </para>
    ///	  <para>
    ///	    Если порт не был указан в строке соединения, то возвращается порт
    ///	    по умолчанию 3050
    ///	  </para>
    ///	</remarks>
    class function ParseConnectionString(const ConnectionString: string;
      out Host, FilenameOrAlias: string; out Port: Word): Boolean; static;
  end;

implementation

uses System.IOUtils, System.StrUtils, System.Win.Registry, WinApi.Windows,
  System.RegularExpressions, VerInfo;

const
  SFirebirdInstancesKey = '\SOFTWARE\Firebird Project\Firebird Server\Instances';
  SFBLib = 'bin\fbclient.dll';

{ TFirebird }

class function TFirebird.ConnectionString(const Host, FilenameOrAlias: string;
  const Port: Word): string;
begin
  Result := FilenameOrAlias;

  if Host <> EmptyStr then
    Result := Format('%s/%d:%s', [Host, Port, Result]);
end;

class function TFirebird.Exists(const Version: string): Boolean;
begin
  Result := TFile.Exists(FBClientLib(Version));
end;

class function TFirebird.FBClientLib(const Version: string): string;
begin
  if Version = EmptyStr then Exit(EmptyStr);

  Result := GetInstance(Version);

  if Result <> EmptyStr then
    Result := IncludeTrailingPathDelimiter(Result) + SFBLib;
end;

class function TFirebird.GetFBLibVersion(const AFBLib: string): string;
var
  Ver: TVerInfoRes;
begin
  if not TFile.Exists(AFBLib) then Exit(EmptyStr);

  Ver := TVerInfoRes.Create(AFBLib);
  try
    Result := Ver.FileVersion;
  finally
    FreeAndNil(Ver);
  end;
end;

class function TFirebird.GetInstance(const Version: string): string;
var
  Instances: TStrings;
  Value, FBLib, InstVersion: string;
begin
  if Version = EmptyStr then Exit(EmptyStr);

  Instances := TStringList.Create;
  try
    if GetInstances(Instances) then
    begin
      for Value in Instances do
      begin
        FBLib := IncludeTrailingPathDelimiter(Value) + SFBLib;

        if TFile.Exists(FBLib) then
        begin
          InstVersion := GetFBLibVersion(FBLib);

          if AnsiStartsStr(Version, InstVersion) then
          begin
            Result := Value;
            Break;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(Instances);
  end;
end;

class function TFirebird.GetInstances(AInstances: TStrings): Boolean;
var
  Reg: TRegistry;
  Values: TStrings;
  Value, FBInstance: string;
begin
  if not Assigned(AInstances) then Exit(False);

  Values := TStringList.Create;
  Reg := TRegistry.Create;
  try
    Reg.Access := KEY_READ;
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.KeyExists(SFirebirdInstancesKey) then
    begin
      if Reg.OpenKeyReadOnly(SFirebirdInstancesKey) then
      begin
        Reg.GetValueNames(Values);

        for Value in Values do
        begin
          FBInstance := Reg.ReadString(Value);

          if FBInstance <> EmptyStr then
          begin
            if TDirectory.Exists(FBInstance) and (AInstances.IndexOf(FBInstance) < 0) then
              AInstances.Add(FBInstance);
          end;
        end;
      end;
    end;

    Result := AInstances.Count > 0;
  finally
    FreeAndNil(Reg);
    FreeAndNil(Values);
  end;
end;

class function TFirebird.ParseConnectionString(const ConnectionString: string;
  out Host, FilenameOrAlias: string; out Port: Word): Boolean;
var
  RegEx: TRegEx;
  Match: TMatch;
  Temp: string;
const
  SPattern = '(^([.A-Za-z0-9]+)[\/]*([\d]*)[\:]+([A-Za-z0-9\-\_]+)$)|^([A-Za-z0-9\-\_]+)$';
begin
  if ConnectionString = EmptyStr then Exit(False);

  RegEx := TRegEx.Create(SPattern, [roIgnoreCase]);

  Result := RegEx.IsMatch(ConnectionString);

  if Result then
  begin
    Match := RegEx.Match(ConnectionString);

    if Match.Groups.Count > 1 then
    begin
      Host := Match.Groups[2].Value;
      Temp := Match.Groups[3].Value;
      FilenameOrAlias := Match.Groups[4].Value;

      if FilenameOrAlias = EmptyStr then
        FilenameOrAlias := Match.Groups[5].Value;

      Port := StrToIntDef(Temp, 3050);
    end;
  end;
end;

end.
