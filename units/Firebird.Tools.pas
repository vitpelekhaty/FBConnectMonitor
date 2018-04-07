unit Firebird.Tools;

interface

uses System.SysUtils, System.Classes;

type
  ///	<summary>
  ///	  ����������� �����, ��������������� API ��� ��������� ���������
  ///	  ���������� �� ������������� � ������� �������/������� Firebird
  ///	</summary>
  TFirebird = record
  private
    class function GetInstances(AInstances: TStrings): Boolean; static;
    class function GetInstance(const Version: string): string; static;
    class function GetFBLibVersion(const AFBLib: string): string; static;
  public
    ///	<summary>
    ///	  ����� ���������, ����������� �� ������������� ������ Firebird ��
    ///	  �������
    ///	</summary>
    ///	<param name="Version">
    ///	  ������������� ������ Firebird
    ///	</param>
    ///	<remarks>
    ///	  ����� �� ��������� ��������� � ���������� ����� Firebird. ���������,
    ///	  ��� Firebird ����������, ���� ������� ���������� fbclient.dll
    ///	  ��������� ������
    ///	</remarks>
    class function Exists(const Version: string): Boolean; static;

    ///	<summary>
    ///	  ����� ���������� ���� � ���������� ���������� fbclient.dll ���������
    ///	  ������ Firebird
    ///	</summary>
    ///	<param name="Version">
    ///	  ��������� ������ Firebird
    ///	</param>
    ///	<remarks>
    ///	  ����� ������ ������ ������, ���� ��������� ������ Firebird ��
    ///	  ����������� � �������
    ///	</remarks>
    class function FBClientLib(const Version: string): string; static;

    ///	<summary>
    ///	  ����� ������� ������ ���������� � �������� ���� ������ ���
    ///	  ����������� ���� Firebird, ��������� ��� (IP-�����) �������, �����
    ///	  ����� � ��������� (���� � �����) ���� ������
    ///	</summary>
    ///	<param name="Host">
    ///	  ��� (IP-�����) �������
    ///	</param>
    ///	<param name="FilenameOrAlias">
    ///	  ��������� (���� � �����) ���� ������
    ///	</param>
    ///	<param name="Port">
    ///	  ���� (�� ��������� - 3050)
    ///	</param>
    class function ConnectionString(const Host, FilenameOrAlias: string;
      const Port: Word = 3050): string; static;

    ///	<summary>
    ///	  ����� ��������� ������ ������ ���������� � ����� ������ ���
    ///	  ����������� ���� Firebird, �, ���� ������ �������� �������,
    ///	  ���������� ����, ���� � ��������� (���� � �����) ���� ������
    ///	</summary>
    ///	<param name="ConnectionString">
    ///	  ������ ����������
    ///	</param>
    ///	<param name="Host">
    ///	  ����
    ///	</param>
    ///	<param name="FilenameOrAlias">
    ///	  ��������� (���� � �����) ���� ������
    ///	</param>
    ///	<param name="Port">
    ///	  ����
    ///	</param>
    ///	<remarks>
    ///	  <para>
    ///	    ���������� True, ���� ConnectionString �������� ���������� ������
    ///	    ����������.
    ///	  </para>
    ///	  <para>
    ///	    ���� ����� �� ������ ��� (IP-�����) �����, �� ���������, ��� ������
    ///	    ���� ������ �������� �� ��� �� ������, ��� � ����������.
    ///	  </para>
    ///	  <para>
    ///	    ���� ���� �� ��� ������ � ������ ����������, �� ������������ ����
    ///	    �� ��������� 3050
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
