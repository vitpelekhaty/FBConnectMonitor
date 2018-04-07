unit VerInfo;

interface

uses SysUtils, WinTypes, Dialogs, Classes;

type
  EVerInfoError = class(Exception);
  ENoVerInfoError = class(Exception);
  ENoFixedVerInfo = class(Exception);

type
  TVerInfoType = (viCompanyName, viFileDescription, viFileVersion,
                  viInternalName, viLegalCopyright, viLegalTrademarks,
                  viOriginalFileName, viProductName, viProductVersion,
                  viComments);

  TVerFileFlags = (ffPreRelease, ffPrivateBuild, ffSpecialBuild, ffDebug);
  TSetVerFileFlags = set of TVerFileFlags;
  
const
  VerNameArray: array [viCompanyName..viComments] of String[20] =
    ('CompanyName',
     'FileDescription',
     'FileVersion',
     'InternalName',
     'LegalCopyright',
     'LegalTrademarks',
     'OriginalFilename',
     'ProductName',
     'ProductVersion',
     'Comments');

type
  TVerInfoRes = class
    Handle: DWORD;
    Size: Integer;
    RezBuffer: String;
    TransTable: PLongInt;
    FixedFileInfoBuf: PVSFixedFileInfo;
    FFileFlags: TSetVerFileFlags;
    FFileName: String;

    procedure FillFixedFileInfoBuf;
    procedure FillFileVersionInfo;
    procedure FillFileMaskInfo;
  private
    function GetFileBuild: Cardinal;
    function GetFileMajorVersion: Cardinal;
    function GetFileMinorVersion: Cardinal;
    function GetFileRelease: Cardinal;
    function GetProductBuild: Cardinal;
    function GetProductMajorVersion: Cardinal;
    function GetProductMinorVersion: Cardinal;
    function GetProductRelease: Cardinal;
    function GetComments: string;
  protected
    function GetFileVersion: String;
    function GetProductVersion: String;
    function GetFileOS: DWORD;
  public
    constructor Create(AFileName: String);
    destructor Destroy; override;

    class function VersionResourceAvailable(const AFilename: string): Boolean;

    function GetPreDefKeyString(AVerKind: TVerInfoType): String;
    function GetUserDefKeyString(AKey: String): String;

    property FileVersion: String read GetFileVersion;
    property ProductVersion: String read GetProductVersion;
    property FileFlags: TSetVerFileFlags read FFIleFlags;
    property FileOS: DWORD read GetFileOS;

    property FileVersionMajor: Cardinal read GetFileMajorVersion;
    property FileVersionMinor: Cardinal read GetFileMinorVersion;
    property FileVersionRelease: Cardinal read GetFileRelease;
    property FileVersionBuild: Cardinal read GetFileBuild;

    property ProductVersionMajor: Cardinal read GetProductMajorVersion;
    property ProductVersionMinor: Cardinal read GetProductMinorVersion;
    property ProductVersionRelease: Cardinal read GetProductRelease;
    property ProductVersionBuild: Cardinal read GetProductBuild;

    property Comments: string read GetComments;
  end;

implementation

uses Windows;

const
  SFInfo = '\StringFileInfo\';
  VerTranslation: PChar = '\VarFileInfo\Translation';
  FormatStr = '%s%.4x%.4x\%s%s';

function VersionString(ms, ls: DWORD): String;
begin
  Result := Format('%d.%d.%d.%d', [HIWORD(ms), LOWORD(ms), HIWORD(ls), LOWORD(ls)]);
end;

{ TVerInfoRes }

constructor TVerInfoRes.Create(AFileName: String);
begin
  Handle := 0;
  
  FFileName := AFileName;
  FFileFlags := [];

  FillFileVersionInfo;
  FillFixedFileInfoBuf;
  FillFileMaskInfo;
end;

destructor TVerInfoRes.Destroy;
begin
  inherited;
end;

procedure TVerInfoRes.FillFileMaskInfo;
begin
  with FixedFileInfoBuf^ do
    begin
      if (dwFileFlagsMask and dwFileFlags and VS_FF_PRERELEASE) <> 0 then
        FFileFlags := FFileFlags + [ffPreRelease];

      if (dwFileFlagsMask and dwFileFlags and VS_FF_PRIVATEBUILD) <> 0 then
        FFileFlags := FFileFlags + [ffPrivateBuild];

      if (dwFileFlagsMask and dwFileFlags and VS_FF_SPECIALBUILD) <> 0 then
        FFileFlags := FFileFlags + [ffSpecialBuild];

      if (dwFileFlagsMask and dwFileFlags and VS_FF_DEBUG) <> 0 then
        FFileFlags := FFileFlags + [ffDebug];
    end;
end;

procedure TVerInfoRes.FillFileVersionInfo;
var
  SBSize: UINT;
begin
  Size := GetFileVersionInfoSize(PChar(FFileName), Handle);

  if Size <= 0 then
    raise ENoVerInfoError.Create('No VersionInfo available!');

  SetLength(RezBuffer, Size);

  if not GetFileVersionInfo(PChar(FFileName), Handle, Size, PChar(RezBuffer)) then
    raise EVerInfoError.Create('Cannot obtain VersionInfo!');

  if not VerQueryValue(PChar(RezBuffer), VerTranslation, pointer(TransTable), SBSize) then
    raise EVerInfoError.Create('No language info!');
end;

procedure TVerInfoRes.FillFixedFileInfoBuf;
var
  Size: UINT;
begin
  if VerQueryValue(PChar(RezBuffer), '\', pointer(FixedFileInfoBuf), Size) then
    begin
      if Size < SizeOf(TVSFixedFileInfo) then
        raise ENoFixedVerInfo.Create('No fixed file info!');
    end
  else
    raise ENoFixedVerInfo.Create('No fixed file info!');
end;

function TVerInfoRes.GetComments: string;
begin
  Result := GetPreDefKeyString(viComments);
end;

function TVerInfoRes.GetFileBuild: Cardinal;
begin
  Result := LOWORD(FixedFileInfoBuf^.dwFileVersionLS);
end;

function TVerInfoRes.GetFileMajorVersion: Cardinal;
begin
  Result := HIWORD(FixedFileInfoBuf^.dwFileVersionMS);
end;

function TVerInfoRes.GetFileMinorVersion: Cardinal;
begin
  Result := LOWORD(FixedFileInfoBuf^.dwFileVersionMS);
end;

function TVerInfoRes.GetFileOS: DWORD;
begin
  Result := FixedFileInfoBuf^.dwFileOS;
end;

function TVerInfoRes.GetFileRelease: Cardinal;
begin
  Result := HIWORD(FixedFileInfoBuf^.dwFileVersionLS);
end;

function TVerInfoRes.GetFileVersion: String;
begin
  with FixedFileInfoBuf^ do
    Result := VersionString(dwFileVersionMS, dwFileVersionLS); 
end;

function TVerInfoRes.GetPreDefKeyString(AVerKind: TVerInfoType): String;
var
  p: PChar;
  s: UINT;
begin
  Result := Format(FormatStr, [SfInfo, LOWORD(TransTable^), HIWORD(TransTable^), VerNameArray[aVerKind], #0]);

  if VerQueryValue(PChar(RezBuffer), @Result[1], pointer(p), s) then
    Result := StrPas(p)
  else
    Result := '';
end;

function TVerInfoRes.GetProductBuild: Cardinal;
begin
  Result := LOWORD(FixedFileInfoBuf^.dwProductVersionLS);
end;

function TVerInfoRes.GetProductMajorVersion: Cardinal;
begin
  Result := HIWORD(FixedFileInfoBuf^.dwProductVersionMS);
end;

function TVerInfoRes.GetProductMinorVersion: Cardinal;
begin
  Result := LOWORD(FixedFileInfoBuf^.dwProductVersionMS);
end;

function TVerInfoRes.GetProductRelease: Cardinal;
begin
  Result := HIWORD(FixedFileInfoBuf^.dwProductVersionLS);
end;

function TVerInfoRes.GetProductVersion: String;
begin
  with FixedFileInfoBuf^ do
    Result := VersionString(dwProductVersionMS, dwProductVersionLS);
end;

function TVerInfoRes.GetUserDefKeyString(AKey: String): String;
var
  p: PChar;
  s: UINT;
begin
  Result := Format(FormatStr, [SfInfo, LOWORD(TransTable^), HIWORD(TransTable^), AKey, #0]);

  if VerQueryValue(PChar(RezBuffer), @Result[1], pointer(p), s) then
    Result := StrPas(p)
  else
    Result := '';
end;

class function TVerInfoRes.VersionResourceAvailable(
  const AFilename: string): Boolean;
var
  Size: UINT;
  hFile: Cardinal;
  Buffer: String;
begin
  Result := False;
  try
    if FileExists(AFilename) then
    begin
      Size := GetFileVersionInfoSize(PChar(AFileName), hFile);
      Result := Size > 0;

      if Result then
      begin
        SetLength(Buffer, Size);
        Result := GetFileVersionInfo(PChar(AFileName), hFile, Size, PChar(Buffer));
      end;
    end;
  except
    Result := False;
  end;
end;

end.
