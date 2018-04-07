///	<summary>
///	  Модуль содержит описание классов для получения сведений о версии
///	  приложения
///	</summary>
unit AppInfo;

interface

uses
  System.SysUtils, System.Classes, VerInfo;

type
  ///	<summary>
  ///	  Класс, обеспечивающий получение сведений о версии программы, а также
  ///	  дополнительных сведений о приложении и его разработчике
  ///	</summary>
  TApplicationInfo = record
  private
    class function ApplicationExeName: string; static;
  public
    const
      AppGUID = '{977D2D66-DBCE-44D7-979C-41EFDD52AD29}';

      AppPublisherUrl = 'www.somesite.ru';
      AppSupportUrl = 'www.somesite.ru/support';
      AppUpdatesUrl = 'www.somesite.ru/support/updates/idb';
      AppWikiUrl = 'www.somesite.ru/support/wiki/idb';
      AppContact = 'somesite@somesite.ru';

    ///	<summary>
    ///	  Версия файла приложения
    ///	</summary>
    class function FileVersion: String; static;

    ///	<summary>
    ///	  Версия программного продукта
    ///	</summary>
    class function ProductVersion: String; static;

    ///	<summary>
    ///	  Старший номер версии файла приложения
    ///	</summary>
    class function FileVersionMajor: Cardinal; static;

    ///	<summary>
    ///	  Младший номер версии файла приложения
    ///	</summary>
    class function FileVersionMinor: Cardinal; static;

    ///	<summary>
    ///	  Номер релиза версии файла приложения
    ///	</summary>
    class function FileVersionRelease: Cardinal; static;

    ///	<summary>
    ///	  Номер сборки файла приложения
    ///	</summary>
    class function FileVersionBuild: Cardinal; static;

    ///	<summary>
    ///	  Старший номер версии программного продукта
    ///	</summary>
    class function ProductVersionMajor: Cardinal; static;

    ///	<summary>
    ///	  Младший номер версии программного продукта
    ///	</summary>
    class function ProductVersionMinor: Cardinal; static;

    ///	<summary>
    ///	  Номер релиза программного продукта
    ///	</summary>
    class function ProductVersionRelease: Cardinal; static;

    ///	<summary>
    ///	  Номер сборки программного продукта
    ///	</summary>
    class function ProductVersionBuild: Cardinal; static;

    ///	<summary>
    ///	  Комментарии к приложению
    ///	</summary>
    class function Comments: string; static;

    ///	<summary>
    ///	  Дата компиляции приложения
    ///	</summary>
    ///	<remarks>
    ///	  В ресурсе должно храниться значение "Last Compile"
    ///	</remarks>
    class function LastCompile: string; static;
  end;

implementation

uses Forms;

{ TApplicationInfo }

class function TApplicationInfo.ApplicationExeName: string;
begin
  Result := Application.ExeName;
end;

class function TApplicationInfo.Comments: string;
var
  vir: TVerInfoRes;
begin
  Result := EmptyStr;
  if not TVerInfoRes.VersionResourceAvailable(ApplicationExeName) then Exit;

  vir := TVerInfoRes.Create(ApplicationExeName);
  try
    Result := vir.Comments;
  finally
    FreeAndNil(vir);
  end;
end;

class function TApplicationInfo.FileVersion: String;
var
  vir: TVerInfoRes;
begin
  Result := EmptyStr;
  if not TVerInfoRes.VersionResourceAvailable(ApplicationExeName) then Exit;

  vir := TVerInfoRes.Create(ApplicationExeName);
  try
    Result := vir.FileVersion;
  finally
    FreeAndNil(vir);
  end;
end;

class function TApplicationInfo.FileVersionBuild: Cardinal;
var
  vir: TVerInfoRes;
begin
  Result := 0;
  if not TVerInfoRes.VersionResourceAvailable(ApplicationExeName) then Exit;

  vir := TVerInfoRes.Create(ApplicationExeName);
  try
    Result := vir.FileVersionBuild;
  finally
    FreeAndNil(vir);
  end;
end;

class function TApplicationInfo.FileVersionMajor: Cardinal;
var
  vir: TVerInfoRes;
begin
  Result := 0;
  if not TVerInfoRes.VersionResourceAvailable(ApplicationExeName) then Exit;

  vir := TVerInfoRes.Create(ApplicationExeName);
  try
    Result := vir.FileVersionMajor;
  finally
    FreeAndNil(vir);
  end;
end;

class function TApplicationInfo.FileVersionMinor: Cardinal;
var
  vir: TVerInfoRes;
begin
  Result := 0;
  if not TVerInfoRes.VersionResourceAvailable(ApplicationExeName) then Exit;

  vir := TVerInfoRes.Create(ApplicationExeName);
  try
    Result := vir.FileVersionMinor;
  finally
    FreeAndNil(vir);
  end;
end;

class function TApplicationInfo.FileVersionRelease: Cardinal;
var
  vir: TVerInfoRes;
begin
  Result := 0;
  if not TVerInfoRes.VersionResourceAvailable(ApplicationExeName) then Exit;

  vir := TVerInfoRes.Create(ApplicationExeName);
  try
    Result := vir.FileVersionRelease;
  finally
    FreeAndNil(vir);
  end;
end;

class function TApplicationInfo.LastCompile: string;
var
  vir: TVerInfoRes;
begin
  Result := EmptyStr;
  if not TVerInfoRes.VersionResourceAvailable(ApplicationExeName) then Exit;

  vir := TVerInfoRes.Create(ApplicationExeName);
  try
    Result := vir.GetUserDefKeyString('Last Compile');
  finally
    FreeAndNil(vir);
  end;
end;

class function TApplicationInfo.ProductVersion: String;
var
  vir: TVerInfoRes;
begin
  Result := EmptyStr;
  if not TVerInfoRes.VersionResourceAvailable(ApplicationExeName) then Exit;

  vir := TVerInfoRes.Create(ApplicationExeName);
  try
    Result := vir.ProductVersion;
  finally
    FreeAndNil(vir);
  end;
end;

class function TApplicationInfo.ProductVersionBuild: Cardinal;
var
  vir: TVerInfoRes;
begin
  Result := 0;
  if not TVerInfoRes.VersionResourceAvailable(ApplicationExeName) then Exit;

  vir := TVerInfoRes.Create(ApplicationExeName);
  try
    Result := vir.ProductVersionBuild;
  finally
    FreeAndNil(vir);
  end;
end;

class function TApplicationInfo.ProductVersionMajor: Cardinal;
var
  vir: TVerInfoRes;
begin
  Result := 0;
  if not TVerInfoRes.VersionResourceAvailable(ApplicationExeName) then Exit;

  vir := TVerInfoRes.Create(ApplicationExeName);
  try
    Result := vir.ProductVersionMajor;
  finally
    FreeAndNil(vir);
  end;
end;

class function TApplicationInfo.ProductVersionMinor: Cardinal;
var
  vir: TVerInfoRes;
begin
  Result := 0;
  if not TVerInfoRes.VersionResourceAvailable(ApplicationExeName) then Exit;

  vir := TVerInfoRes.Create(ApplicationExeName);
  try
    Result := vir.ProductVersionMinor;
  finally
    FreeAndNil(vir);
  end;
end;

class function TApplicationInfo.ProductVersionRelease: Cardinal;
var
  vir: TVerInfoRes;
begin
  Result := 0;
  if not TVerInfoRes.VersionResourceAvailable(ApplicationExeName) then Exit;

  vir := TVerInfoRes.Create(ApplicationExeName);
  try
    Result := vir.ProductVersionRelease;
  finally
    FreeAndNil(vir);
  end;
end;

end.
