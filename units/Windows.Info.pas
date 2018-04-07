unit Windows.Info;

interface

type
  TSystemDir = record
  private
    class function GetSpecialFolder(const nFolder: Integer): string; static;
  public
    ///	<summary>
    ///	  <b>Version 5.0</b>. The file system directory that is used to store
    ///	  administrative tools for an individual user. The Microsoft Management
    ///	  Console (MMC) will save customized consoles to this directory, and it
    ///	  will roam with the user
    ///	</summary>
    class function GetAdminToolsDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that corresponds to the user's nonlocalized
    ///	  Startup program group
    ///	</summary>
    class function GetAltStartupDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 4.71</b>. The file system directory that serves as a
    ///	  common repository for application-specific data. A typical path is
    ///	  C:\Documents and Settings\<i>username</i>\Application Data. This
    ///	  CSIDL is supported by the redistributable Shfolder.dll for systems
    ///	  that do not have the Microsoft Internet Explorer 4.0 integrated Shell
    ///	  installed
    ///	</summary>
    class function GetAppDataDir: string; static; inline;

    ///	<summary>
    ///	  The virtual folder containing the objects in the user's
    ///	  <b>Recycle Bin</b>
    ///	</summary>
    class function GetBitBucketDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 6.0</b>. The file system directory acting as a staging
    ///	  area for files waiting to be written to CD. A typical path is
    ///	  C:\Documents and Settings\<i>username</i>\Local Settings\Application
    ///	  Data\Microsoft\CD Burning
    ///	</summary>
    class function GetCDBurnAreaDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 5.0</b>. The file system directory containing
    ///	  administrative tools for all users of the computer
    ///	</summary>
    class function GetCommonAdminToolsDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that corresponds to the nonlocalized
    ///	  Startup program group for all users. Valid only for Microsoft Windows
    ///	  NT systems
    ///	</summary>
    class function GetCommonAltStartupDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 5.0</b>. The file system directory containing application
    ///	  data for all users. A typical path is C:\Documents and Settings\All
    ///	  Users\Application Data
    ///	</summary>
    class function GetCommonAppDataDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that contains files and folders that appear
    ///	  on the desktop for all users. A typical path is C:\Documents and
    ///	  Settings\All Users\Desktop. Valid only for Windows NT systems
    ///	</summary>
    class function GetCommonDesktopDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that contains documents that are common to
    ///	  all users. A typical paths is C:\Documents and Settings\All
    ///	  Users\Documents. Valid for Windows NT systems and Microsoft Windows
    ///	  95 and Windows 98 systems with Shfolder.dll installed
    ///	</summary>
    class function GetCommonDocumentsDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that serves as a common repository for
    ///	  favorite items common to all users. Valid only for Windows NT systems
    ///	</summary>
    class function GetCommonFavoritesDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 6.0</b>. The file system directory that serves as a
    ///	  repository for music files common to all users. A typical path is
    ///	  C:\Documents and Settings\All Users\Documents\My Music
    ///	</summary>
    class function GetCommonMusicDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 6.0</b>. The file system directory that serves as a
    ///	  repository for image files common to all users. A typical path is
    ///	  C:\Documents and Settings\All Users\Documents\My Pictures
    ///	</summary>
    class function GetCommonPicturesDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that contains the directories for the
    ///	  common program groups that appear on the <b>Start</b> menu for all
    ///	  users. A typical path is C:\Documents and Settings\All Users\Start
    ///	  Menu\Programs. Valid only for Windows NT systems
    ///	</summary>
    class function GetCommonProgramsDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that contains the programs and folders that
    ///	  appear on the <b>Start</b> menu for all users. A typical path is
    ///	  C:\Documents and Settings\All Users\Start Menu. Valid only for
    ///	  Windows NT systems
    ///	</summary>
    class function GetCommonStartMenuDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that contains the programs that appear in
    ///	  the Startup folder for all users. A typical path is C:\Documents and
    ///	  Settings\All Users\Start Menu\Programs\Startup. Valid only for
    ///	  Windows NT systems
    ///	</summary>
    class function GetCommonStartupDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that contains the templates that are
    ///	  available to all users. A typical path is C:\Documents and
    ///	  Settings\All Users\Templates. Valid only for Windows NT systems
    ///	</summary>
    class function GetCommonTemplatesDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 6.0</b>. The file system directory that serves as a
    ///	  repository for video files common to all users. A typical path is
    ///	  C:\Documents and Settings\All Users\Documents\My Videos
    ///	</summary>
    class function GetCommonVideoDir: string; static; inline;

    ///	<summary>
    ///	  The folder representing other machines in your workgroup
    ///	</summary>
    class function GetComputersNearMe: string; static; inline;

    ///	<summary>
    ///	  The virtual folder representing Network Connections, containing
    ///	  network and dial-up connections
    ///	</summary>
    class function GetConnectionsDir: string; static; inline;

    ///	<summary>
    ///	  The virtual folder containing icons for the Control Panel applications
    ///	</summary>
    class function GetControlsDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that serves as a common repository for
    ///	  Internet cookies. A typical path is C:\Documents and Settings\
    ///	  <i>username</i>\Cookies
    ///	</summary>
    class function GetCookiesDir: string; static; inline;

    ///	<summary>
    ///	  The virtual folder representing the Windows desktop, the root of the
    ///	  namespace
    ///	</summary>
    class function GetDesktop: string; static; inline;

    ///	<summary>
    ///	  The file system directory used to physically store file objects on
    ///	  the desktop (not to be confused with the desktop folder itself). A
    ///	  typical path is C:\Documents and Settings\<i>username</i>\Desktop
    ///	</summary>
    class function GetDesktopDir: string; static; inline;

    ///	<summary>
    ///	  The virtual folder representing My Computer, containing everything on
    ///	  the local computer: storage devices, printers, and Control Panel. The
    ///	  folder may also contain mapped network drives
    ///	</summary>
    class function GetDrives: string; static; inline;

    ///	<summary>
    ///	  The file system directory that serves as a common repository for the
    ///	  user's favorite items. A typical path is C:\Documents and Settings\
    ///	  <i>username</i>\Favorites
    ///	</summary>
    class function GetFavoritesDir: string; static; inline;

    ///	<summary>
    ///	  A virtual folder containing fonts. A typical path is C:\Windows\Fonts
    ///	</summary>
    class function GetFontsDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that serves as a common repository for
    ///	  Internet history items
    ///	</summary>
    class function GetHistoryDir: string; static; inline;

    ///	<summary>
    ///	  A viritual folder for Internet Explorer
    ///	</summary>
    class function GetInternetDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 4.72</b>. The file system directory that serves as a
    ///	  common repository for temporary Internet files. A typical path is
    ///	  C:\Documents and Settings\<i>username</i>\Local Settings\Temporary
    ///	  Internet Files
    ///	</summary>
    class function GetInternetCacheDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 5.0</b>. The file system directory that serves as a data
    ///	  repository for local (nonroaming) applications. A typical path is
    ///	  C:\Documents and Settings\<i>username</i>\Local Settings\Application
    ///	  Data
    ///	</summary>
    class function GetLocalAppDataDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 6.0</b>. The virtual folder representing the My Documents
    ///	  desktop item
    ///	</summary>
    class function GetMyDocumentsDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that serves as a common repository for
    ///	  music files. A typical path is C:\Documents and Settings\User\My
    ///	  Documents\My Music
    ///	</summary>
    class function GetMyMusicDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 5.0</b>. The file system directory that serves as a common
    ///	  repository for image files. A typical path is C:\Documents and
    ///	  Settings\<i>username</i>\My Documents\My Pictures
    ///	</summary>
    class function GetMyPicturesDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 6.0</b>. The file system directory that serves as a common
    ///	  repository for video files. A typical path is C:\Documents and
    ///	  Settings\<i>username</i>\My Documents\My Videos
    ///	</summary>
    class function GetMyVideoDir: string; static; inline;

    ///	<summary>
    ///	  A file system directory containing the link objects that may exist in
    ///	  the <b>My Network Places</b> virtual folder. It is not the same as
    ///	  CSIDL_NETWORK, which represents the network namespace root. A typical
    ///	  path is C:\Documents and Settings\<i>username</i>\NetHood
    ///	</summary>
    class function GetNethoodDir: string; static; inline;

    ///	<summary>
    ///	  A virtual folder representing Network Neighborhood, the root of the
    ///	  network namespace hierarchy
    ///	</summary>
    class function GetNetwork: string; static; inline;

    ///	<summary>
    ///	  <para>
    ///	    <b>Version 6.0</b>. The virtual folder representing the My
    ///	    Documents desktop item. This is equivalent to CSIDL_MYDOCUMENTS.
    ///	  </para>
    ///	  <para>
    ///	    <b>Previous to Version 6.0</b>. The file system directory used to
    ///	    physically store a user's common repository of documents. A typical
    ///	    path is C:\Documents and Settings\<i>username</i>\My Documents.
    ///	    This should be distinguished from the virtual <b>My Documents</b>
    ///	    folder in the namespace
    ///	  </para>
    ///	</summary>
    class function GetPersonalDir: string; static; inline;

    ///	<summary>
    ///	  The virtual folder containing installed printers
    ///	</summary>
    class function GetPrinters: string; static; inline;

    ///	<summary>
    ///	  The file system directory that contains the link objects that can
    ///	  exist in the <b>Printers</b> virtual folder. A typical path is
    ///	  C:\Documents and Settings\<i>username</i>\PrintHood
    ///	</summary>
    class function GetPrinthoodDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 5.0</b>. The user's profile folder. A typical path is
    ///	  C:\Documents and Settings\<i>username</i>. Applications should not
    ///	  create files or folders at this level; they should put their data
    ///	  under the locations referred to by CSIDL_APPDATA or
    ///	  CSIDL_LOCAL_APPDATA
    ///	</summary>
    class function GetProfileDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 5.0</b>. The Program Files folder. A typical path is
    ///	  C:\Program Files
    ///	</summary>
    class function GetProgramFilesDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 5.0</b>. A folder for components that are shared across
    ///	  applications. A typical path is C:\Program Files\Common. Valid only
    ///	  for Windows NT, Windows 2000, and Windows XP systems. Not valid for
    ///	  Windows Millennium Edition (Windows Me).
    ///	</summary>
    class function GetCommonProgramFilesDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that contains the user's program groups
    ///	  (which are themselves file system directories). A typical path is
    ///	  C:\Documents and Settings\<i>username</i>\Start Menu\Programs
    ///	</summary>
    class function GetProgramsDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that contains shortcuts to the user's most
    ///	  recently used documents. A typical path is C:\Documents and Settings\
    ///	  <i>username</i>\My Recent Documents
    ///	</summary>
    class function GetRecentDir: string; static; inline;

    ///	<summary>
    ///	  Windows Vista. The file system directory that contains resource data.
    ///	  A typical path is C:\Windows\Resources
    ///	</summary>
    class function GetResourcesDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory containing <b>Start</b> menu items. A
    ///	  typical path is C:\Documents and Settings\<i>username</i>\Start Menu
    ///	</summary>
    class function GetStartMenuDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that corresponds to the user's Startup
    ///	  program group. The system starts these programs whenever any user
    ///	  logs onto Windows NT or starts Windows 95. A typical path is
    ///	  C:\Documents and Settings\<i>username</i>\Start Menu\Programs\Startup
    ///	</summary>
    class function GetStartupDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 5.0</b>. The Windows System folder. A typical path is
    ///	  C:\Windows\System32
    ///	</summary>
    class function GetSystemDir: string; static; inline;

    ///	<summary>
    ///	  The file system directory that serves as a common repository for
    ///	  document templates. A typical path is C:\Documents and Settings\
    ///	  <i>username</i>\Templates
    ///	</summary>
    class function GetTemplatesDir: string; static; inline;

    ///	<summary>
    ///	  <b>Version 5.0</b>. The Windows directory or SYSROOT. This
    ///	  corresponds to the %windir% or %SYSTEMROOT% environment variables. A
    ///	  typical path is C:\Windows
    ///	</summary>
    class function GetWindowsDir: string; static; inline;
  end;

implementation

uses WinApi.Windows, WinApi.ShlObj, System.SysUtils;

{ TSystemDir }

class function TSystemDir.GetAdminToolsDir: string;
begin
  Result := GetSpecialFolder(CSIDL_ADMINTOOLS);
end;

class function TSystemDir.GetAltStartupDir: string;
begin
  Result := GetSpecialFolder(CSIDL_ALTSTARTUP);
end;

class function TSystemDir.GetAppDataDir: string;
begin
  Result := GetSpecialFolder(CSIDL_APPDATA);
end;

class function TSystemDir.GetBitBucketDir: string;
begin
  Result := GetSpecialFolder(CSIDL_BITBUCKET);
end;

class function TSystemDir.GetCDBurnAreaDir: string;
begin
  if Win32MajorVersion < 6 then Exit(EmptyStr);
  Result := GetSpecialFolder(CSIDL_CDBURN_AREA);
end;

class function TSystemDir.GetCommonAdminToolsDir: string;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_ADMINTOOLS);
end;

class function TSystemDir.GetCommonAltStartupDir: string;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_ALTSTARTUP);
end;

class function TSystemDir.GetCommonAppDataDir: string;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_APPDATA);
end;

class function TSystemDir.GetCommonDesktopDir: string;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_DESKTOPDIRECTORY);
end;

class function TSystemDir.GetCommonDocumentsDir: string;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_DOCUMENTS);
end;

class function TSystemDir.GetCommonFavoritesDir: string;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_FAVORITES);
end;

class function TSystemDir.GetCommonMusicDir: string;
begin
  if Win32MajorVersion < 6 then Exit(EmptyStr);
  Result := GetSpecialFolder(CSIDL_COMMON_MUSIC);
end;

class function TSystemDir.GetCommonPicturesDir: string;
begin
  if Win32MajorVersion < 6 then Exit(EmptyStr);
  Result := GetSpecialFolder(CSIDL_COMMON_PICTURES);
end;

class function TSystemDir.GetCommonProgramFilesDir: string;
begin
  Result := GetSpecialFolder(CSIDL_PROGRAM_FILES_COMMON);
end;

class function TSystemDir.GetCommonProgramsDir: string;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_PROGRAMS);
end;

class function TSystemDir.GetCommonStartMenuDir: string;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_STARTMENU);
end;

class function TSystemDir.GetCommonStartupDir: string;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_STARTUP);
end;

class function TSystemDir.GetCommonTemplatesDir: string;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_TEMPLATES);
end;

class function TSystemDir.GetCommonVideoDir: string;
begin
  if Win32MajorVersion < 6 then Exit(EmptyStr);
  Result := GetSpecialFolder(CSIDL_COMMON_VIDEO);
end;

class function TSystemDir.GetComputersNearMe: string;
begin
  Result := GetSpecialFolder(CSIDL_COMPUTERSNEARME);
end;

class function TSystemDir.GetConnectionsDir: string;
begin
  Result := GetSpecialFolder(CSIDL_CONNECTIONS);
end;

class function TSystemDir.GetControlsDir: string;
begin
  Result := GetSpecialFolder(CSIDL_CONTROLS);
end;

class function TSystemDir.GetCookiesDir: string;
begin
  Result := GetSpecialFolder(CSIDL_COOKIES);
end;

class function TSystemDir.GetDesktop: string;
begin
  Result := GetSpecialFolder(CSIDL_DESKTOP);
end;

class function TSystemDir.GetDesktopDir: string;
begin
  Result := GetSpecialFolder(CSIDL_DESKTOPDIRECTORY);
end;

class function TSystemDir.GetDrives: string;
begin
  Result := GetSpecialFolder(CSIDL_DRIVES);
end;

class function TSystemDir.GetFavoritesDir: string;
begin
  Result := GetSpecialFolder(CSIDL_FAVORITES);
end;

class function TSystemDir.GetFontsDir: string;
begin
  Result := GetSpecialFolder(CSIDL_FONTS);
end;

class function TSystemDir.GetHistoryDir: string;
begin
  Result := GetSpecialFolder(CSIDL_HISTORY);
end;

class function TSystemDir.GetInternetCacheDir: string;
begin
  Result := GetSpecialFolder(CSIDL_INTERNET_CACHE);
end;

class function TSystemDir.GetInternetDir: string;
begin
  Result := GetSpecialFolder(CSIDL_INTERNET);
end;

class function TSystemDir.GetLocalAppDataDir: string;
begin
  Result := GetSpecialFolder(CSIDL_LOCAL_APPDATA);
end;

class function TSystemDir.GetMyDocumentsDir: string;
begin
  if Win32MajorVersion < 6 then
    Result := GetSpecialFolder(CSIDL_PERSONAL)
  else
    Result := GetSpecialFolder(CSIDL_MYDOCUMENTS);
end;

class function TSystemDir.GetMyMusicDir: string;
begin
  Result := GetSpecialFolder(CSIDL_MYMUSIC);
end;

class function TSystemDir.GetMyPicturesDir: string;
begin
  Result := GetSpecialFolder(CSIDL_MYPICTURES);
end;

class function TSystemDir.GetMyVideoDir: string;
begin
  if Win32MajorVersion < 6 then Exit(EmptyStr);
  Result := GetSpecialFolder(CSIDL_MYVIDEO);
end;

class function TSystemDir.GetNethoodDir: string;
begin
  Result := GetSpecialFolder(CSIDL_NETHOOD);
end;

class function TSystemDir.GetNetwork: string;
begin
  Result := GetSpecialFolder(CSIDL_NETWORK);
end;

class function TSystemDir.GetPersonalDir: string;
begin
  Result := GetSpecialFolder(CSIDL_PERSONAL);
end;

class function TSystemDir.GetPrinters: string;
begin
  Result := GetSpecialFolder(CSIDL_PRINTERS);
end;

class function TSystemDir.GetPrinthoodDir: string;
begin
  Result := GetSpecialFolder(CSIDL_PRINTHOOD);
end;

class function TSystemDir.GetProfileDir: string;
begin
  Result := GetSpecialFolder(CSIDL_PROFILE);
end;

class function TSystemDir.GetProgramFilesDir: string;
begin
  Result := GetSpecialFolder(CSIDL_PROGRAM_FILES);
end;

class function TSystemDir.GetProgramsDir: string;
begin
  Result := GetSpecialFolder(CSIDL_PROGRAMS);
end;

class function TSystemDir.GetRecentDir: string;
begin
  Result := GetSpecialFolder(CSIDL_RECENT);
end;

class function TSystemDir.GetResourcesDir: string;
begin
  if Win32MajorVersion < 6 then Exit(EmptyStr);
  Result := GetSpecialFolder(CSIDL_RESOURCES);
end;

class function TSystemDir.GetSpecialFolder(const nFolder: Integer): string;
var
  Path: array [0 .. MAX_PATH] of char;
begin
  SHGetSpecialFolderPath(0, Path, nFolder, False);
  Result := Path;
end;

class function TSystemDir.GetStartMenuDir: string;
begin
  Result := GetSpecialFolder(CSIDL_STARTMENU);
end;

class function TSystemDir.GetStartupDir: string;
begin
  Result := GetSpecialFolder(CSIDL_STARTUP);
end;

class function TSystemDir.GetSystemDir: string;
begin
  Result := GetSpecialFolder(CSIDL_SYSTEM);
end;

class function TSystemDir.GetTemplatesDir: string;
begin
  Result := GetSpecialFolder(CSIDL_TEMPLATES);
end;

class function TSystemDir.GetWindowsDir: string;
begin
  Result := GetSpecialFolder(CSIDL_WINDOWS);
end;

end.
