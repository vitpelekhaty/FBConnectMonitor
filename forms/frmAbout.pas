///	<summary>
///	  ������ �������� �������� ������ ���� �������� � ���������
///	</summary>
unit frmAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  ///	<summary>
  ///	  ����� ���� �������� � ���������
  ///	</summary>
  TAboutForm = class(TForm)
    Image: TImage;
    lProjectName: TLabel;
    lVersion: TLabel;
    lAuthorComment: TLabel;
    lAuthor: TLabel;
    lLicense: TLabel;
    lFreeware: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure lAuthorClick(Sender: TObject);
    procedure lAuthorMouseEnter(Sender: TObject);
    procedure lAuthorMouseLeave(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    ///	<summary>
    ///	  ����� ������� ����� ��������� ���� �������� � ��������� � ����������
    ///	  ��� � ����������� ����
    ///	</summary>
    class function ShowAboutForm: TAboutForm;

    ///	<summary>
    ///	  ����� ��������� �� ������������� ���������� ������ ���� �������� �
    ///	  ���������
    ///	</summary>
    ///	<param name="AForm">
    ///	  ��������� ���� �������� � ���������
    ///	</param>
    ///	<remarks>
    ///	  ����� ���������� True, ���� ��������� ��������� ����������
    ///	</remarks>
    class function InstanceExists(AForm: TAboutForm): Boolean;
  end;

implementation

uses
  WinApi.ShellApi, AppInfo;

{$R *.dfm}

{ TAboutForm }

procedure TAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Image.Picture.Icon := Application.Icon;
  lProjectName.Caption := Application.Title;
  lVersion.Caption := TApplicationInfo.FileVersion;
end;

procedure TAboutForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

class function TAboutForm.InstanceExists(AForm: TAboutForm): Boolean;
var
  i: Integer;
begin
  if not Assigned(AForm) then Exit(False);

  for i := 0 to Screen.FormCount - 1 do
  begin
    if (Screen.Forms[i] is TAboutForm) and (Screen.Forms[i] = AForm) then
      Exit(True);
  end;

  Result := False;
end;

procedure TAboutForm.lAuthorClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://plus.google.com/115445200058763001534/about',
    nil, nil, SW_MAXIMIZE);
end;

procedure TAboutForm.lAuthorMouseEnter(Sender: TObject);
begin
  if Sender is TLabel then
    TLabel(Sender).Font.Style := [fsUnderline];
end;

procedure TAboutForm.lAuthorMouseLeave(Sender: TObject);
begin
  if Sender is TLabel then
    TLabel(Sender).Font.Style := [];
end;

class function TAboutForm.ShowAboutForm: TAboutForm;
begin
  Result := TAboutForm.Create(Application);
  Result.Show;
end;

end.
