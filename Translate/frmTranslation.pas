unit frmTranslation;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IniFiles,
  System.IOUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  uBaseTranslate,
  uAmazon.Translate,
  uGoogle.Translate,
  uMicrosoft.Translate
  ;

type
  TfrmMainTranslationWindow = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    ranslationEngine1: TMenuItem;
    miMicrosoft: TMenuItem;
    miGoogle: TMenuItem;
    miAmazonTranslate: TMenuItem;
    miToLanguage: TMenuItem;
    miFromLanguage: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure miMicrosoftClick(Sender: TObject);
  private
    { Private declarations }
    FTranslate : TBaseTranslate;
    FSettings : TIniFile;
    procedure LoadLanguageMenus;
  public
    { Public declarations }
  end;

var
  frmMainTranslationWindow: TfrmMainTranslationWindow;

implementation

{$R *.dfm}

procedure TfrmMainTranslationWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSettings);
end;

procedure TfrmMainTranslationWindow.FormCreate(Sender: TObject);
var
  filename : string;
  toLanguage : string;
  languageEngine : string;
begin
  filename := ChangeFileExt(ParamStr(0),'.ini');
  FSettings := TIniFile.Create(filename);
  toLanguage := FSettings.ReadString('Settings', 'ToLanguage', 'English');
  languageEngine := FSettings.ReadString('Settings', 'LanguageEngine', 'Microsoft Translate');
  if languageEngine.Replace('&','') = 'Microsoft Translate' then
  begin
    miMicrosoft.Checked := True;
    FTranslate := TMicrosoftTranslate.Create('','https://api.cognitive.microsofttranslator.com/','','');
  end
  else if languageEngine.Replace('&','') = 'Google Translate' then
  begin
    miGoogle.Checked := True;
    FTranslate := TGoogleTranslate.Create('','','');
  end
  else if languageEngine.Replace('&','') = 'Amazon Translate' then
  begin
    miAmazonTranslate.Checked := True;
  end;
  LoadLanguageMenus;
end;

procedure TfrmMainTranslationWindow.LoadLanguageMenus;
var
  languages : TArray<string>;
  lang: string;
  menu : TMenuItem;
begin
  languages := FTranslate.FromLanguages;
  for lang in languages do
  begin
    menu := TMenuItem.Create(miFromLanguage);
    menu.Caption := lang;
    miFromLanguage.Add(menu)
  end;

  languages := FTranslate.ToLanguages;
  for lang in languages do
  begin
    menu := TMenuItem.Create(miToLanguage);
    menu.Caption := lang;
    miToLanguage.Add(menu)
  end;
end;

procedure TfrmMainTranslationWindow.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMainTranslationWindow.miMicrosoftClick(Sender: TObject);
begin
  FSettings.WriteString('Settings', 'LanguageEngine', TMenuItem(Sender).Caption);
end;

end.
