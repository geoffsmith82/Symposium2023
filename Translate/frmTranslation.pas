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
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.StdCtrls,
  Xml.Win.msxmldom,
  XMLDoc,
  uBaseTranslate,
  uEngineManager,
  uAmazon.Translate,
  uGoogle.Translate,
  uMicrosoft.Translate,
  uTranslatedfn,
  uOutputChangedLanguageTokens,
  OpenAI
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
    miTranslationEngine: TMenuItem;
    miMicrosoft: TMenuItem;
    miGoogle: TMenuItem;
    miAmazonTranslate: TMenuItem;
    miToLanguage: TMenuItem;
    miFromLanguage: TMenuItem;
    mmoSourceText: TMemo;
    mmoTranslatedText: TMemo;
    btnTranslate: TButton;
    Button1: TButton;
    miGoogleAuthenticate: TMenuItem;
    Button2: TButton;
    miGoogleMenu: TMenuItem;
    Logout1: TMenuItem;
    procedure btnTranslateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure miSelectEngineClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure miGoogleAuthenticateClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure miSelectSourceLanguageClick(Sender: TObject);
    procedure miSelectDestinationLanguageClick(Sender: TObject);
  private
    { Private declarations }
    FTranslateEngines: TEngineManager<TBaseTranslate>;
    FSettings : TIniFile;
    toLanguage : string;
    procedure LoadLanguageMenus;
    procedure HandleGoogleEngineSelected(Sender: TObject);
    procedure HandleMicrosoftEngineSelected(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmMainTranslationWindow: TfrmMainTranslationWindow;

implementation

{$R *.dfm}

uses
  LanguageCodes;

{$i ..\Libs\apikey.inc}

procedure TfrmMainTranslationWindow.btnTranslateClick(Sender: TObject);
begin
  mmoTranslatedText.Text := FTranslateEngines.ActiveEngine.Translate(mmoSourceText.Text, toLanguage, '');
end;

procedure TfrmMainTranslationWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSettings);
  FreeAndNil(FTranslateEngines);
end;

procedure TfrmMainTranslationWindow.miGoogleAuthenticateClick(Sender: TObject);
begin
  (FTranslateEngines.ActiveEngine as TGoogleTranslate).Authenticate;
end;

procedure TfrmMainTranslationWindow.HandleMicrosoftEngineSelected(Sender: TObject);
begin
  miGoogleMenu.Visible := False;
  LoadLanguageMenus;
end;

procedure TfrmMainTranslationWindow.HandleGoogleEngineSelected(Sender: TObject);
begin
  miGoogleMenu.Visible := True;
  LoadLanguageMenus;
  if FSettings.ReadString('Authentication', 'RefreshToken', '').IsEmpty then
  begin
    miGoogle.Enabled := True;
  end
  else
  begin
    miGoogle.Enabled := False;
  end;
end;

procedure TfrmMainTranslationWindow.FormCreate(Sender: TObject);
var
  filename : string;
  languageEngine : string;
  googleEngine : TGoogleTranslate;
  microsoftEngine : TMicrosoftTranslate;
  amazonEngine : TAmazonTranslate;
begin
  filename := ChangeFileExt(ParamStr(0),'.ini');
  FTranslateEngines := TEngineManager<TBaseTranslate>.Create;
  FSettings := TIniFile.Create(filename);
  toLanguage := FSettings.ReadString('Settings', 'ToLanguage', 'English');

  microsoftEngine := TMicrosoftTranslate.Create(ms_translate_key,'https://api.cognitive.microsofttranslator.com/');
  FTranslateEngines.RegisterEngine(microsoftEngine, miMicrosoft, HandleMicrosoftEngineSelected);

  googleEngine := TGoogleTranslate.Create(google_clientid, google_clientsecret, FSettings);
  FTranslateEngines.RegisterEngine(googleEngine, miGoogle, HandleGoogleEngineSelected);

  amazonEngine := TAmazonTranslate.Create(AWSAccessKey, AWSSecretkey, '');
  FTranslateEngines.RegisterEngine(amazonEngine, miAmazonTranslate, HandleMicrosoftEngineSelected);

  languageEngine := FSettings.ReadString('Settings', 'LanguageEngine', 'Microsoft Translate');
  FTranslateEngines.SelectEngine(languageEngine);
  FTranslateEngines.ActiveMenuItem.Checked := True;
end;

procedure TfrmMainTranslationWindow.LoadLanguageMenus;
var
  languages : TArray<string>;
  lang: string;
  menu : TMenuItem;
begin
  miFromLanguage.Clear;
  miToLanguage.Clear;
  languages := FTranslateEngines.ActiveEngine.FromLanguages;
  for lang in languages do
  begin
    menu := TMenuItem.Create(miFromLanguage);
    if lang.Length = 2 then
      menu.Caption := GetLanguageNameFromCode(lang)
    else
      menu.Caption := lang;
    menu.OnClick := miSelectSourceLanguageClick;
    menu.GroupIndex := 120;
    menu.RadioItem := True;
    miFromLanguage.Add(menu)
  end;

  languages := FTranslateEngines.ActiveEngine.ToLanguages;
  for lang in languages do
  begin
    menu := TMenuItem.Create(miToLanguage);
    menu.Caption := lang;
    menu.OnClick := miSelectDestinationLanguageClick;
    menu.GroupIndex := 140;
    menu.RadioItem := True;
    miToLanguage.Add(menu)
  end;
end;

procedure TfrmMainTranslationWindow.miSelectSourceLanguageClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
end;

procedure TfrmMainTranslationWindow.miSelectDestinationLanguageClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
end;

procedure TfrmMainTranslationWindow.Button1Click(Sender: TObject);
var
  xml : IXMLXliffType;
  i: Integer;
  v : IXMLVType;
  w : IXMLWType;
begin
  MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);
  xml := Loadxliff('D:\Programming\ADUG\Symposium2023\Translate\ComponentDinosOffice-OpenOffice-versionOficial\Demo\ENA\Unit1.dfn');
  v := Newv;
  for i := 0 to xml.File_.Body.Count - 1 do
  begin
    w := v.Add;
    w.X := i;
    w.Y := xml.File_.Body.Transunit[i].Source;
  end;
  mmoSourceText.Text := xmlDoc.FormatXMLData(v.XML);
end;

procedure TfrmMainTranslationWindow.Button2Click(Sender: TObject);
var
  doc : TXMLDocument;
  xml : IXMLXliffType;
  v : IXMLVType;
  w : IXMLWType;
  k, i : Integer;
  destInt : Integer;
  dest : string;
begin
  doc := TXMLDocument.Create(nil);
  try
    doc.LoadFromXML(mmoTranslatedText.Text);
    v := Getv(doc);
    xml := Loadxliff('D:\Programming\ADUG\Symposium2023\Translate\ComponentDinosOffice-OpenOffice-versionOficial\Demo\ENA\Unit1.dfn');
    for i := 0 to xml.File_.Body.Count - 1 do
    begin
      dest := v.W[i].Y.Replace('st', '').Replace('th', '').Replace('nd', '').Replace('rd', '');
      if not TryStrToInt(dest, destInt) then
      begin
        if xml.File_.Body.Transunit[i].Resname.EndsWith('.Caption') then
        begin

          if v.W[i].Y.DeQuotedString = v.W[i].Y then
          begin
            xml.File_.Body.Transunit[i].Target := v.W[i].Y.QuotedString;
          end
          else
            xml.File_.Body.Transunit[i].Target := v.W[i].Y;

          for k := 0 to xml.File_.Body.Transunit[i].Propgroup.Count - 1 do
          begin
            if xml.File_.Body.Transunit[i].Propgroup.Prop[k].Proptype = 'Status' then
            begin
              xml.File_.Body.Transunit[i].Propgroup.Prop[k].Text := '1';
            end;
          end;
        end;
      end;
    end;
    TFile.WriteAllText('D:\Programming\ADUG\Symposium2023\Translate\ComponentDinosOffice-OpenOffice-versionOficial\Demo\ENA\Unit1.out.dfn', xml.XML);
  finally
    v := nil;
  end;
end;

procedure TfrmMainTranslationWindow.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMainTranslationWindow.miSelectEngineClick(Sender: TObject);
begin
  FTranslateEngines.SelectEngine(Sender as TMenuItem);
  TMenuItem(Sender).Checked := True;
  FSettings.WriteString('Settings', 'LanguageEngine', FTranslateEngines.ActiveEngine.ClassName);
end;

end.
