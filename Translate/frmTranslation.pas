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
  Vcl.StdCtrls,
  uBaseTranslate,
  uAmazon.Translate,
  uGoogle.Translate,
  uMicrosoft.Translate,
  uTranslatedfn,
  uOutputChangedLanguageTokens,
  Xml.Win.msxmldom,
  XMLDoc
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
    TranslationEngine1: TMenuItem;
    miMicrosoft: TMenuItem;
    miGoogle: TMenuItem;
    miAmazonTranslate: TMenuItem;
    miToLanguage: TMenuItem;
    miFromLanguage: TMenuItem;
    mmoSourceText: TMemo;
    mmoTranslatedText: TMemo;
    btnTranslate: TButton;
    Button1: TButton;
    GoogleAuthenticate1: TMenuItem;
    Button2: TButton;
    procedure btnTranslateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure miMicrosoftClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure GoogleAuthenticate1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

{$i ..\Libs\apikey.inc}

procedure TfrmMainTranslationWindow.btnTranslateClick(Sender: TObject);
begin
  mmoTranslatedText.Text := FTranslate.Translate(mmoSourceText.Text);
end;

procedure TfrmMainTranslationWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSettings);
end;

procedure TfrmMainTranslationWindow.GoogleAuthenticate1Click(Sender: TObject);
begin
  (FTranslate as TGoogleTranslate).Authenticate;
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
    FTranslate := TMicrosoftTranslate.Create(ms_translate_key,'https://api.cognitive.microsofttranslator.com/','fr','en');
  end
  else if languageEngine.Replace('&','') = 'Google Translate' then
  begin
    miGoogle.Checked := True;
    FTranslate := TGoogleTranslate.Create(google_clientid, google_clientsecret, '', '', FSettings);
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

procedure TfrmMainTranslationWindow.miMicrosoftClick(Sender: TObject);
var
  languageEngine : string;
begin
  FSettings.WriteString('Settings', 'LanguageEngine', TMenuItem(Sender).Caption);
  languageEngine := TMenuItem(Sender).Caption;
  if languageEngine.Replace('&','') = 'Microsoft Translate' then
  begin
    miMicrosoft.Checked := True;
    FTranslate := TMicrosoftTranslate.Create(ms_translate_key,'https://api.cognitive.microsofttranslator.com/','fr','en');
  end
  else if languageEngine.Replace('&','') = 'Google Translate' then
  begin
    miGoogle.Checked := True;
    FTranslate := TGoogleTranslate.Create(google_clientid, google_clientsecret, '', '', FSettings);
  end
  else if languageEngine.Replace('&','') = 'Amazon Translate' then
  begin
    miAmazonTranslate.Checked := True;
  end;
end;

end.
