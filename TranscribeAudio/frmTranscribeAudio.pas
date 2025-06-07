unit frmTranscribeAudio;

interface

uses
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Inifiles,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.StdCtrls,
  ApiKeyStore,
  uSTT,
  uSTT.Google,
  uSTT.Amazon,
  uSTT.Microsoft,
  uSTT.OpenAI.Whisper.Online,
  uEngineManager
  ;

type
  TVoiceRecognitionForm = class(TForm)
    edtFilename: TEdit;
    btnBrowse: TButton;
    ComboBox1: TComboBox;
    OpenDialog: TOpenDialog;
    btnRecognizeSpeech: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    mmMainMenu: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    miExit: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    miEngine: TMenuItem;
    miMicrosoft: TMenuItem;
    miGoogle: TMenuItem;
    miAmazon: TMenuItem;
    miOpenAIWhisper: TMenuItem;
    miOpenAIWhisperLocal: TMenuItem;
    GoogleAuthenticate1: TMenuItem;
    miGoogleMenu: TMenuItem;
    Settings1: TMenuItem;
    miAPIKeys: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnRecognizeSpeechClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miEngineSelectedClick(Sender: TObject);
    procedure GoogleAuthenticate1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HandleGoogleEngineSelected(Sender: TObject);
    procedure HandleMicrosoftEngineSelected(Sender: TObject);
    procedure HandleWhisperOnlineEngineSelected(Sender: TObject);
    procedure HandleAmazonEngineSelected(Sender: TObject);
    procedure miAPIKeysClick(Sender: TObject);
  private
    { Private declarations }
    FSpeechToTextEngines : TEngineManager<TBaseSpeechToText, TMenuItem>;
    FSettings : TIniFile;
    FApiKeyStore : TApiKeyStore;
  public
    { Public declarations }
  end;

var
  VoiceRecognitionForm: TVoiceRecognitionForm;

implementation

{$R *.dfm}

uses
  frmApiKeyStore;

procedure TVoiceRecognitionForm.FormCreate(Sender: TObject);
var
  engine : string;
  googleEngine : TGoogleSpeechToText;
  microsoftEngine : TMicrosoftSpeechToText;
  whisperOnlineEngine : TOpenAiWhisperOnline;
  amazonEngine : TAmazonSpeechToText;
begin
  FSpeechToTextEngines := TEngineManager<TBaseSpeechToText, TMenuItem>.Create;
  FApiKeyStore := TApiKeyStore.GetInstance;

  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));

  googleEngine := TGoogleSpeechToText.Create(FApiKeyStore.LoadApiKey('google_clientid'), FApiKeyStore.LoadApiKey('google_clientsecret'), 'ADUG Demo', '', FSettings);
  FSpeechToTextEngines.RegisterEngine(googleEngine, miGoogle, HandleGoogleEngineSelected);

  microsoftEngine := TMicrosoftSpeechToText.Create('', '', '');
  FSpeechToTextEngines.RegisterEngine(microsoftEngine, miMicrosoft, HandleMicrosoftEngineSelected);

  amazonEngine := TAmazonSpeechToText.Create(nil, FApiKeyStore.LoadApiKey('AWSAccessKey'), FApiKeyStore.LoadApiKey('AWSSecretKey'), FApiKeyStore.LoadSetting('AWSRegion'), 'bucket');
  FSpeechToTextEngines.RegisterEngine(amazonEngine, miAmazon, HandleAmazonEngineSelected);

  whisperOnlineEngine := TOpenAiWhisperOnline.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey'), '', '');
  FSpeechToTextEngines.RegisterEngine(whisperOnlineEngine, miOpenAIWhisper, HandleWhisperOnlineEngineSelected);

  engine := FSettings.ReadString('Settings', 'Engine', 'MicrosoftSpeech');
  FSpeechToTextEngines.SelectEngine(engine);
  FSpeechToTextEngines.ActiveMenuItem.Checked := True;
end;

procedure TVoiceRecognitionForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSettings);
  FreeAndNil(FSpeechToTextEngines);
end;

procedure TVoiceRecognitionForm.GoogleAuthenticate1Click(Sender: TObject);
begin
  (FSpeechToTextEngines.ActiveEngine as TGoogleSpeechToText).Authenticate;
end;

procedure TVoiceRecognitionForm.btnBrowseClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    edtFilename.Text := OpenDialog.FileName;
  end;
end;

procedure TVoiceRecognitionForm.btnRecognizeSpeechClick(Sender: TObject);
var
  filename : string;
begin
  filename := edtFilename.Text;
  Memo1.Text := FSpeechToTextEngines.ActiveEngine.TranscribeAudio(filename, 'whisper-1');
end;

procedure TVoiceRecognitionForm.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TVoiceRecognitionForm.HandleMicrosoftEngineSelected(Sender: TObject);
begin
  miGoogleMenu.Visible := False;
end;

procedure TVoiceRecognitionForm.HandleWhisperOnlineEngineSelected(Sender: TObject);
begin
  miGoogleMenu.Visible := False;
end;

procedure TVoiceRecognitionForm.HandleAmazonEngineSelected(Sender: TObject);
begin
  miGoogleMenu.Visible := False;
end;

procedure TVoiceRecognitionForm.HandleGoogleEngineSelected(Sender: TObject);
begin
  miGoogleMenu.Visible := True;
  if FSettings.ReadString('Authentication', 'RefreshToken', '').IsEmpty then
  begin
    miGoogle.Enabled := True;
  end
  else
  begin
    miGoogle.Enabled := False;
  end;
end;


procedure TVoiceRecognitionForm.miAPIKeysClick(Sender: TObject);
var
  frmApiKeyStores : TfrmApiKeyStores;
begin
  frmApiKeyStores := TfrmApiKeyStores.Create(nil);
  try
    frmApiKeyStores.ShowModal;
  finally
    FreeAndNil(frmApiKeyStores)
  end;
end;

procedure TVoiceRecognitionForm.miEngineSelectedClick(Sender: TObject);
begin
  FSpeechToTextEngines.SelectEngine(Sender as TMenuItem);
  FSettings.WriteString('Settings', 'Engine', FSpeechToTextEngines.ActiveEngine.ClassName);
end;

end.
