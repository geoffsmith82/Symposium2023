unit ufrmTalk;

interface

uses
  System.SysUtils,
  System.Types,
  System.Threading,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
  System.ImageList,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.Memo.Types,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Menus,
  FMX.ListView,
  FMX.StdCtrls,
  FMX.ImgList,
  uTTS,
  uTTS.Windows.Engine,
  uTTS.OpenAI,
  uTTS.ElevenLabs,
  uTTS.Microsoft.Cognitive,
  uTTS.Amazon.Polly,
  uTTS.GoogleSpeech,
  ApiKeyStore.Windows,
  FMX.frmApiKeyStore,
  ApiKeyStore
  ;

type
  TfrmTalk = class(TForm)
    lvVoices: TListView;
    Memo1: TMemo;
    btnTalk: TButton;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    miExit: TMenuItem;
    miSettings: TMenuItem;
    miGoogleAuthenticate: TMenuItem;
    miAPIKeys: TMenuItem;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure btnTalkClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miAPIKeysClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miGoogleAuthenticateClick(Sender: TObject);
  private
    { Private declarations }
    FKeyStore: TApiKeyStore;
    FlblStatus: TLabel;
    FWindowsSpeech : TWindowsSpeechService;
    FOpenAITTS : TOpenAITextToSpeech;
    FElevenLabsTTS : TElevenLabsService;
    FMsTTS : TMicrosoftCognitiveService;
    FPolly : TAmazonPollyService;
    FGoogleSpeech : TGoogleSpeechService;
    procedure AddVoicesToList(TTS: TBaseTextToSpeech; ImageIndex: Integer);
    procedure LoadVoices;
  public
    { Public declarations }
  end;

var
  frmTalk: TfrmTalk;

implementation

{$R *.fmx}

function GetVoiceId(const InputStr: string): string;
var
  SpacePos: Integer;
begin
  // Find the position of the first space character
  SpacePos := Pos(' ', InputStr);

  // If a space is found, extract everything before it
  if SpacePos > 0 then
    Result := Copy(InputStr, 1, SpacePos - 1)
  else
    // If no space is found, return the entire string
    Result := InputStr;
end;


procedure TfrmTalk.btnTalkClick(Sender: TObject);
begin
  (lvVoices.Selected.TagObject as TBaseTextToSpeech).PlayText(Memo1.Text, GetVoiceId((lvVoices.Selected as TListViewItem).Detail));
end;

procedure TfrmTalk.FormCreate(Sender: TObject);
begin
  FKeyStore := TApiKeyStore.GetInstance;
  FlblStatus := TLabel.Create(nil);
  FlblStatus.Margins.Left := 10;
  StatusBar.AddObject(FlblStatus);
  TTask.Run(LoadVoices);
end;

procedure TfrmTalk.LoadVoices;
begin
  // Windows Speech (always created)
  FwindowsSpeech := TWindowsSpeechService.Create;
  AddVoicesToList(FwindowsSpeech, 0);

  // OpenAI
  if not FKeyStore.LoadApiKey('chatgpt_apikey').IsEmpty then
  begin
    FOpenAITTS := TOpenAITextToSpeech.Create(FKeyStore.LoadApiKey('chatgpt_apikey'));
    AddVoicesToList(FOpenAITTS, 1);
  end;

  // Eleven Labs
  if not FKeyStore.LoadApiKey('ElevenLabsAPIKey').IsEmpty then
  begin
    FElevenLabsTTS := TElevenLabsService.Create(FKeyStore.LoadApiKey('ElevenLabsAPIKey'));
    AddVoicesToList(FElevenLabsTTS, 2);
  end;

  // Microsoft Cognitive
  if not FKeyStore.LoadApiKey('ms_cognative_service_resource_key').IsEmpty then
  begin
    FMsTTS := TMicrosoftCognitiveService.Create(
      FKeyStore.LoadApiKey('ms_cognative_service_resource_key'),
      'australiaeast.tts.speech.microsoft.com'
    );
    AddVoicesToList(FMsTTS, 3);
  end;

  // Amazon Polly
  if not FKeyStore.LoadApiKey('AWSAccessKey').IsEmpty then
  begin
    FPolly := TAmazonPollyService.Create(
      FKeyStore.LoadApiKey('AWSAccessKey'),
      FKeyStore.LoadApiKey('AWSSecretKey'),
      FKeyStore.LoadSetting('AWSRegion')
    );
    AddVoicesToList(FPolly, 4);
  end;

  // Google Speech
  if not FKeyStore.LoadApiKey('google_refreshtoken').IsEmpty then
  begin
    var Settings := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
    FGoogleSpeech := TGoogleSpeechService.Create(
      FKeyStore.LoadApiKey('google_clientid'),
      FKeyStore.LoadApiKey('google_clientsecret'),
      'ADUG Demo',
      '',
      Settings
    );
    AddVoicesToList(FGoogleSpeech, 5);
    Settings.Free;
  end;
end;

procedure TfrmTalk.AddVoicesToList(TTS: TBaseTextToSpeech; ImageIndex: Integer);
var
  i: Integer;

  procedure QueueVoice(const AName, ADetail: string; const AImageIndex: Integer; const AObject: TBaseTextToSpeech);
  begin
    TThread.Queue(nil, procedure
    var
      item: TListViewItem;
    begin
      item := lvVoices.Items.Add;
      item.ImageIndex := AImageIndex;
      item.TagObject := AObject;
      item.Text := AName;
      item.Detail := ADetail;
      FlblStatus.Text := lvVoices.ItemCount.ToString + ' Voices listed';
    end);
  end;

begin
  for i := 0 to TTS.Voices.Count - 1 do
  begin
    var VoiceName := TTS.Voices[i].VoiceName;
    var VoiceId := TTS.Voices[i].VoiceId;
    var VoiceGender := TTS.Voices[i].VoiceGender;
    var Detail := VoiceId + ' ' + VoiceGender;

    QueueVoice(VoiceName, Detail, ImageIndex, TTS);
  end;
end;

procedure TfrmTalk.FormDestroy(Sender: TObject);
begin
    FreeAndNil(FWindowsSpeech);
    FreeAndNil(FOpenAITTS);
    FreeAndNil(FMsTTS);
    FreeAndNil(FElevenLabsTTS);
end;

procedure TfrmTalk.FormResize(Sender: TObject);
begin
  lvVoices.Width := Width / 3;
  Memo1.Position.X := lvVoices.Width + lvVoices.Position.X + 10;
  Memo1.Width := Width - Memo1.Position.X - 20;
end;

procedure TfrmTalk.miAPIKeysClick(Sender: TObject);
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

procedure TfrmTalk.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmTalk.miGoogleAuthenticateClick(Sender: TObject);
begin
  if FKeyStore.LoadApiKey('google_refreshtoken').IsEmpty then
  begin
    var   FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
   // Assert(Assigned(FGoogleSpeech));

    FGoogleSpeech := TGoogleSpeechService.Create(FKeyStore.LoadApiKey('google_clientid'),  FKeyStore.LoadApiKey('google_clientsecret'), 'ADUG Demo', '', FSettings);
    FGoogleSpeech.Authenticate;
  end;
end;

end.
