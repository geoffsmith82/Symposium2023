unit ufrmTalk;

interface

uses
  System.SysUtils,
  System.Types,
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
    FWindowsSpeech : TWindowsSpeechService;
    FOpenAITTS : TOpenAITextToSpeech;
    FElevenLabsTTS : TElevenLabsService;
    FMsTTS : TMicrosoftCognitiveService;
    FPolly : TAmazonPollyService;
    FGoogleSpeech : TGoogleSpeechService;
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
var
  i: Integer;
  item : TListViewItem;
begin
  FKeyStore := TApiKeyStore.GetInstance;
  FwindowsSpeech := TWindowsSpeechService.Create;
  for i := 0 to FwindowsSpeech.Voices.Count - 1 do
  begin
    item := lvVoices.Items.Add;
    item.ImageIndex := 0;
    item.TagObject := FwindowsSpeech;
    item.Text := FwindowsSpeech.Voices[i].VoiceName;
    item.Detail := FwindowsSpeech.Voices[i].VoiceId + ' ' + FwindowsSpeech.Voices[i].VoiceGender;
  end;



  if not FKeyStore.LoadApiKey('chatgpt_apikey').IsEmpty then
  begin
    FOpenAITTS := TOpenAITextToSpeech.Create(FKeyStore.LoadApiKey('chatgpt_apikey'));
    for i := 0 to FOpenAITTS.Voices.Count - 1 do
    begin
      item := lvVoices.Items.Add;
      item.ImageIndex := 1;
      item.TagObject := FOpenAITTS;
      item.Text := FOpenAITTS.Voices[i].VoiceName;
      item.Detail := FOpenAITTS.Voices[i].VoiceId + ' ' + FOpenAITTS.Voices[i].VoiceGender;
    end;
  end;

  if not FKeyStore.LoadApiKey('ElevenLabsAPIKey').IsEmpty then
  begin
    FElevenLabsTTS := TElevenLabsService.Create(FKeyStore.LoadApiKey('ElevenLabsAPIKey'));
    for i := 0 to FElevenLabsTTS.Voices.Count - 1 do
    begin
      item := lvVoices.Items.Add;
      item.ImageIndex := 2;
      item.TagObject := FElevenLabsTTS;
      item.Text := FElevenLabsTTS.Voices[i].VoiceName;
      item.Detail := FElevenLabsTTS.Voices[i].VoiceId + ' ' + FElevenLabsTTS.Voices[i].VoiceGender;
    end;
  end;

  if not FKeyStore.LoadApiKey('ms_cognative_service_resource_key').IsEmpty then
  begin
    FMsTTS := TMicrosoftCognitiveService.Create(FKeyStore.LoadApiKey('ms_cognative_service_resource_key'), 'australiaeast.tts.speech.microsoft.com');
    for i := 0 to FMsTTS.Voices.Count - 1 do
    begin
      item := lvVoices.Items.Add;
      item.ImageIndex := 3;
      item.TagObject := FMsTTS;
      item.Text := FMsTTS.Voices[i].VoiceName;
      item.Detail := FMsTTS.Voices[i].VoiceId + ' ' + FMsTTS.Voices[i].VoiceGender;
    end;
  end;

  if not FKeyStore.LoadApiKey('AWSAccessKey').IsEmpty then
  begin
    FPolly := TAmazonPollyService.Create(FKeyStore.LoadApiKey('AWSAccessKey'), FKeyStore.LoadApiKey('AWSSecretKey'), FKeyStore.LoadSetting('AWSRegion'));
    for i := 0 to FPolly.Voices.Count - 1 do
    begin
      item := lvVoices.Items.Add;
      item.ImageIndex := 4;
      item.TagObject := FPolly;
      item.Text := FPolly.Voices[i].VoiceName;
      item.Detail := FPolly.Voices[i].VoiceId + ' ' + FPolly.Voices[i].VoiceGender;
    end;
  end;

  if not FKeyStore.LoadApiKey('google_refreshtoken').IsEmpty then
  begin
    var   FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
    FGoogleSpeech := TGoogleSpeechService.Create(FKeyStore.LoadApiKey('google_clientid'),  FKeyStore.LoadApiKey('google_clientsecret'), 'ADUG Demo', '', FSettings);
    for i := 0 to FGoogleSpeech.Voices.Count - 1 do
    begin
      item := lvVoices.Items.Add;
      item.ImageIndex := 5;
      item.TagObject := FGoogleSpeech;
      item.Text := FGoogleSpeech.Voices[i].VoiceName;
      item.Detail := FGoogleSpeech.Voices[i].VoiceId + ' ' + FGoogleSpeech.Voices[i].VoiceGender;
    end;
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
