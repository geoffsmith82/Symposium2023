unit UChatGPT;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IniFiles,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.MPlayer,
  System.IOUtils,
  OpenAI,
  uElevenLabs.REST,
  uMicrosoft.Cognitive.REST,
  uAmazon.Polly,
  uGoogleSpeech,
  uWindows.Engine,
  uBaseSpeech
  ;

type
  TForm1 = class(TForm)
    btnAskTheMachine: TButton;
    mmoOutput: TMemo;
    Image1: TImage;
    mmoPrompt: TMemo;
    MediaPlayer1: TMediaPlayer;
    chkSpeak: TCheckBox;
    btnSpeakQuestion: TButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    miExit: TMenuItem;
    SpeechEngine1: TMenuItem;
    miMicrosoftSpeechEngine: TMenuItem;
    miAmazonSpeechEngine: TMenuItem;
    miGoogleSpeechEngine: TMenuItem;
    miElevenLabsSpeechEngine: TMenuItem;
    miWindowsSpeechEngine: TMenuItem;
    ModelMenu: TMenuItem;
    miTextDavinci003: TMenuItem;
    miTextCurie0011: TMenuItem;
    miTextBabbage001: TMenuItem;
    miTextAda0011: TMenuItem;
    procedure miAmazonSpeechEngineClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAskTheMachineClick(Sender: TObject);
    procedure btnGoogleAuthClick(Sender: TObject);
    procedure btnSpeakQuestionClick(Sender: TObject);
    procedure miElevenLabsSpeechEngineClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miGoogleSpeechEngineClick(Sender: TObject);
    procedure miMicrosoftSpeechEngineClick(Sender: TObject);
    procedure miWindowsSpeechEngineClick(Sender: TObject);
    procedure miTextDavinci003Click(Sender: TObject);
  private
    SpeechEngine : TBaseSpeech;
    MsVoiceService : TMicrosoftCognitiveService;
    ElevenLabsVoiceService : TElevenLabsService;
    AmazonPolyVoiceService : TAmazonPollyService;
    GoogleVoiceService : TGoogleSpeechService;
    WindowsVoiceService : TWindowsSpeechService;
    Settings : TIniFile;
    procedure PlayTextWithSelectedEngine(text: string);
    function SelectedModel: string;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{$I ..\Libs\apikey.inc}

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MsVoiceService);
  FreeAndNil(ElevenLabsVoiceService);
  FreeAndNil(AmazonPolyVoiceService);
  FreeAndNil(GoogleVoiceService);
  FreeAndNil(WindowsVoiceService);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  lSpeechEngine: string;
  i: Integer;
  currentModel : string;
begin
  MsVoiceService := TMicrosoftCognitiveService.Create(ms_cognative_service_resource_key, 'australiaeast.tts.speech.microsoft.com');
  MsVoiceService.GetAccessToken;
  ElevenLabsVoiceService := TElevenLabsService.Create(ElevenLabsAPIKey, 'ADUG Demo', 'ElevenLabsAPIKey');
  AmazonPolyVoiceService := TAmazonPollyService.Create(AWSAccessKey, AWSSecretkey);//'ADUG Demo', '');
  WindowsVoiceService := TWindowsSpeechService.Create('','','');
  GoogleVoiceService := TGoogleSpeechService.Create('','ADUG Demo', '');
  Settings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  SpeechEngine := WindowsVoiceService;
  lSpeechEngine := Settings.ReadString('Speech', 'SelectedEngine', 'Windows');
  if lSpeechEngine.Contains(ElevenLabsVoiceService.SpeechEngineName) then
  begin
    SpeechEngine := ElevenLabsVoiceService;
    miElevenLabsSpeechEngine.Checked := True;
  end
  else if lSpeechEngine.Contains(MsVoiceService.SpeechEngineName) then
  begin
    SpeechEngine := MsVoiceService;
    miMicrosoftSpeechEngine.Checked := True;
  end
  else if lSpeechEngine.Contains(AmazonPolyVoiceService.SpeechEngineName) then
  begin
    SpeechEngine := AmazonPolyVoiceService;
    miAmazonSpeechEngine.Checked := True;
  end
  else if lSpeechEngine.Contains(GoogleVoiceService.SpeechEngineName) then
  begin
    SpeechEngine := GoogleVoiceService;
    miGoogleSpeechEngine.Checked := True;
  end
  else if lSpeechEngine.Contains(WindowsVoiceService.SpeechEngineName) then
  begin
    SpeechEngine := WindowsVoiceService;
    miWindowsSpeechEngine.Checked := True;
  end
  else
  begin
    SpeechEngine := WindowsVoiceService;  // default engine
    miWindowsSpeechEngine.Checked := True;
  end;
  currentModel := Settings.ReadString('ChatGPT', 'Model', 'text-davinci-003').Replace('&', '');
  for i := 0 to ModelMenu.Count - 1 do
  begin
    if ModelMenu.Items[i].Caption = currentModel then
    begin
      ModelMenu.Items[i].Click;
      break;
    end;
  end;
end;

function TForm1.SelectedModel: string;
var
  i : Integer;
begin
  for i := 0 to ModelMenu.Count - 1 do
  begin
    if ModelMenu.Items[i].Checked then
    begin
      Result := ModelMenu.Items[i].Caption.Replace('&', '');
    end;
  end;
end;


procedure TForm1.btnAskTheMachineClick(Sender: TObject);
var
  OldCursor : TCursor;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    mmoOutput.Lines.Text := TOpenAI.AskChatGPT(mmoPrompt.Text, SelectedModel);
    Update;
    if chkSpeak.Checked then
    begin
      PlayTextWithSelectedEngine(mmoOutput.Lines.Text);
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TForm1.PlayTextWithSelectedEngine(text:string);
var
  Stream: TMemoryStream;
  FileName: string;
begin
  Stream := TMemoryStream.Create;
  try
    Stream := SpeechEngine.TextToSpeech(text);
    if not Assigned(Stream) then
      Exit;
    FileName := TPath.GetTempFileName + '.mp3';
    Stream.Position := 0;
    Stream.SaveToFile(FileName);
  finally
    Stream.Free;
  end;
  MediaPlayer1.FileName := FileName;
  MediaPlayer1.Open;
  MediaPlayer1.Play;
end;

procedure TForm1.btnGoogleAuthClick(Sender: TObject);
begin
  GoogleVoiceService.Authenticate(Settings);
end;

procedure TForm1.btnSpeakQuestionClick(Sender: TObject);
begin
  PlayTextWithSelectedEngine(mmoPrompt.Lines.Text);
end;

procedure TForm1.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.miAmazonSpeechEngineClick(Sender: TObject);
begin
  SpeechEngine := AmazonPolyVoiceService;
  Settings.WriteString('Speech', 'SelectedEngine', SpeechEngine.SpeechEngineName);
end;

procedure TForm1.miElevenLabsSpeechEngineClick(Sender: TObject);
begin
  SpeechEngine := ElevenLabsVoiceService;
  Settings.WriteString('Speech', 'SelectedEngine', SpeechEngine.SpeechEngineName);
end;

procedure TForm1.miGoogleSpeechEngineClick(Sender: TObject);
begin
  SpeechEngine := GoogleVoiceService;
  Settings.WriteString('Speech', 'SelectedEngine', SpeechEngine.SpeechEngineName);
end;

procedure TForm1.miMicrosoftSpeechEngineClick(Sender: TObject);
begin
  SpeechEngine := MsVoiceService;
  Settings.WriteString('Speech', 'SelectedEngine', SpeechEngine.SpeechEngineName);
end;

procedure TForm1.miWindowsSpeechEngineClick(Sender: TObject);
begin
  SpeechEngine := WindowsVoiceService;
  Settings.WriteString('Speech', 'SelectedEngine', SpeechEngine.SpeechEngineName);
end;

procedure TForm1.miTextDavinci003Click(Sender: TObject);
var
  modelMenuItem : TMenuItem;
begin
  modelMenuItem := Sender as TMenuItem;
  Settings.WriteString('ChatGPT', 'Model', modelMenuItem.Caption.Replace('&', ''));
end;

end.
