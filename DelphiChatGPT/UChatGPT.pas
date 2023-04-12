unit UChatGPT;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IniFiles,
  System.Generics.Collections,
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
    procedure FormCreate(Sender: TObject);
    procedure btnAskTheMachineClick(Sender: TObject);
    procedure btnGoogleAuthClick(Sender: TObject);
    procedure btnSpeakQuestionClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miTextDavinci003Click(Sender: TObject);
    procedure SelectSpeechEngine(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    SpeechEngine : TBaseSpeech;
    SpeechEngines : TObjectDictionary<string, TBaseSpeech>;
    SpeechEngineMenuItems : TDictionary<string, TMenuItem>;
    SpeechEngineNames: TDictionary<TMenuItem, string>;
    Settings : TIniFile;
    procedure PlayTextWithSelectedEngine(text: string);
    function SelectedModel: string;
    procedure RegisterSpeechToTextEngine(menuItem: TMenuItem; engineClass: TBaseSpeech);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{$I ..\Libs\apikey.inc}

procedure TForm1.RegisterSpeechToTextEngine(menuItem: TMenuItem; engineClass : TBaseSpeech);
var
  engineName: string;
begin
  engineName := engineClass.SpeechEngineName;
  SpeechEngines.AddOrSetValue(engineName, engineClass);
  SpeechEngineMenuItems.Add(engineName, menuItem);
  SpeechEngineNames.Add(menuItem, engineName);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  lSpeechEngine: string;
  i: Integer;
  currentModel : string;
  menu: TMenuItem;
begin
  SpeechEngines := TObjectDictionary<string, TBaseSpeech>.Create([doOwnsValues]);
  SpeechEngineMenuItems := TDictionary<string, TMenuItem>.Create;
  SpeechEngineNames := TDictionary<TMenuItem, string>.Create;
  Settings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));

  RegisterSpeechToTextEngine(miMicrosoftSpeechEngine,
     TMicrosoftCognitiveService.Create(Self, ms_cognative_service_resource_key, '', 'australiaeast.tts.speech.microsoft.com'));
  RegisterSpeechToTextEngine(miElevenLabsSpeechEngine,
     TElevenLabsService.Create(Self, ElevenLabsAPIKey, 'ADUG Demo', 'ElevenLabsAPIKey'));
  RegisterSpeechToTextEngine(miAmazonSpeechEngine,
     TAmazonPollyService.Create(Self, AWSAccessKey, AWSSecretkey));//'ADUG Demo', '');
  RegisterSpeechToTextEngine(miWindowsSpeechEngine,
     TWindowsSpeechService.Create(Self, '','',''));
  RegisterSpeechToTextEngine(miGoogleSpeechEngine,
     TGoogleSpeechService.Create(Self, google_clientid, google_clientsecret,'ADUG Demo', '', Settings));

  lSpeechEngine := Settings.ReadString('Speech', 'SelectedEngine', 'Windows');
  SpeechEngines.TryGetValue(lSpeechEngine, SpeechEngine);
  if SpeechEngineMenuItems.TryGetValue(lSpeechEngine, menu) then
  begin
    menu.Checked := True;
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

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SpeechEngines);
  FreeAndNil(SpeechEngineMenuItems);
  FreeAndNil(SpeechEngineNames);
  FreeAndNil(Settings);
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


procedure TForm1.SelectSpeechEngine(Sender: TObject);
var
  engine : string;
begin
  engine := SpeechEngineNames[Sender as TMenuItem];
  SpeechEngine := SpeechEngines[engine];
  (Sender as TMenuItem).Checked := True;
  Settings.WriteString('Speech', 'SelectedEngine', engine);
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
  (SpeechEngine as TGoogleSpeechService).Authenticate;
end;

procedure TForm1.btnSpeakQuestionClick(Sender: TObject);
begin
  PlayTextWithSelectedEngine(mmoPrompt.Lines.Text);
end;

procedure TForm1.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.miTextDavinci003Click(Sender: TObject);
var
  modelMenuItem : TMenuItem;
begin
  modelMenuItem := Sender as TMenuItem;
  Settings.WriteString('ChatGPT', 'Model', modelMenuItem.Caption.Replace('&', ''));
end;

end.
