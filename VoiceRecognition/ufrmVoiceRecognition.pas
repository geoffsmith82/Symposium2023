unit ufrmVoiceRecognition;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.NetEncoding,
  System.IniFiles,
  System.IOUtils,
  System.JSON,
  System.SyncObjs,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.MPlayer,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.BaseImageCollection,
  Vcl.ImageCollection,
  Vcl.VirtualImage,
  sgcBase_Classes,
  sgcSocket_Classes,
  sgcTCP_Classes,
  sgcWebSocket_Classes,
  sgcWebSocket_Classes_Indy,
  sgcWebSocket_Client,
  sgcWebSocket,
  ACS_Classes,
  ACS_DXAudio,
  ACS_Misc,
  Vcl.StdCtrls,
  ACS_Streams,
  ACS_LAME,
  ACS_FLAC,
  ACS_WinMedia,
  ACS_smpeg,
  NewACIndicators,
  ACS_Wave,
  OpenAI,
  uBaseSpeech,
  uMicrosoft.Cognitive.REST,
  uElevenLabs.REST,
  uGoogleSpeech,
  uAmazon.Polly,
  uWindows.Engine,
  uAssemblyAI.SpeechToText,
  uDeepGram.SpeechToText,
  uBaseSpeechRecognition
  ;

type
  TBaseSpeechClass = class of TBaseTextToSpeech;

type
  TfrmVoiceRecognition = class(TForm)
    DXAudioIn1: TDXAudioIn;
    AudioProcessor1: TAudioProcessor;
    btnStart: TButton;
    StreamOut1: TStreamOut;
    sgcWebSocketClient1: TsgcWebSocketClient;
    mmoQuestions: TMemo;
    btnStop: TButton;
    mmoAnswers: TMemo;
    mmMainMenu: TMainMenu;
    miFile: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    miExit: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    miElevenLabsSpeechEngine: TMenuItem;
    miMicrosoftSpeechEngine: TMenuItem;
    miAmazonSpeechEngine: TMenuItem;
    miGoogleSpeechEngine: TMenuItem;
    miWindowsSpeechEngine: TMenuItem;
    Timer1: TTimer;
    miAudioInput: TMenuItem;
    VirtualImage1: TVirtualImage;
    ImageCollection1: TImageCollection;
    miSpeechRecognitionEngine: TMenuItem;
    miDeepGram: TMenuItem;
    miAssemblyAI: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure AudioProcessor1GetData(Sender: TComponent; var Buffer: Pointer; var Bytes: Cardinal);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SelectSpeechEngine(Sender: TObject);
    procedure SelectSpeechRecognitionClick(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    FTextToSpeechEngine : TBaseTextToSpeech;
    FSpeechRecognitionEngine : TBaseSpeechRecognition;
    FmemStream : TMemoryStream;
    FConnected : Boolean;
    FSendThread : TAssemblyAiSendThread;

    FTextToSpeechEngines : TObjectDictionary<string, TBaseTextToSpeech>;
    FTextSpeechEngineMenuItems : TDictionary<string, TMenuItem>;
    FTextSpeechEngineNames: TDictionary<TMenuItem, string>;

    FSpeechRecognitionEngines : TObjectDictionary<string, TBaseSpeechRecognition>;
    FSpeechRecognitionEngineMenuItems : TDictionary<string, TMenuItem>;
    FSpeechRecognitionEngineNames : TDictionary<TMenuItem, string>;

    procedure LoadAudioInputsMenu;
    procedure RegisterSpeechToTextEngine(engineClass : TBaseTextToSpeech; menuItem: TMenuItem);
    function LookupSpeechEngineClassByName(engineName: string): TBaseTextToSpeech;
    procedure RegisterSpeechRecognitionEngine(engineClass : TBaseSpeechRecognition; menuItem: TMenuItem);

    procedure OnHandleMessage(const Text: string);
    procedure OnHandleConnect(Connection: TsgcWSConnection);
    procedure OnHandleDisconnect(Connection: TsgcWSConnection);
  public
    { Public declarations }
    procedure Listen;
    procedure Speak;
  end;

var
  frmVoiceRecognition: TfrmVoiceRecognition;

implementation

{$R *.dfm}

{$I ..\Libs\apikey.inc}


procedure TfrmVoiceRecognition.RegisterSpeechToTextEngine(engineClass : TBaseTextToSpeech; menuItem: TMenuItem);
var
  engineName: string;
begin
  engineName := engineClass.SpeechEngineName;
  FTextToSpeechEngines.AddOrSetValue(engineName, engineClass);
  FTextSpeechEngineMenuItems.Add(engineName, menuItem);
  FTextSpeechEngineNames.Add(menuItem, engineName);
end;

procedure TfrmVoiceRecognition.RegisterSpeechRecognitionEngine(engineClass : TBaseSpeechRecognition; menuItem: TMenuItem);
var
  engineName: string;
begin
  engineName := engineClass.ClassName;
  FSpeechRecognitionEngines.AddOrSetValue(engineName, engineClass);
  FSpeechRecognitionEngineMenuItems.Add(engineName, menuItem);
  FTextSpeechEngineNames.Add(menuItem, engineName);
end;

function TfrmVoiceRecognition.LookupSpeechEngineClassByName(engineName: string): TBaseTextToSpeech;
begin
  Result := FTextToSpeechEngines[engineName];
end;

procedure TfrmVoiceRecognition.SelectSpeechEngine(Sender: TObject);
var
  lSpeechEngine: String;
begin
  lSpeechEngine := FTextSpeechEngineNames[Sender as TMenuItem];
  FTextToSpeechEngine := LookupSpeechEngineClassByName(lSpeechEngine);
  FSettings.WriteString('Speech', 'SelectedEngine', FTextToSpeechEngine.SpeechEngineName);
end;

procedure TfrmVoiceRecognition.SelectSpeechRecognitionClick(Sender: TObject);
var
  lSpeechEngine: String;
begin
  lSpeechEngine := FSpeechRecognitionEngineNames[Sender as TMenuItem];
  FSpeechRecognitionEngine :=  FSpeechRecognitionEngines[lSpeechEngine];
  FSettings.WriteString('Speech', 'SelectedRecognitionEngine', FSpeechRecognitionEngine.ClassName);
end;

procedure TfrmVoiceRecognition.Speak;
begin
  VirtualImage1.ImageIndex := 1;
  VirtualImage1.Update;
end;

procedure TfrmVoiceRecognition.Listen;
begin
  if FConnected then
  begin  
    VirtualImage1.ImageIndex := 0;
  end
  else
  begin
    VirtualImage1.ImageIndex := -1;
  end;
  VirtualImage1.Update;  
end;

procedure TfrmVoiceRecognition.LoadAudioInputsMenu;
var
  lAudioInput: Integer;
  i: Integer;
  mi: TMenuItem;
begin
  miAudioInput.Clear;
  lAudioInput := FSettings.ReadInteger('Audio', 'Input', 0);
  for i := 0 to DXAudioIn1.DeviceCount - 1 do
  begin
    mi := TMenuItem.Create(nil);
    mi.Caption := DXAudioIn1.DeviceName[i];
    mi.Tag := i;
    if lAudioInput = i then
    begin
      mi.Checked := True;
      DXAudioIn1.DeviceNumber := i;
    end;
    mi.GroupIndex := 10;
    mi.RadioItem := True;
    mi.AutoCheck := True;
    miAudioInput.Add(mi);
  end;
end;

procedure TfrmVoiceRecognition.FormCreate(Sender: TObject);
var
  lSpeechEngine : string;
begin
  FConnected := False;
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  FTextToSpeechEngines := TObjectDictionary<string, TBaseTextToSpeech>.Create([doOwnsValues]);
  FTextSpeechEngineMenuItems := TDictionary<string, TMenuItem>.Create;
  FTextSpeechEngineNames := TDictionary<TMenuItem, string>.Create;


  RegisterSpeechToTextEngine(TMicrosoftCognitiveService.Create(Self, ms_cognative_service_resource_key, '', 'australiaeast.tts.speech.microsoft.com'),
                  miMicrosoftSpeechEngine);

  RegisterSpeechToTextEngine(TElevenLabsService.Create(Self, ElevenLabsAPIKey, 'ADUG Demo', 'ElevenLabsAPIKey'),
                  miElevenLabsSpeechEngine);

  RegisterSpeechToTextEngine(TAmazonPollyService.Create(Self, AWSAccessKey, AWSSecretkey),
                  miAmazonSpeechEngine);//'ADUG Demo', ''););

  RegisterSpeechToTextEngine(TWindowsSpeechService.Create(Self, '','',''),
                  miWindowsSpeechEngine);

  RegisterSpeechToTextEngine(TGoogleSpeechService.Create(Self, google_clientid, google_clientsecret,'ADUG Demo', '', FSettings),
                  miGoogleSpeechEngine);

  lSpeechEngine := FSettings.ReadString('Speech', 'SelectedEngine', 'Windows');

  FTextToSpeechEngine := LookupSpeechEngineClassByName(lSpeechEngine);
  FTextSpeechEngineMenuItems[FTextToSpeechEngine.SpeechEngineName].Checked := True;

  FmemStream := TMemoryStream.Create;
  FmemStream.SetSize(100*1024*1024);

  FSendThread := TAssemblyAiSendThread.Create(True, assemblyai_key);
  FSendThread.OnHandleMessage := OnHandleMessage;
  FSendThread.OnConnect := OnHandleConnect;
  FSendThread.OnDisconnect := OnHandleDisconnect;
  LoadAudioInputsMenu;
end;

procedure TfrmVoiceRecognition.FormDestroy(Sender: TObject);
begin
  FSendThread.Terminate;
  FreeAndNil(FSendThread);
  FreeAndNil(FTextToSpeechEngines);
  FreeAndNil(FTextSpeechEngineMenuItems);
  FreeAndNil(FTextSpeechEngineNames);
  FreeAndNil(FSettings);
  FreeAndNil(FmemStream);
end;


procedure TfrmVoiceRecognition.Timer1Timer(Sender: TObject);
begin
 // OutputDebugString(PChar(MediaPlayer1.EndPos.ToString + ' ' + MediaPlayer1.Position.ToString));
  if FTextToSpeechEngine.Mode = mpStopped then
  begin
    if StreamOut1.Status <> tosPlaying then
    begin
      StreamOut1.Run;
    end;
    Listen;
  end;
  if FTextToSpeechEngine.Mode = mpPlaying then
  begin
  
  end
  else
  begin
    Listen;  
  end;
end;

procedure TfrmVoiceRecognition.OnHandleConnect(Connection: TsgcWSConnection);
begin
  mmoQuestions.Lines.Add('Connected');
  FConnected := True;
end;

procedure TfrmVoiceRecognition.OnHandleDisconnect(Connection: TsgcWSConnection);
begin
  mmoQuestions.Lines.Add('Disconnected');
  FConnected := False;
end;

procedure TfrmVoiceRecognition.OnHandleMessage(const Text: string);
var
  msg : TJSONObject;
  value : string;
  response : string;
  question : string;
begin
  msg := TJSONObject.ParseJSONValue(Text) as TJSONObject;
  if msg.TryGetValue('message_type', Value) then
  begin
    if (value = 'FinalTranscript') and (msg.Values['text'].Value<>'') and
      (FTextToSpeechEngine.Mode <> mpPlaying) then
    begin
       question := msg.Values['text'].Value;
       mmoQuestions.Lines.Add(question);

       response := TOpenAI.AskChatGPT(question, 'text-davinci-003');
       mmoAnswers.Lines.Text := response;
       mmoAnswers.Update;
       StreamOut1.Stop(False);
       FmemStream.Clear;
       Sleep(100);
       Speak;
       FTextToSpeechEngine.PlayText(response);
    end;
  end;
end;


procedure TfrmVoiceRecognition.AudioProcessor1GetData(Sender: TComponent; var Buffer: Pointer; var Bytes: Cardinal);
var
  mem : TMemoryStream;
begin
  TAudioProcessor(Sender).Input.GetData(Buffer, Bytes);

  mem := TMemoryStream.Create;
  mem.WriteData(Buffer, Bytes);
  mem.Position := 0;
  if Assigned(FSendThread) then
    FSendThread.Add(mem);

  OutputDebugString(PChar('Len ' + Bytes.ToString));
end;

procedure TfrmVoiceRecognition.btnStartClick(Sender: TObject);
begin
  StreamOut1.Stream := FmemStream;
  FSendThread.Resume;
  Sleep(100);
  StreamOut1.Run;
  Listen;
  Timer1.Enabled := True;
end;

procedure TfrmVoiceRecognition.btnStopClick(Sender: TObject);
begin
  sgcWebSocketClient1.WriteData('{ "terminate_session": True }');
  VirtualImage1.ImageIndex := -1;
  StreamOut1.Stop(False);
  Timer1.Enabled := False;
end;

procedure TfrmVoiceRecognition.miExitClick(Sender: TObject);
begin
  FSendThread.Terminate;
  Application.Terminate;
end;

end.
