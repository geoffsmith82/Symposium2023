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
  uDeepGram.SpeechToText
  ;

type
  TBaseSpeechClass = class of TBaseSpeech;

type
  TfrmVoiceRecognition = class(TForm)
    DXAudioIn1: TDXAudioIn;
    AudioProcessor1: TAudioProcessor;
    btnStart: TButton;
    StreamOut1: TStreamOut;
    sgcWebSocketClient1: TsgcWebSocketClient;
    Memo1: TMemo;
    btnStop: TButton;
    Memo2: TMemo;
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
    miElevenLabsSpeechEngine: TMenuItem;
    miMicrosoftSpeechEngine: TMenuItem;
    miAmazonSpeechEngine: TMenuItem;
    miGoogleSpeechEngine: TMenuItem;
    miWindowsSpeechEngine: TMenuItem;
    Timer1: TTimer;
    miAudioInput: TMenuItem;
    VirtualImage1: TVirtualImage;
    ImageCollection1: TImageCollection;
    procedure FormCreate(Sender: TObject);
    procedure AudioProcessor1GetData(Sender: TComponent; var Buffer: Pointer; var Bytes: Cardinal);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SelectSpeechEngine(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    FSpeechEngine : TBaseSpeech;
    FmemStream : TMemoryStream;
    FConnected : Boolean;
    FSendThread : TAssemblyAiSendThread;
    SpeechEngines : TObjectDictionary<string, TBaseSpeech>;
    SpeechEngineMenuItems : TDictionary<string, TMenuItem>;
    SpeechEngineNames: TDictionary<TMenuItem, string>;
    procedure LoadAudioInputsMenu;
    procedure RegisterSpeechToTextEngine(engineClass : TBaseSpeech; menuItem: TMenuItem);
    function LookupSpeechEngineClassByName(engineName: string): TBaseSpeech;
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


procedure TfrmVoiceRecognition.RegisterSpeechToTextEngine(engineClass : TBaseSpeech; menuItem: TMenuItem);
var
  engineName: string;
begin
  engineName := engineClass.SpeechEngineName;
  SpeechEngines.AddOrSetValue(engineName, engineClass);
  SpeechEngineMenuItems.Add(engineName, menuItem);
  SpeechEngineNames.Add(menuItem, engineName);
end;

function TfrmVoiceRecognition.LookupSpeechEngineClassByName(engineName: string): TBaseSpeech;
begin
  Result := SpeechEngines[engineName];
end;

procedure TfrmVoiceRecognition.SelectSpeechEngine(Sender: TObject);
var
  lSpeechEngine: String;
begin
  lSpeechEngine := SpeechEngineNames[Sender as TMenuItem];
  FSpeechEngine := LookupSpeechEngineClassByName(lSpeechEngine);
  FSettings.WriteString('Speech', 'SelectedEngine', FSpeechEngine.SpeechEngineName);
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
  SpeechEngines := TObjectDictionary<string, TBaseSpeech>.Create;
  SpeechEngineMenuItems := TDictionary<string, TMenuItem>.Create;
  SpeechEngineNames := TDictionary<TMenuItem, string>.Create;


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

  FSpeechEngine := LookupSpeechEngineClassByName(lSpeechEngine);
  SpeechEngineMenuItems[FSpeechEngine.SpeechEngineName].Checked := True;

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
  FreeAndNil(SpeechEngines);
  FreeAndNil(SpeechEngineMenuItems);
  FreeAndNil(SpeechEngineNames);
  FreeAndNil(FSettings);
end;


procedure TfrmVoiceRecognition.Timer1Timer(Sender: TObject);
begin
 // OutputDebugString(PChar(MediaPlayer1.EndPos.ToString + ' ' + MediaPlayer1.Position.ToString));
  if FSpeechEngine.Mode = mpStopped then
  begin
    if StreamOut1.Status <> tosPlaying then
    begin
      StreamOut1.Run;
    end;
    Listen;
  end;
  if FSpeechEngine.Mode = mpPlaying then
  begin
  
  end
  else
  begin
    Listen;  
  end;
  
end;

procedure TfrmVoiceRecognition.OnHandleConnect(Connection: TsgcWSConnection);
begin
  Memo1.Lines.Add('Connected');
  FConnected := True;
end;

procedure TfrmVoiceRecognition.OnHandleDisconnect(Connection: TsgcWSConnection);
begin
  Memo1.Lines.Add('Disconnected');
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
      (FSpeechEngine.Mode <> mpPlaying) then
    begin
       question := msg.Values['text'].Value;
       Memo1.Lines.Add(question);

       response := TOpenAI.AskChatGPT(question, 'text-davinci-003');
       Memo2.Lines.Text := response;
       Memo2.Update;
       StreamOut1.Stop(False);
       FmemStream.Clear;
       Sleep(100);
       Speak;
       FSpeechEngine.PlayText(response);
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

procedure TfrmVoiceRecognition.Exit1Click(Sender: TObject);
begin
  FSendThread.Terminate;
  Application.Terminate;
end;

end.
