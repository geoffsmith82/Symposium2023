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
    MediaPlayer1: TMediaPlayer;
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
    procedure miAmazonSpeechEngineClick(Sender: TObject);
    procedure miElevenLabsSpeechEngineClick(Sender: TObject);
    procedure miGoogleSpeechEngineClick(Sender: TObject);
    procedure miMicrosoftSpeechEngineClick(Sender: TObject);
    procedure miWindowsSpeechEngineClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    FSpeechEngine : TBaseSpeech;
    FMsVoiceService : TMicrosoftCognitiveService;
    FElevenLabsVoiceService : TElevenLabsService;
    FAmazonPolyVoiceService : TAmazonPollyService;
    FGoogleVoiceService : TGoogleSpeechService;
    FWindowsVoiceService : TWindowsSpeechService;
    FmemStream : TMemoryStream;
    FSendThread : TAssemblyAiSendThread;
    procedure OnHandleMessage(const Text: string);
    procedure PlayTextWithSelectedEngine(const text:string);
    procedure NotifyProc(Sender: TObject);
    procedure OnHandleConnect(Connection: TsgcWSConnection);
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

procedure TfrmVoiceRecognition.PlayTextWithSelectedEngine(const text:string);
var
  Stream: TMemoryStream;
  FileName: string;
begin
  MediaPlayer1.Notify := true;
  MediaPlayer1.OnNotify := NotifyProc;
  Stream := TMemoryStream.Create;
  try
    Stream := FSpeechEngine.TextToSpeech(text);
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
  MediaPlayer1.Notify := true;
  MediaPlayer1.Play;
end;

procedure TfrmVoiceRecognition.Speak;
begin
  VirtualImage1.ImageIndex := 1;
  VirtualImage1.Update;
end;

procedure TfrmVoiceRecognition.FormCreate(Sender: TObject);
var
  i : Integer;
  lSpeechEngine : string;
  lAudioInput : Integer;
  mi : TMenuItem;
begin
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  lSpeechEngine := FSettings.ReadString('Speech', 'SelectedEngine', 'Windows');
  FMsVoiceService := TMicrosoftCognitiveService.Create(ms_cognative_service_resource_key, 'australiaeast.tts.speech.microsoft.com');
  FElevenLabsVoiceService := TElevenLabsService.Create(ElevenLabsAPIKey, 'ADUG Demo', 'ElevenLabsAPIKey');
  FAmazonPolyVoiceService := TAmazonPollyService.Create(AWSAccessKey, AWSSecretkey);//'ADUG Demo', '');
  FWindowsVoiceService := TWindowsSpeechService.Create('','','');
  FGoogleVoiceService := TGoogleSpeechService.Create(google_clientid, google_clientsecret,'ADUG Demo', '', FSettings);
  FSpeechEngine := FAmazonPolyVoiceService;


  if lSpeechEngine.Contains(FElevenLabsVoiceService.SpeechEngineName) then
  begin
    FSpeechEngine := FElevenLabsVoiceService;
    miElevenLabsSpeechEngine.Checked := True;
  end
  else if lSpeechEngine.Contains(FMsVoiceService.SpeechEngineName) then
  begin
    FSpeechEngine := FMsVoiceService;
    miMicrosoftSpeechEngine.Checked := True;
  end
  else if lSpeechEngine.Contains(FAmazonPolyVoiceService.SpeechEngineName) then
  begin
    FSpeechEngine := FAmazonPolyVoiceService;
    miAmazonSpeechEngine.Checked := True;
  end
  else if lSpeechEngine.Contains(FGoogleVoiceService.SpeechEngineName) then
  begin
    FSpeechEngine := FGoogleVoiceService;
    miGoogleSpeechEngine.Checked := True;
  end
  else if lSpeechEngine.Contains(FWindowsVoiceService.SpeechEngineName) then
  begin
    FSpeechEngine := FWindowsVoiceService;
    miWindowsSpeechEngine.Checked := True;
  end
  else
  begin
    FSpeechEngine := FWindowsVoiceService;  // default engine
    miWindowsSpeechEngine.Checked := True;
  end;

  FmemStream := TMemoryStream.Create;
  FmemStream.SetSize(100*1024*1024);

  FSendThread := TAssemblyAiSendThread.Create(True, assemblyai_key);
  FSendThread.OnHandleMessage := OnHandleMessage;
  FSendThread.OnConnect := OnHandleConnect;

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

procedure TfrmVoiceRecognition.FormDestroy(Sender: TObject);
begin
  FSendThread.Terminate;
  FreeAndNil(FSendThread);
end;

procedure TfrmVoiceRecognition.Listen;
begin
  VirtualImage1.ImageIndex := 0;
  VirtualImage1.Update;
end;

procedure TfrmVoiceRecognition.Timer1Timer(Sender: TObject);
begin
  OutputDebugString(PChar(MediaPlayer1.EndPos.ToString + ' ' + MediaPlayer1.Position.ToString));
  if Mediaplayer1.Mode = mpStopped then
  begin
    if StreamOut1.Status <> tosPlaying then
    begin
      StreamOut1.Run;
      Listen;
    end;
  end;
end;

procedure TfrmVoiceRecognition.NotifyProc(Sender: TObject);
begin
  OutputDebugString(PChar('Mediaplayer NotifyProc'));
  with Sender as TMediaPlayer do
  begin
    case Mode of
      mpStopped:
      begin{do something here}
        OutputDebugString(PChar('Mediaplayer Stopped'));
      end;
    end;
    //must set to true to enable next-time notification
    Notify := True;
  end;
end;

procedure TfrmVoiceRecognition.OnHandleConnect(Connection: TsgcWSConnection);
begin
  Memo1.Lines.Add('Connected');
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
      (Mediaplayer1.Mode <> mpPlaying) then
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
       PlayTextWithSelectedEngine(response);
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
  Listen;
  Sleep(100);
  StreamOut1.Run;
end;

procedure TfrmVoiceRecognition.btnStopClick(Sender: TObject);
begin
  sgcWebSocketClient1.WriteData('{ "terminate_session": True }');
  VirtualImage1.ImageIndex := -1;
  StreamOut1.Stop;
end;

procedure TfrmVoiceRecognition.Exit1Click(Sender: TObject);
begin
  FSendThread.Terminate;
  Application.Terminate;
end;

procedure TfrmVoiceRecognition.miAmazonSpeechEngineClick(Sender: TObject);
begin
  FSpeechEngine := FAmazonPolyVoiceService;
  FSettings.WriteString('Speech', 'SelectedEngine', FSpeechEngine.SpeechEngineName);
end;

procedure TfrmVoiceRecognition.miElevenLabsSpeechEngineClick(Sender: TObject);
begin
  FSpeechEngine := FElevenLabsVoiceService;
  FSettings.WriteString('Speech', 'SelectedEngine', FSpeechEngine.SpeechEngineName);
end;

procedure TfrmVoiceRecognition.miGoogleSpeechEngineClick(Sender: TObject);
begin
  FSpeechEngine := FGoogleVoiceService;
  FSettings.WriteString('Speech', 'SelectedEngine', FSpeechEngine.SpeechEngineName);
end;

procedure TfrmVoiceRecognition.miMicrosoftSpeechEngineClick(Sender: TObject);
begin
  FSpeechEngine := FMsVoiceService;
  FSettings.WriteString('Speech', 'SelectedEngine', FSpeechEngine.SpeechEngineName);
end;

procedure TfrmVoiceRecognition.miWindowsSpeechEngineClick(Sender: TObject);
begin
  FSpeechEngine := FWindowsVoiceService;
  FSettings.WriteString('Speech', 'SelectedEngine', FSpeechEngine.SpeechEngineName);
end;

end.
