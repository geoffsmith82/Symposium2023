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
  Vcl.DBCGrids,
  Vcl.DBCtrls,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  FireDAC.Phys.MSAcc,
  FireDAC.Phys.MSAccDef,
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
  uBaseSpeechRecognition,
  uEngineManager
  ;

type
  TfrmVoiceRecognition = class(TForm)
    DXAudioIn1: TDXAudioIn;
    AudioProcessor1: TAudioProcessor;
    StreamOut1: TStreamOut;
    sgcWebSocketClient1: TsgcWebSocketClient;
    mmoQuestions: TMemo;
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
    UserInterfaceUpdateTimer: TTimer;
    miAudioInput: TMenuItem;
    VirtualImage1: TVirtualImage;
    ImageCollection1: TImageCollection;
    miSpeechRecognitionEngine: TMenuItem;
    miDeepGram: TMenuItem;
    miAssemblyAI: TMenuItem;
    DBCtrlGrid1: TDBCtrlGrid;
    FDConnection: TFDConnection;
    tblSessions: TFDTable;
    tblConversion: TFDTable;
    dsSessions: TDataSource;
    dsConversation: TDataSource;
    PnlButtons: TPanel;
    btnNewChatSession: TButton;
    btnStart: TButton;
    btnStop: TButton;
    DBText1: TDBText;
    procedure FormCreate(Sender: TObject);
    procedure AudioProcessor1GetData(Sender: TComponent; var Buffer: Pointer; var Bytes: Cardinal);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure UserInterfaceUpdateTimerTimer(Sender: TObject);
    procedure SelectSpeechEngine(Sender: TObject);
    procedure SelectSpeechRecognitionClick(Sender: TObject);
    procedure btnNewChatSessionClick(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    FmemStream : TMemoryStream;
    FConnected : Boolean;
    FTextToSpeechEngines : TEngineManager<TBaseTextToSpeech>;
    FSpeechRecognitionEngines : TEngineManager<TBaseSpeechRecognition>;

    procedure LoadAudioInputsMenu;

    procedure OnHandleMessage(const Text: string);
    procedure OnHandleConnect(Connection: TsgcWSConnection);
    procedure OnHandleDisconnect(Connection: TsgcWSConnection);
    procedure SetupTextToSpeechEngines;
    procedure SetupSpeechRecognitionEngines;
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

procedure TfrmVoiceRecognition.SelectSpeechEngine(Sender: TObject);
begin
  FTextToSpeechEngines.SelectEngine(Sender as TMenuItem);
  FSettings.WriteString('Speech', 'SelectedTextToSpeechEngine', FTextToSpeechEngines.ActiveEngine.ClassName);
end;

procedure TfrmVoiceRecognition.SelectSpeechRecognitionClick(Sender: TObject);
begin
  FSpeechRecognitionEngines.SelectEngine(Sender as TMenuItem);
  FSettings.WriteString('Speech', 'SelectedRecognitionEngine', FSpeechRecognitionEngines.ActiveEngine.ClassName);
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

procedure TfrmVoiceRecognition.SetupTextToSpeechEngines;
var
  lSpeechEngine: string;
begin
  FTextToSpeechEngines.RegisterEngine(TMicrosoftCognitiveService.Create(Self, ms_cognative_service_resource_key, '', 'australiaeast.tts.speech.microsoft.com'), miMicrosoftSpeechEngine);
  FTextToSpeechEngines.RegisterEngine(TElevenLabsService.Create(Self, ElevenLabsAPIKey, 'ADUG Demo', 'ElevenLabsAPIKey'), miElevenLabsSpeechEngine);
  FTextToSpeechEngines.RegisterEngine(TAmazonPollyService.Create(Self, AWSAccessKey, AWSSecretkey), miAmazonSpeechEngine);
  FTextToSpeechEngines.RegisterEngine(TWindowsSpeechService.Create(Self, '', '', ''), miWindowsSpeechEngine);
  FTextToSpeechEngines.RegisterEngine(TGoogleSpeechService.Create(Self, google_clientid, google_clientsecret, 'ADUG Demo', '', FSettings), miGoogleSpeechEngine);

  lSpeechEngine := FSettings.ReadString('Speech', 'SelectedTextToSpeechEngine', 'TWindowsSpeechService');
  FTextToSpeechEngines.SelectEngine(lSpeechEngine);
  FTextToSpeechEngines.ActiveMenuItem.Checked := True;
end;

procedure TfrmVoiceRecognition.SetupSpeechRecognitionEngines;
var
  lSpeechEngine: string;
  lAssemblyAi : TAssemblyAiRecognition;
  lDeepGram : TDeepGramRecognition;
begin
  lAssemblyAi := TAssemblyAiRecognition.Create(assemblyai_key,'','');
  lAssemblyAi.OnHandleMessage := OnHandleMessage;
  lAssemblyAi.OnConnect := OnHandleConnect;
  lAssemblyAi.OnDisconnect := OnHandleDisconnect;
  FSpeechRecognitionEngines.RegisterEngine(lAssemblyAi, miAssemblyAI);

  lDeepGram := TDeepGramRecognition.Create(deepgram_key,'','');
  lDeepGram.OnHandleMessage := OnHandleMessage;
  lDeepGram.OnConnect := OnHandleConnect;
  lDeepGram.OnDisconnect := OnHandleDisconnect;
  FSpeechRecognitionEngines.RegisterEngine(lDeepGram, miDeepGram);

  lSpeechEngine := FSettings.ReadString('Speech', 'SelectedRecognitionEngine', 'TAssemblyAiRecognition');
  FSpeechRecognitionEngines.SelectEngine(lSpeechEngine);
  FSpeechRecognitionEngines.ActiveEngine.DoSelectEngine;
  FSpeechRecognitionEngines.ActiveMenuItem.Checked := True;
end;

procedure TfrmVoiceRecognition.FormCreate(Sender: TObject);
begin
  FConnected := False;
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  FTextToSpeechEngines :=  TEngineManager<TBaseTextToSpeech>.Create;
  FSpeechRecognitionEngines := TEngineManager<TBaseSpeechRecognition>.Create;

  tblSessions.Active := True;
  tblConversion.Active := True;

  SetupTextToSpeechEngines;
  SetupSpeechRecognitionEngines;

  FmemStream := TMemoryStream.Create;
  FmemStream.SetSize(50*1024*1024); // not used but needed for the stream out

  LoadAudioInputsMenu;
end;

procedure TfrmVoiceRecognition.FormDestroy(Sender: TObject);
begin
  UserInterfaceUpdateTimer.Enabled := False;

  FreeAndNil(FSettings);
  FreeAndNil(FmemStream);
  FreeAndNil(FTextToSpeechEngines);
  FreeAndNil(FSpeechRecognitionEngines);
end;


procedure TfrmVoiceRecognition.UserInterfaceUpdateTimerTimer(Sender: TObject);
begin
 // OutputDebugString(PChar(MediaPlayer1.EndPos.ToString + ' ' + MediaPlayer1.Position.ToString));
  if FTextToSpeechEngines.ActiveEngine.Mode = mpStopped then
  begin
    if StreamOut1.Status <> tosPlaying then
    begin
      StreamOut1.Run;
    end;
    Listen;
  end;
  if FTextToSpeechEngines.ActiveEngine.Mode = mpPlaying then
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
      (FTextToSpeechEngines.ActiveEngine.Mode <> mpPlaying) then
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
       FTextToSpeechEngines.ActiveEngine.PlayText(response);
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

  FSpeechRecognitionEngines.ActiveEngine.Add(mem);

  OutputDebugString(PChar('Len ' + Bytes.ToString));
end;

procedure TfrmVoiceRecognition.btnNewChatSessionClick(Sender: TObject);
begin
  DBCtrlGrid1.DataSource.DataSet.Append;
  DBCtrlGrid1.DataSource.DataSet.FieldByName('CreationTime').AsDateTime := now;
  DBCtrlGrid1.DataSource.DataSet.Post;
end;

procedure TfrmVoiceRecognition.btnStartClick(Sender: TObject);
begin
  StreamOut1.Stream := FmemStream;
  FSpeechRecognitionEngines.ActiveEngine.Resume;
  Sleep(100);
  StreamOut1.Run;
  Listen;
  UserInterfaceUpdateTimer.Enabled := True;
end;

procedure TfrmVoiceRecognition.btnStopClick(Sender: TObject);
begin
  FSpeechRecognitionEngines.ActiveEngine.Finish;
  VirtualImage1.ImageIndex := -1;
  StreamOut1.Stop(False);
  UserInterfaceUpdateTimer.Enabled := False;
end;

procedure TfrmVoiceRecognition.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.
