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
  System.Threading,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.MPlayer,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.BaseImageCollection,
  Vcl.ImageCollection,
  Vcl.VirtualImage,
  Vcl.DBCGrids,
  Vcl.DBCtrls,
  Vcl.DBGrids,
  Vcl.Grids,
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
  uLLM,
  OpenAI,
  uAzureGPT,
  uBaseSpeech,
  uMicrosoft.Cognitive.REST,
  uElevenLabs.REST,
  uGoogleSpeech,
  uAmazon.Polly,
  uWindows.Engine,
  uAssemblyAI.SpeechToText,
  uDeepGram.SpeechToText,
  uBaseSpeechRecognition,
  uEngineManager,
  AdvUtil,
  AdvObj,
  BaseGrid,
  AdvGrid,
  DBAdvGrid
  ;

type
  TRecognitionStatus = (rsListening, rsThinking, rsSpeaking , rsStopped);
  TfrmVoiceRecognition = class(TForm)
    DXAudioIn: TDXAudioIn;
    AudioProcessor: TAudioProcessor;
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
    miAudioInput: TMenuItem;
    ImageCollection: TImageCollection;
    miSpeechRecognitionEngine: TMenuItem;
    miDeepGram: TMenuItem;
    miAssemblyAI: TMenuItem;
    DBCtrlGrid1: TDBCtrlGrid;
    FDConnection: TFDConnection;
    tblSessions: TFDTable;
    tblConversation: TFDTable;
    dsSessions: TDataSource;
    dsConversation: TDataSource;
    PnlButtons: TPanel;
    btnNewChatSession: TButton;
    btnStart: TButton;
    btnStop: TButton;
    DBText1: TDBText;
    NULLOut: TNULLOut;
    StatusBar: TStatusBar;
    DBAdvGrid1: TDBAdvGrid;
    Panel1: TPanel;
    VirtualImage1: TVirtualImage;
    DBText2: TDBText;
    btnDeleteSession: TButton;
    btnDeleteMessage: TButton;
    procedure FormCreate(Sender: TObject);
    procedure AudioProcessorGetData(Sender: TComponent; var Buffer: Pointer; var Bytes: Cardinal);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure SelectSpeechEngine(Sender: TObject);
    procedure SelectSpeechRecognitionClick(Sender: TObject);
    procedure btnNewChatSessionClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tblSessionsAfterScroll(DataSet: TDataSet);
    procedure DBCtrlGrid1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure btnDeleteSessionClick(Sender: TObject);
    procedure Model2Click(Sender: TObject);
    procedure btnDeleteMessageClick(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    FConnected : Boolean;
    FShouldBeListening : Boolean;
    FTextToSpeechEngines : TEngineManager<TBaseTextToSpeech>;
    FSpeechRecognitionEngines : TEngineManager<TBaseSpeechRecognition>;
    task : ITask;
    FStatus : TRecognitionStatus;
    FOpenAI : TOpenAI;

    procedure LoadAudioInputsMenu;

    procedure OnHandleSpeechRecognitionCompletion(const Text: string);
    procedure OnHandleConnect(Connection: TObject);
    procedure OnHandleDisconnect(Connection: TObject);
    procedure SetupTextToSpeechEngines;
    procedure SetupSpeechRecognitionEngines;
    procedure OnHandleChatResponse(SessionID: Int64 ;ChatResponse: TChatResponse);
    procedure AsyncSendChatMessagesToOpenAI(ASessionID: Int64;
      AChatMessages: TObjectList<TChatMessage>;
      AOnMessageResults: TOnChatMessageMessageResults);
    procedure OnFinishedPlaying(Sender: TObject);
  public
    { Public declarations }
    procedure ShowListening;
    procedure ShowSpeaking;
    procedure ShowThinking;
    procedure StopListening;
    procedure StartListening;
  end;

var
  frmVoiceRecognition: TfrmVoiceRecognition;

implementation

{$R *.dfm}

uses ufrmChatSession;

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

procedure TfrmVoiceRecognition.ShowSpeaking;
begin
  VirtualImage1.ImageIndex := 1;
  VirtualImage1.Update;
end;

procedure TfrmVoiceRecognition.ShowThinking;
begin
  VirtualImage1.ImageIndex := 2;
  VirtualImage1.Update;
end;

procedure TfrmVoiceRecognition.StartListening;
begin
  FShouldBeListening := True;
  FStatus := TRecognitionStatus.rsListening;
  FSpeechRecognitionEngines.ActiveEngine.Resume;
  NULLOut.Run;
  ShowListening;
end;

procedure TfrmVoiceRecognition.StopListening;
begin
  FShouldBeListening := False;
  FSpeechRecognitionEngines.ActiveEngine.Finish;
  VirtualImage1.ImageIndex := -1;
  NULLOut.Stop(False);
end;

procedure TfrmVoiceRecognition.ShowListening;
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

procedure TfrmVoiceRecognition.tblSessionsAfterScroll(DataSet: TDataSet);
begin
  DBAdvGrid1.AutoSizeRows(True, 4);
end;

procedure TfrmVoiceRecognition.LoadAudioInputsMenu;
var
  lAudioInput: Integer;
  i: Integer;
  mi: TMenuItem;
begin
  miAudioInput.Clear;
  lAudioInput := FSettings.ReadInteger('Audio', 'Input', 0);
  for i := 0 to DXAudioIn.DeviceCount - 1 do
  begin
    mi := TMenuItem.Create(nil);
    mi.Caption := DXAudioIn.DeviceName[i];
    mi.Tag := i;
    if lAudioInput = i then
    begin
      mi.Checked := True;
      DXAudioIn.DeviceNumber := i;
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
  FTextToSpeechEngines.RegisterEngine(TMicrosoftCognitiveService.Create(Self, ms_cognative_service_resource_key, 'australiaeast.tts.speech.microsoft.com'), miMicrosoftSpeechEngine);
  FTextToSpeechEngines.RegisterEngine(TElevenLabsService.Create(Self, ElevenLabsAPIKey), miElevenLabsSpeechEngine);
  FTextToSpeechEngines.RegisterEngine(TAmazonPollyService.Create(Self, AWSAccessKey, AWSSecretkey), miAmazonSpeechEngine);
  FTextToSpeechEngines.RegisterEngine(TWindowsSpeechService.Create(Self), miWindowsSpeechEngine);
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
  lAssemblyAi := TAssemblyAiRecognition.Create(assemblyai_key);
  lAssemblyAi.OnHandleSpeechRecognitionCompletion := OnHandleSpeechRecognitionCompletion;
  lAssemblyAi.OnConnect := OnHandleConnect;
  lAssemblyAi.OnDisconnect := OnHandleDisconnect;
  FSpeechRecognitionEngines.RegisterEngine(lAssemblyAi, miAssemblyAI);

  lDeepGram := TDeepGramRecognition.Create(deepgram_key);
  lDeepGram.OnHandleSpeechRecognitionCompletion := OnHandleSpeechRecognitionCompletion;
  lDeepGram.OnConnect := OnHandleConnect;
  lDeepGram.OnDisconnect := OnHandleDisconnect;
  FSpeechRecognitionEngines.RegisterEngine(lDeepGram, miDeepGram);

  lSpeechEngine := FSettings.ReadString('Speech', 'SelectedRecognitionEngine', 'TAssemblyAiRecognition');
  FSpeechRecognitionEngines.SelectEngine(lSpeechEngine);
  FSpeechRecognitionEngines.ActiveMenuItem.Checked := True;
end;

procedure TfrmVoiceRecognition.FormCreate(Sender: TObject);
begin
  FConnected := False;
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  FTextToSpeechEngines :=  TEngineManager<TBaseTextToSpeech>.Create;
  FSpeechRecognitionEngines := TEngineManager<TBaseSpeechRecognition>.Create;
  FOpenAI := TOpenAI.Create(chatgpt_apikey);


  tblSessions.Active := True;
  tblConversation.Active := True;

  SetupTextToSpeechEngines;
  SetupSpeechRecognitionEngines;
  LoadAudioInputsMenu;
  DBAdvGrid1.AutoSizeRows(True, 4);
end;

procedure TfrmVoiceRecognition.FormDestroy(Sender: TObject);
begin
  StopListening;
  FreeAndNil(FSettings);
  FreeAndNil(FTextToSpeechEngines);
  FreeAndNil(FSpeechRecognitionEngines);
  FreeAndNil(FOpenAI);
end;

procedure TfrmVoiceRecognition.FormResize(Sender: TObject);
begin
  DBAdvGrid1.ColWidths[0] := 150;
  DBAdvGrid1.ColWidths[1] := DBAdvGrid1.ClientWidth - 150;
end;

procedure TfrmVoiceRecognition.OnHandleConnect(Connection: TObject);
begin
  mmoQuestions.Lines.Add('Connected');
  FConnected := True;
  if FShouldBeListening then
    ShowListening;
end;

procedure TfrmVoiceRecognition.OnHandleDisconnect(Connection: TObject);
begin
  mmoQuestions.Lines.Add('Disconnected');
  FConnected := False;
  if not FShouldBeListening then
    StopListening;
end;

procedure TfrmVoiceRecognition.AsyncSendChatMessagesToOpenAI(ASessionID: Int64; AChatMessages: TObjectList<TChatMessage>; AOnMessageResults: TOnChatMessageMessageResults);
var
  ChatConfig: TChatSettings;
begin
  if AChatMessages.Count = 0 then
  begin
    FreeAndNil(AChatMessages);
    Exit;
  end;
  FStatus := TRecognitionStatus.rsThinking;
  ShowThinking;
  task := TTask.Run(procedure ()
               var
                 ChatResponse: TChatResponse;
               begin
                 ChatResponse := FOpenAI.ChatCompletion(ChatConfig, AChatMessages);
                 if not Assigned(AOnMessageResults) then
                   raise Exception.Create('No Message Results Event given');

                 TThread.Queue(nil, procedure
                   begin
                     AOnMessageResults(ASessionID, ChatResponse);
                   end);
                 FreeAndNil(AChatMessages);
               end);
  task.Start;
end;

procedure TfrmVoiceRecognition.OnHandleChatResponse(SessionID: Int64 ;ChatResponse: TChatResponse);
begin
  tblConversation.Append;
  try
    tblConversation.FieldByName('SessionID').AsLargeInt := SessionID;
    tblConversation.FieldByName('User').AsString := 'Assistant';
    tblConversation.FieldByName('Message').AsString := ChatResponse.Content;
    tblConversation.FieldByName('TokenCount').AsInteger := ChatResponse.Completion_Tokens;
    tblConversation.Post;
    DBAdvGrid1.AutoSizeRows(True, 4);
  except
    on e : Exception do
    begin
      tblConversation.Cancel;
    end;
  end;
  NULLOut.Stop(False);
  FStatus := TRecognitionStatus.rsSpeaking;
  ShowSpeaking;
  FTextToSpeechEngines.ActiveEngine.OnFinishedPlaying := OnFinishedPlaying;
  FTextToSpeechEngines.ActiveEngine.PlayText(ChatResponse.Content);
end;

procedure TfrmVoiceRecognition.OnFinishedPlaying(Sender: TObject);
begin
  // Change back to listening
  StartListening;
end;


procedure TfrmVoiceRecognition.OnHandleSpeechRecognitionCompletion(const Text: string);
var
  question : string;
  ChatMessages: TObjectList<TChatMessage>;
  chat : TChatMessage;
  SessionID : Int64;
begin
  question := Text;
  if Text.IsEmpty then
    Exit;

  StopListening;

  if Text.StartsWith('Stop Listening', true) then
  begin
    Exit;
  end;

  ChatMessages := TObjectList<TChatMessage>.Create(True);
  tblConversation.DisableControls;
  try
    SessionID := tblSessions.FieldByName('SessionID').AsLargeInt;

    tblConversation.Append;
    tblConversation.FieldByName('User').AsString := 'User';
    tblConversation.FieldByName('Message').AsString := question;
    tblConversation.FieldByName('SessionID').AsLargeInt := SessionID;
    tblConversation.Post;
  finally
    tblConversation.EnableControls;
  end;
  DBAdvGrid1.AutoSizeRows(True, 4);
  tblConversation.DisableControls;
  try
    tblConversation.First;
    repeat
      chat := TChatMessage.Create;
      chat.Role := tblConversation.FieldByName('User').AsString;
      chat.Content := tblConversation.FieldByName('Message').AsString;
      ChatMessages.Add(chat);
      tblConversation.Next;
    until tblConversation.Eof;
    FStatus := TRecognitionStatus.rsThinking;
    ShowThinking;
    AsyncSendChatMessagesToOpenAI(SessionID, ChatMessages, OnHandleChatResponse);
  finally
    tblConversation.EnableControls;
  end;
end;


procedure TfrmVoiceRecognition.AudioProcessorGetData(Sender: TComponent; var Buffer: Pointer; var Bytes: Cardinal);
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

procedure TfrmVoiceRecognition.btnDeleteMessageClick(Sender: TObject);
begin
  tblConversation.Delete;
end;

procedure TfrmVoiceRecognition.btnDeleteSessionClick(Sender: TObject);
var
  SessionID : Int64;
  qry : TFDQuery;
begin
  SessionID := tblSessions.FieldByName('SessionID').AsLargeInt;
  FDConnection.StartTransaction;
  try
    qry := TFDQuery.Create(nil);
    try
      qry.Connection := FDConnection;
      qry.SQL.Text := 'DELETE FROM Conversation WHERE (SessionID=:SessionID)';
      qry.ParamByName('SessionID').AsLargeInt := SessionID;
      qry.ExecSQL;
    finally
      FreeAndNil(qry);
    end;

    tblSessions.Delete;
    FDConnection.Commit;
  except
    FDConnection.Rollback;
  end;
end;

procedure TfrmVoiceRecognition.btnNewChatSessionClick(Sender: TObject);
begin
  DBCtrlGrid1.DataSource.DataSet.Append;
  DBCtrlGrid1.DataSource.DataSet.FieldByName('CreationTime').AsDateTime := now;
  DBCtrlGrid1.DataSource.DataSet.Post;
end;

procedure TfrmVoiceRecognition.btnStartClick(Sender: TObject);
var
  SessionID: Int64;
  ChatMessages: TObjectList<TChatMessage>;
  chat : TChatMessage;
begin
  SessionID := tblSessions.FieldByName('SessionID').AsLargeInt;
  if tblConversation.RecordCount = 1 then
  begin
    ChatMessages:= TObjectList<TChatMessage>.Create;
    chat := TChatMessage.Create;
    chat.Role := 'System';
    chat.Content := tblConversation.FieldByName('Message').AsString;
    ChatMessages.Add(chat);
    AsyncSendChatMessagesToOpenAI(SessionID, ChatMessages, OnHandleChatResponse);
  end
  else
  begin
    StartListening;
  end;
end;

procedure TfrmVoiceRecognition.btnStopClick(Sender: TObject);
begin
  StopListening;
end;

procedure TfrmVoiceRecognition.DBCtrlGrid1Click(Sender: TObject);
begin
  DBAdvGrid1.AutoSizeRows(True, 4);
end;

procedure TfrmVoiceRecognition.miExitClick(Sender: TObject);
var
  exitStream: TExitStream;
begin
  exitStream := TExitStream.Create;
  FSpeechRecognitionEngines.ActiveEngine.Add(exitStream);
  Application.Terminate;
end;

procedure TfrmVoiceRecognition.New1Click(Sender: TObject);
begin
  frmNewChatSession.ShowModal;
end;

end.
