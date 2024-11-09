unit ufrmVoiceRecognition;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ActiveX,
  System.SysUtils,
  System.Math,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.NetEncoding,
  System.IniFiles,
  System.IOUtils,
  System.JSON,
  System.SyncObjs,
  System.Threading,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.BaseImageCollection,
  Vcl.ImageCollection,
  Vcl.VirtualImage,
  Vcl.Grids,
  Vcl.DBCtrls,
  Vcl.DBCGrids,
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
  uLLM,
  uLLM.OpenAI,
  uLLM.Azure,
  uTTS,
  uTTS.Microsoft.Cognitive,
  uTTS.ElevenLabs,
  uTTS.GoogleSpeech,
  uTTS.Amazon.Polly,
  uTTS.Windows.Engine,
  uAssemblyAI.SpeechToText,
  uDeepGram.SpeechToText,
  uRevAI.SpeechToText,
  uBaseSpeechRecognition,
  uEngineManager,
  uAudioRecorder,
  ApiKeyStore,
  frmApiKeyStore,
  BubbleText
  ;

type
  TRecognitionStatus = (rsListening, rsThinking, rsSpeaking, rsStopped);

  TfrmVoiceRecognition = class(TForm)
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
    StatusBar: TStatusBar;
    Panel1: TPanel;
    VirtualImage1: TVirtualImage;
    DBText2: TDBText;
    btnDeleteSession: TButton;
    btnDeleteMessage: TButton;
    Model1: TMenuItem;
    Model2: TMenuItem;
    gpt41: TMenuItem;
    miRevAI: TMenuItem;
    sbMessagesView: TScrollBox;
    miOpenAiTextToSpeech: TMenuItem;
    miSetup: TMenuItem;
    miAPIKeys: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure SelectSpeechEngine(Sender: TObject);
    procedure SelectSpeechRecognitionClick(Sender: TObject);
    procedure btnNewChatSessionClick(Sender: TObject);
    procedure tblSessionsAfterScroll(DataSet: TDataSet);
    procedure New1Click(Sender: TObject);
    procedure btnDeleteSessionClick(Sender: TObject);
    procedure Model2Click(Sender: TObject);
    procedure btnDeleteMessageClick(Sender: TObject);
    procedure sbMessagesViewMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure miAPIKeysClick(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    FConnected : Boolean;
    FShouldBeListening : Boolean;
    FTextToSpeechEngines : TEngineManager<TBaseTextToSpeech>;
    FSpeechRecognitionEngines : TEngineManager<TBaseSpeechRecognition>;
    FTask : ITask;
    FAudio : TAudioRecorder;
    FStatus : TRecognitionStatus;
    FOpenAI : TOpenAI;
    FApiKeyStore : TApiKeyStore;

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
    function AddMessage(const user:string; const msg: string): TBubbleText;
    function LastHeight: Integer;
    procedure OnAudioData(Sender: TObject; Data: TMemoryStream);
    procedure OnSelected(Sender: TObject; Bubble: TBubbleText);
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

uses ufrmChatSession,
  uTTS.OpenAI
  ;

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
  FAudio.Start;
  ShowListening;
end;

procedure TfrmVoiceRecognition.StopListening;
begin
  FShouldBeListening := False;
  FSpeechRecognitionEngines.ActiveEngine.Finish;
  VirtualImage1.ImageIndex := -1;
  FAudio.Stop;
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

procedure TfrmVoiceRecognition.OnSelected(Sender: TObject; Bubble: TBubbleText);
var
  i : Integer;
begin
   for i := 0 to sbMessagesView.ComponentCount - 1 do
   begin
     if sbMessagesView.Components[i] is TBubbleText then
     begin
       if sbMessagesView.Components[i] <> Bubble then
       begin
         (sbMessagesView.Components[i] as TBubbleText).Selected := False;
       end;
     end;
   end;
end;

function TfrmVoiceRecognition.LastHeight: Integer;
var
  i: Integer;
begin
   Result := 0;
   for i := 0 to sbMessagesView.ComponentCount - 1 do
   begin
     if sbMessagesView.Components[i] is TBubbleText then
     begin
       Result := max(Result, (sbMessagesView.Components[i] as TBubbleText).Top + (sbMessagesView.Components[i] as TBubbleText).Height + 10);
     end;
   end;
end;

function TfrmVoiceRecognition.AddMessage(const user:string; const msg: string): TBubbleText;
var
  bubble : TBubbleText;
begin
  bubble := TBubbleText.Create(sbMessagesView);
  bubble.Parent := sbMessagesView;
  bubble.Text := msg;
  bubble.Visible := True;
  if user = 'User' then
    bubble.BubbleType := btUser
  else if user = 'System' then
    bubble.BubbleType := btSystem
  else
    bubble.BubbleType := btOther;
  bubble.Text := msg;
  bubble.Top := LastHeight;
  bubble.Align := alTop;
  bubble.OnSelected := OnSelected;

  Result := bubble;
end;


procedure TfrmVoiceRecognition.tblSessionsAfterScroll(DataSet: TDataSet);
var
  I: Integer;
  b : TBubbleText;
begin
  sbMessagesView.DisableAlign;
  try
    if sbMessagesView.ComponentCount > 0 then
    begin
      for I := sbMessagesView.ComponentCount - 1 downto 0 do
      begin
         b := sbMessagesView.Components[i]  as TBubbleText;
         FreeAndNil(b);
      end;
    end;
    b := nil;
    if not tblConversation.Active then Exit;
    tblConversation.First;
    repeat
      b := AddMessage(tblConversation.FieldByName('User').AsString, tblConversation.FieldByName('Message').AsString);
      b.PrimaryKey := tblConversation.FieldByName('ConversationID').AsLargeInt;
      tblConversation.Next;
    until tblConversation.Eof;
  finally
    sbMessagesView.EnableAlign;
    if Assigned(b) then
    begin
      sbMessagesView.ClientHeight := b.Top + b.Height + 10;
      sbMessagesView.Visible := True;
      sbMessagesView.ScrollInView(b);
    end;
  end;
end;

procedure TfrmVoiceRecognition.LoadAudioInputsMenu;
var
  lAudioInput: Integer;
  i: Integer;
  mi: TMenuItem;
  devices : TAudioDevices;
begin
  miAudioInput.Clear;
  lAudioInput := FSettings.ReadInteger('Audio', 'Input', 0);
  devices := FAudio.GetAvailableDevices;
  for i := 0 to Length(devices) - 1 do
  begin
    mi := TMenuItem.Create(nil);
    mi.Caption := devices[i].DeviceName;
    mi.Tag := i;
    if lAudioInput = i then
    begin
      mi.Checked := True;
      FAudio.SelectDevice(i);
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
  FTextToSpeechEngines.RegisterEngine(TMicrosoftCognitiveService.Create(FApiKeyStore.LoadApiKey('ms_cognative_service_resource_key'), 'australiaeast.tts.speech.microsoft.com'), miMicrosoftSpeechEngine);
  FTextToSpeechEngines.RegisterEngine(TElevenLabsService.Create(FApiKeyStore.LoadApiKey('ElevenLabsAPIKey')), miElevenLabsSpeechEngine);
  FTextToSpeechEngines.RegisterEngine(TAmazonPollyService.Create(FApiKeyStore.LoadApiKey('AWSAccessKey'), FApiKeyStore.LoadApiKey('AWSSecretkey'), FApiKeyStore.LoadSetting('AWSRegion')), miAmazonSpeechEngine);
  FTextToSpeechEngines.RegisterEngine(TWindowsSpeechService.Create, miWindowsSpeechEngine);
  FTextToSpeechEngines.RegisterEngine(TGoogleSpeechService.Create(FApiKeyStore.LoadApiKey('google_clientid'), FApiKeyStore.LoadApiKey('google_clientsecret'), 'ADUG Demo', '', FSettings), miGoogleSpeechEngine);
  FTextToSpeechEngines.RegisterEngine(TOpenAITextToSpeech.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey')), miOpenAiTextToSpeech);


  lSpeechEngine := FSettings.ReadString('Speech', 'SelectedTextToSpeechEngine', 'TWindowsSpeechService');
  FTextToSpeechEngines.SelectEngine(lSpeechEngine);
  FTextToSpeechEngines.ActiveMenuItem.Checked := True;
end;

procedure TfrmVoiceRecognition.SetupSpeechRecognitionEngines;
var
  lSpeechEngine: string;
  lAssemblyAi : TAssemblyAiRecognition;
  lDeepGram : TDeepGramRecognition;
  lRevAi : TRevAiRecognition;
begin
  lRevAi := TRevAiRecognition.Create(FApiKeyStore.LoadApiKey('revai_key'));
  lRevAi.OnHandleSpeechRecognitionCompletion := OnHandleSpeechRecognitionCompletion;
  lRevAi.OnConnect := OnHandleConnect;
  lRevAi.OnDisconnect := OnHandleDisconnect;  
  FSpeechRecognitionEngines.RegisterEngine(lRevAi, miRevAI);

  lAssemblyAi := TAssemblyAiRecognition.Create(FApiKeyStore.LoadApiKey('assemblyai_key'));
  lAssemblyAi.OnHandleSpeechRecognitionCompletion := OnHandleSpeechRecognitionCompletion;
  lAssemblyAi.OnConnect := OnHandleConnect;
  lAssemblyAi.OnDisconnect := OnHandleDisconnect;
  FSpeechRecognitionEngines.RegisterEngine(lAssemblyAi, miAssemblyAI);

  lDeepGram := TDeepGramRecognition.Create(FApiKeyStore.LoadApiKey('deepgram_key'));
  lDeepGram.OnHandleSpeechRecognitionCompletion := OnHandleSpeechRecognitionCompletion;
  lDeepGram.OnConnect := OnHandleConnect;
  lDeepGram.OnDisconnect := OnHandleDisconnect;
  FSpeechRecognitionEngines.RegisterEngine(lDeepGram, miDeepGram);

  lSpeechEngine := FSettings.ReadString('Speech', 'SelectedRecognitionEngine', 'TAssemblyAiRecognition');
  FSpeechRecognitionEngines.SelectEngine(lSpeechEngine);
  FSpeechRecognitionEngines.ActiveMenuItem.Checked := True;
end;

procedure TfrmVoiceRecognition.OnAudioData(Sender: TObject; Data: TMemoryStream);
begin
  Data.Position := 0;
  FSpeechRecognitionEngines.ActiveEngine.Add(Data);
end;

procedure TfrmVoiceRecognition.FormCreate(Sender: TObject);
begin
  FConnected := False;
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  FApiKeyStore := TApiKeyStore.GetInstance;
  FTextToSpeechEngines :=  TEngineManager<TBaseTextToSpeech>.Create;
  FSpeechRecognitionEngines := TEngineManager<TBaseSpeechRecognition>.Create;
  FOpenAI := TOpenAI.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey'));
  FAudio := TAudioRecorder.Create;
  FAudio.OnAudioData := OnAudioData;

  tblSessions.Active := True;
  tblConversation.Active := True;

  SetupTextToSpeechEngines;
  SetupSpeechRecognitionEngines;
  LoadAudioInputsMenu;
  tblSessionsAfterScroll(tblSessions);
end;

procedure TfrmVoiceRecognition.FormDestroy(Sender: TObject);
begin
  StopListening;
  FreeAndNil(FAudio);
  FreeAndNil(FSettings);
  FreeAndNil(FTextToSpeechEngines);
  FreeAndNil(FSpeechRecognitionEngines);
  FreeAndNil(FOpenAI);
end;

procedure TfrmVoiceRecognition.OnHandleConnect(Connection: TObject);
begin
  FConnected := True;
  if FShouldBeListening then
    ShowListening;
end;

procedure TfrmVoiceRecognition.OnHandleDisconnect(Connection: TObject);
begin
  FConnected := False;
end;

procedure TfrmVoiceRecognition.miAPIKeysClick(Sender: TObject);
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
  FTask := TTask.Run(procedure ()
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
end;

procedure TfrmVoiceRecognition.OnHandleChatResponse(SessionID: Int64 ;ChatResponse: TChatResponse);
var
  bubble : TBubbleText;
  KeyId : Int64;
begin
  KeyId := -1;
  tblConversation.Append;
  try
    tblConversation.FieldByName('SessionID').AsLargeInt := SessionID;
    tblConversation.FieldByName('User').AsString := 'Assistant';
    tblConversation.FieldByName('Message').AsString := ChatResponse.Content;
    tblConversation.FieldByName('TokenCount').AsInteger := ChatResponse.Completion_Tokens;
    tblConversation.Post;
    KeyId := tblConversation.FieldByName('ConversationID').AsLargeInt;
  except
    on e : Exception do
    begin
      tblConversation.Cancel;
    end;
  end;
  StopListening;
  FStatus := TRecognitionStatus.rsSpeaking;
  bubble := AddMessage('Assistant', ChatResponse.Content);
  bubble.PrimaryKey := KeyId;
  sbMessagesView.ScrollInView(bubble);
  ShowSpeaking;
  FTextToSpeechEngines.ActiveEngine.OnFinishedPlaying := OnFinishedPlaying;
  FTextToSpeechEngines.ActiveEngine.PlayText(ChatResponse.Content, 'Olivia');
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
  KeyID: Int64;
  bubble : TBubbleText;
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
    KeyId := tblConversation.FieldByName('ConversationID').AsLargeInt;

  finally
    tblConversation.EnableControls;
  end;
  bubble := AddMessage('User', question);
  bubble.PrimaryKey := KeyId;

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

procedure TfrmVoiceRecognition.miExitClick(Sender: TObject);
var
  exitStream: TExitStream;
begin
  exitStream := TExitStream.Create;
  FSpeechRecognitionEngines.ActiveEngine.Add(exitStream);
  Application.Terminate;
end;

procedure TfrmVoiceRecognition.Model2Click(Sender: TObject);
begin
  FSettings.WriteString('Settings', 'Model', (Sender as TMenuItem).Caption);
end;

procedure TfrmVoiceRecognition.New1Click(Sender: TObject);
begin
  frmNewChatSession.ShowModal;
  tblSessionsAfterScroll(tblSessions);
end;

procedure TfrmVoiceRecognition.sbMessagesViewMouseWheel(Sender: TObject; Shift:
    TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    sbMessagesView.VertScrollBar.Position := sbMessagesView.VertScrollBar.Position - 60
  else
    sbMessagesView.VertScrollBar.Position := sbMessagesView.VertScrollBar.Position + 60;

  Handled := True;
end;

end.
