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
  uEngineManager,
  AdvUtil,
  AdvObj,
  BaseGrid,
  AdvGrid,
  DBAdvGrid
  ;

type
  TfrmVoiceRecognition = class(TForm)
    DXAudioIn1: TDXAudioIn;
    AudioProcessor1: TAudioProcessor;
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
    ImageCollection1: TImageCollection;
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
    procedure FormResize(Sender: TObject);
    procedure tblSessionsAfterScroll(DataSet: TDataSet);
    procedure DBCtrlGrid1Click(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    FConnected : Boolean;
    FTextToSpeechEngines : TEngineManager<TBaseTextToSpeech>;
    FSpeechRecognitionEngines : TEngineManager<TBaseSpeechRecognition>;

    procedure LoadAudioInputsMenu;

    procedure OnHandleSpeechRecognitionCompletion(const Text: string);
    procedure OnHandleConnect(Connection: TObject);
    procedure OnHandleDisconnect(Connection: TObject);
    procedure SetupTextToSpeechEngines;
    procedure SetupSpeechRecognitionEngines;
  public
    { Public declarations }
    procedure ShowListening;
    procedure ShowSpeaking;
    procedure StopListening;
    procedure StartListening;
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

procedure TfrmVoiceRecognition.ShowSpeaking;
begin
  VirtualImage1.ImageIndex := 1;
  VirtualImage1.Update;
end;

procedure TfrmVoiceRecognition.StartListening;
begin
  FSpeechRecognitionEngines.ActiveEngine.Resume;
  NULLOut.Run;
  ShowListening;
  UserInterfaceUpdateTimer.Enabled := True;
end;

procedure TfrmVoiceRecognition.StopListening;
begin
  FSpeechRecognitionEngines.ActiveEngine.Finish;
  VirtualImage1.ImageIndex := -1;
  NULLOut.Stop(False);
  UserInterfaceUpdateTimer.Enabled := False;
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
  lAssemblyAi.OnHandleSpeechRecognitionCompletion := OnHandleSpeechRecognitionCompletion;
  lAssemblyAi.OnConnect := OnHandleConnect;
  lAssemblyAi.OnDisconnect := OnHandleDisconnect;
  FSpeechRecognitionEngines.RegisterEngine(lAssemblyAi, miAssemblyAI);

  lDeepGram := TDeepGramRecognition.Create(deepgram_key,'','');
  lDeepGram.OnHandleSpeechRecognitionCompletion := OnHandleSpeechRecognitionCompletion;
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
end;

procedure TfrmVoiceRecognition.FormResize(Sender: TObject);
begin
  DBAdvGrid1.ColWidths[0] := 150;
  DBAdvGrid1.ColWidths[1] := DBAdvGrid1.ClientWidth - 150;
end;

procedure TfrmVoiceRecognition.UserInterfaceUpdateTimerTimer(Sender: TObject);
begin
 // OutputDebugString(PChar(MediaPlayer1.EndPos.ToString + ' ' + MediaPlayer1.Position.ToString));
  if FTextToSpeechEngines.ActiveEngine.Mode = mpStopped then
  begin
    if NULLOut.Status <> tosPlaying then
      StartListening;
  end;
  if FTextToSpeechEngines.ActiveEngine.Mode = mpPlaying then
  begin

  end
  else
  begin
    ShowListening;
  end;
end;

procedure TfrmVoiceRecognition.OnHandleConnect(Connection: TObject);
begin
  mmoQuestions.Lines.Add('Connected');
  FConnected := True;
end;

procedure TfrmVoiceRecognition.OnHandleDisconnect(Connection: TObject);
begin
  mmoQuestions.Lines.Add('Disconnected');
  FConnected := False;
end;

procedure TfrmVoiceRecognition.OnHandleSpeechRecognitionCompletion(const Text: string);
var
  question : string;
  ChatMessages: TObjectList<TChatMessage>;
  ChatResponse: TChatResponse;
  chat : TChatMessage;
  SessionID : Int64;
  embedding: TArray<string>;
begin
   question := Text;
   mmoQuestions.Lines.Add(question);
//   response := TOpenAI.AskChatGPT(Text, 'text-davinci-003');
   ChatMessages := TObjectList<TChatMessage>.Create(True);
   try
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

       ChatResponse := TOpenAI.SendChatMessagesToOpenAI(CHATGPT_APIKEY, ChatMessages);
       tblConversation.Append;
       try
         tblConversation.FieldByName('SessionID').AsLargeInt := SessionID;
         tblConversation.FieldByName('User').AsString := 'Assistant';
         tblConversation.FieldByName('Message').AsString := ChatResponse.Content;
         setlength(embedding, 2);
         embedding[0] := question;
         embedding[1] := ChatResponse.Content;
         TOpenAI.Embeddings(embedding);

         tblConversation.FieldByName('TokenCount').AsInteger := ChatResponse.Completion_Tokens;
         tblConversation.Post;
       except
         on e : Exception do
         begin

         end;
       end;
     finally
       tblConversation.EnableControls;
     end;
   finally
     FreeAndNil(ChatMessages);
   end;
   mmoAnswers.Lines.Text := ChatResponse.Content;
   mmoAnswers.Update;
   NULLOut.Stop(False);
   ShowSpeaking;
   FTextToSpeechEngines.ActiveEngine.PlayText(ChatResponse.Content);
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
  StartListening;
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
begin
  Application.Terminate;
end;

end.
