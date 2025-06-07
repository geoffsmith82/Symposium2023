unit frmLLMList;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
  System.Threading,
  FMX.Types,
  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  System.ImageList,
  FMX.ImgList,
  FMX.ListView,
  FMX.Menus,
  FMX.frmApiKeyStore,
  ApiKeyStore,
  uLLM,
  uLLM.Anthropic,
  uLLm.Azure,
  uLLm.Google.Gemini,
  uLLM.Groq,
  uLLM.HuggingFace,
  uLLM.OpenAI,
  uLLM.OpenRouter,
  uLLM.Replicate,
  uLLM.Mistral,
  uLLM.X.Ai
  ;

type
  TfrmLLMLister = class(TForm)
    lvLLMs: TListView;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    miExit: TMenuItem;
    miSettings: TMenuItem;
    miGoogleAuthenticate: TMenuItem;
    miAPIKeys: TMenuItem;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miAPIKeysClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miGoogleAuthenticateClick(Sender: TObject);
  private
    { Private declarations }
    FKeyStore: TApiKeyStore;
    FSettings : TIniFile;
    FlblStatus : TLabel;
    FOpenAI : TOpenAI;
    FAzureAI : TMicrosoftOpenAI;
    FGrokAI : TXGrokAI;
    FGeminiAI : TGemini;
    FMistralAI : TMistral;
    FOpenRouterAI: TOpenRouter;
    FClaudeAI : TAnthropic;
    FGroqAI : TGroqLLM;
    procedure LoadModels;
    procedure AddModelsToList(AI: TBaseLLM; ImageIndex: Integer);
  public
    { Public declarations }
  end;

var
  frmLLMLister: TfrmLLMLister;

implementation

{$R *.fmx}

procedure TfrmLLMLister.FormCreate(Sender: TObject);
begin
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  FKeyStore := TApiKeyStore.GetInstance;
  FlblStatus := TLabel.Create(nil);
  FlblStatus.Margins.Left := 10;
  StatusBar.AddObject(FlblStatus);
  TTask.Run(LoadModels);
end;

procedure TfrmLLMLister.LoadModels;
begin
  if not FKeyStore.LoadApiKey('chatgpt_apikey').IsEmpty then
  begin
    FOpenAI := TOpenAI.Create(FKeyStore.LoadApiKey('chatgpt_apikey'));
    AddModelsToList(FOpenAI, 1);
  end;

  var azureKey := FKeyStore.LoadApiKey('AzureAPIKey');
  var azureEndpoint := FKeyStore.LoadSetting('AzureOpenAIEndpoint');
  if (not azureKey.IsEmpty) and (not azureEndpoint.IsEmpty) then
  begin
    FAzureAI := TMicrosoftOpenAI.Create(azureKey, azureEndpoint, '');
    AddModelsToList(FAzureAI, 3);
  end;

  if not FKeyStore.LoadApiKey('X_AI').IsEmpty then
  begin
    FGrokAI := TXGrokAI.Create(FKeyStore.LoadApiKey('X_AI'));
    AddModelsToList(FGrokAI, 7);
  end;

  if not FKeyStore.LoadApiKey('google_AI_APIKey').IsEmpty then
  begin
    FGeminiAI := TGemini.Create(FKeyStore.LoadApiKey('google_AI_APIKey'));
    AddModelsToList(FGeminiAI, 9);
  end;

  if not FKeyStore.LoadApiKey('Claude_APIKey').IsEmpty then
  begin
    FClaudeAI := TAnthropic.Create(FKeyStore.LoadApiKey('Claude_APIKey'));
    AddModelsToList(FClaudeAI, 6);
  end;

  if not FKeyStore.LoadApiKey('groq_apikey').IsEmpty then
  begin
    FGroqAI := TGroqLLM.Create(FKeyStore.LoadApiKey('groq_apikey'));
    AddModelsToList(FGroqAI, 8);
  end;

  if not FKeyStore.LoadApiKey('Mistral_APIKey').IsEmpty then
  begin
    FMistralAI := TMistral.Create(FKeyStore.LoadApiKey('Mistral_APIKey'));
    AddModelsToList(FMistralAI, 10);
  end;

  if not FKeyStore.LoadApiKey('OpenRouter_APIKey').IsEmpty then
  begin
    FOpenRouterAI := TOpenRouter.Create(FKeyStore.LoadApiKey('OpenRouter_APIKey'));
    AddModelsToList(FOpenRouterAI, 11);
  end;
end;
procedure TfrmLLMLister.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FOpenAI);
  FreeAndNil(FAzureAI);
  FreeAndNil(FGrokAI);
  FreeAndNil(FGeminiAI);
  FreeAndNil(FClaudeAI);
  FreeAndNil(FGroqAI);
  FreeAndNil(FMistralAI);
  FreeAndNil(FOpenRouterAI);
  FreeAndNil(FSettings);
end;

procedure TfrmLLMLister.AddModelsToList(AI: TBaseLLM; ImageIndex: Integer);
var
  i: Integer;

  procedure QueueItem(const AModelName, ADetail: string; const AImageIndex: Integer; const AObject: TBaseLLM);
  begin
    TThread.Queue(nil,
      procedure
      var
        item: TListViewItem;
      begin
        item := lvLLMs.Items.Add;
        item.ImageIndex := AImageIndex;
        item.TagObject := AObject;
        item.Text := AModelName;
        item.Detail := ADetail;
        FlblStatus.Text := lvLLMs.ItemCount.ToString + ' Models';
      end);
  end;

begin
  for i := 0 to AI.ModelInfo.Count - 1 do
  begin
    var ModelName := AI.ModelInfo[i].modelName;
    var Version := AI.ModelInfo[i].version;
    var Detail := ModelName + ' ' + Version;

    QueueItem(ModelName, Detail, ImageIndex, AI);
  end;
end;




procedure TfrmLLMLister.miAPIKeysClick(Sender: TObject);
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

procedure TfrmLLMLister.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmLLMLister.miGoogleAuthenticateClick(Sender: TObject);
begin
  if FKeyStore.LoadApiKey('google_refreshtoken').IsEmpty then
  begin
    FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
   // Assert(Assigned(FGoogleSpeech));

//    FGoogleSpeech := TGoogleSpeechService.Create(FKeyStore.LoadApiKey('google_clientid'),  FKeyStore.LoadApiKey('google_clientsecret'), 'ADUG Demo', '', FSettings);
//    FGoogleSpeech.Authenticate;
  end;
end;

end.
