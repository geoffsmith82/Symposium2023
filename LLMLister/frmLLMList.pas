unit frmLLMList;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  System.ImageList,
  FMX.ImgList,
  FMX.ListView,
  FMX.Menus,
  ApiKeyStore.Windows,
  FMX.frmApiKeyStore,
  ApiKeyStore,
  uLLM,
  uLLM.Anthropic,
  uLLm.Azure,
  uLLm.Google.Gemini,
  uLLM.Groq,
  uLLM.HuggingFace,
  uLLM.OpenAI,
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
    procedure FormCreate(Sender: TObject);
    procedure miAPIKeysClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miGoogleAuthenticateClick(Sender: TObject);
  private
    { Private declarations }
    FKeyStore: TApiKeyStore;
    FSettings : TIniFile;
    FOpenAI : TOpenAI;
    FAzureAI : TMicrosoftOpenAI;
    FGrokAI : TXGrokAI;
    FGeminiAI : TGemini;
    FMistralAI : TMistral;
    FClaudeAI : TAnthropic;
    FGroqAI : TGroqLLM;
  public
    { Public declarations }
  end;

var
  frmLLMLister: TfrmLLMLister;

implementation

{$R *.fmx}

procedure TfrmLLMLister.FormCreate(Sender: TObject);
var
  i : Integer;
  item : TListViewItem;
begin
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  FKeyStore := TApiKeyStore.GetInstance;
  if not FKeyStore.LoadApiKey('chatgpt_apikey').IsEmpty then
  begin
    FOpenAI := TOpenAI.Create(FKeyStore.LoadApiKey('chatgpt_apikey'));
    for i := 0 to FOpenAI.ModelInfo.Count - 1 do
    begin
      item := lvLLMs.Items.Add;
      item.ImageIndex := 1;
      item.TagObject := FOpenAI;
      item.Text := FOpenAI.ModelInfo[i].modelName;
      item.Detail := FOpenAI.ModelInfo[i].modelName + ' ' + FOpenAI.ModelInfo[i].version;
    end;
  end;

  if not FKeyStore.LoadApiKey('AzureAPIKey').IsEmpty then
  begin
    FAzureAI := TMicrosoftOpenAI.Create(FKeyStore.LoadApiKey('AzureAPIKey'), FKeyStore.LoadSetting('AzureOpenAIEndpoint'), '');
    for i := 0 to FAzureAI.ModelInfo.Count - 1 do
    begin
      item := lvLLMs.Items.Add;
      item.ImageIndex := 3;
      item.TagObject := FAzureAI;
      item.Text := FAzureAI.ModelInfo[i].modelName;
      item.Detail := FAzureAI.ModelInfo[i].modelName + ' ' + FAzureAI.ModelInfo[i].version;
    end;
  end;

  if not FKeyStore.LoadApiKey('X_AI').IsEmpty then
  begin
    FGrokAI := TXGrokAI.Create(FKeyStore.LoadApiKey('X_AI'));
    for i := 0 to FGrokAI.ModelInfo.Count - 1 do
    begin
      item := lvLLMs.Items.Add;
      item.ImageIndex := 7;
      item.TagObject := FGrokAI;
      item.Text := FGrokAI.ModelInfo[i].modelName;
      item.Detail := FGrokAI.ModelInfo[i].modelName + ' ' + FGrokAI.ModelInfo[i].version;
    end;
  end;

  if not FKeyStore.LoadApiKey('google_AI_APIKey').IsEmpty then
  begin
    FGeminiAI := TGemini.Create(FKeyStore.LoadApiKey('google_AI_APIKey'));
    for i := 0 to FGeminiAI.ModelInfo.Count - 1 do
    begin
      item := lvLLMs.Items.Add;
      item.ImageIndex := 9;
      item.TagObject := FGeminiAI;
      item.Text := FGeminiAI.ModelInfo[i].modelName;
      item.Detail := FGeminiAI.ModelInfo[i].modelName + ' ' + FGeminiAI.ModelInfo[i].version;
    end;
  end;

  if not FKeyStore.LoadApiKey('Claude_APIKey').IsEmpty then
  begin
    FClaudeAI := TAnthropic.Create(FKeyStore.LoadApiKey('Claude_APIKey'));
    for i := 0 to FClaudeAI.ModelInfo.Count - 1 do
    begin
      item := lvLLMs.Items.Add;
      item.ImageIndex := 6;
      item.TagObject := FClaudeAI;
      item.Text := FClaudeAI.ModelInfo[i].modelName;
      item.Detail := FClaudeAI.ModelInfo[i].modelName + ' ' + FClaudeAI.ModelInfo[i].version;
    end;
  end;

  if not FKeyStore.LoadApiKey('groq_apikey').IsEmpty then
  begin
    FGroqAI := TGroqLLM.Create(FKeyStore.LoadApiKey('groq_apikey'));
    for i := 0 to FGroqAI.ModelInfo.Count - 1 do
    begin
      item := lvLLMs.Items.Add;
      item.ImageIndex := 8;
      item.TagObject := FGroqAI;
      item.Text := FGroqAI.ModelInfo[i].modelName;
      item.Detail := FGroqAI.ModelInfo[i].modelName + ' ' + FGroqAI.ModelInfo[i].version;
    end;
  end;

  if not FKeyStore.LoadApiKey('Mistral_APIKey').IsEmpty then
  begin
    FMistralAI := TMistral.Create(FKeyStore.LoadApiKey('Mistral_APIKey'));
    for i := 0 to FMistralAI.ModelInfo.Count - 1 do
    begin
      item := lvLLMs.Items.Add;
      item.ImageIndex := 10;
      item.TagObject := FMistralAI;
      item.Text := FMistralAI.ModelInfo[i].modelName;
      item.Detail := FMistralAI.ModelInfo[i].modelName + ' ' + FMistralAI.ModelInfo[i].version;
    end;
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
