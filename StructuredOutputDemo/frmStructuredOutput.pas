unit frmStructuredOutput;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Menus,
  Vcl.ExtCtrls,
  ApiKeyStore,
  frmApiKeyStore,
  uLLM,
  uLLM.OpenAI,
  uLLM.Anthropic,
  uLLM.Google.Gemini,
  uLLM.DeepSeek,
  uLLM.Groq,
  uLLM.Mistral,
  uLLM.X.Ai,
  uLLM.OpenRouter,
  uSchemaModels;

type
  TProviderInfo = record
    Name: string;
    KeyName: string;
    DefaultModel: string;
  end;

  TfrmStructuredOutputDemo = class(TForm)
    pnlLeft: TPanel;
    pnlRight: TPanel;
    splMain: TSplitter;
    grpProvider: TGroupBox;
    cboProvider: TComboBox;
    lblProvider: TLabel;
    grpSchema: TGroupBox;
    cboSchema: TComboBox;
    lblSchema: TLabel;
    grpPrompt: TGroupBox;
    mmoPrompt: TMemo;
    btnExecute: TButton;
    grpResponse: TGroupBox;
    mmoResponse: TMemo;
    grpLog: TGroupBox;
    mmoLog: TMemo;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miExit: TMenuItem;
    miSettings: TMenuItem;
    miAPIKeys: TMenuItem;
    lblStatus: TLabel;
    lblTokens: TLabel;
    btnViewSchema: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnViewSchemaClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miAPIKeysClick(Sender: TObject);
    procedure cboSchemaChange(Sender: TObject);
  private
    FApiKeyStore: TApiKeyStore;
    FProviders: TArray<TProviderInfo>;
    function CreateLLM(ProviderIndex: Integer): TBaseLLM;
    function GetSchemaClass: TClass;
    function GetSchemaName: string;
    function GetDefaultPrompt: string;
    procedure FormatJSONResponse(const JSONStr: string);
  end;

var
  frmStructuredOutputDemo: TfrmStructuredOutputDemo;

implementation

{$R *.dfm}

uses
  PasDantic.SchemaGenerator;

procedure TfrmStructuredOutputDemo.FormCreate(Sender: TObject);
begin
  FApiKeyStore := TApiKeyStore.GetInstance;

  SetLength(FProviders, 8);

  FProviders[0].Name := 'OpenAI';
  FProviders[0].KeyName := 'chatgpt_apikey';
  FProviders[0].DefaultModel := 'gpt-4o-mini';

  FProviders[1].Name := 'Anthropic (Fallback)';
  FProviders[1].KeyName := 'Claude_APIKey';
  FProviders[1].DefaultModel := 'claude-sonnet-4-20250514';

  FProviders[2].Name := 'Google Gemini (Fallback)';
  FProviders[2].KeyName := 'google_AI_APIKey';
  FProviders[2].DefaultModel := 'gemini-2.0-flash';

  FProviders[3].Name := 'DeepSeek';
  FProviders[3].KeyName := 'Deepseek_Key';
  FProviders[3].DefaultModel := 'deepseek-chat';

  FProviders[4].Name := 'Groq';
  FProviders[4].KeyName := 'groq_apikey';
  FProviders[4].DefaultModel := 'llama-3.3-70b-versatile';

  FProviders[5].Name := 'Mistral';
  FProviders[5].KeyName := 'Mistral_APIKey';
  FProviders[5].DefaultModel := 'mistral-small-latest';

  FProviders[6].Name := 'X.AI (Grok)';
  FProviders[6].KeyName := 'X_AI';
  FProviders[6].DefaultModel := 'grok-3-mini-fast';

  FProviders[7].Name := 'OpenRouter';
  FProviders[7].KeyName := 'OpenRouter_APIKey';
  FProviders[7].DefaultModel := 'openai/gpt-4o-mini';

  cboProvider.Items.Clear;
  for var P in FProviders do
    cboProvider.Items.Add(P.Name);
  cboProvider.ItemIndex := 0;

  cboSchema.Items.Clear;
  cboSchema.Items.Add('Recipe');
  cboSchema.Items.Add('Movie Review');
  cboSchema.Items.Add('Contact Info');
  cboSchema.ItemIndex := 0;

  cboSchemaChange(nil);
end;

procedure TfrmStructuredOutputDemo.FormDestroy(Sender: TObject);
begin
  // FApiKeyStore is singleton, not owned by us
end;

function TfrmStructuredOutputDemo.CreateLLM(ProviderIndex: Integer): TBaseLLM;
var
  Key: string;
begin
  Key := FApiKeyStore.LoadApiKey(FProviders[ProviderIndex].KeyName);
  if Key.IsEmpty then
    raise ELLMException.Create('API key not configured for ' + FProviders[ProviderIndex].Name +
      '. Go to Settings > API Keys to add it.');

  case ProviderIndex of
    0: Result := TOpenAI.Create(Key);
    1: Result := TAnthropic.Create(Key);
    2: Result := TGemini.Create(Key);
    3: Result := TDeepSeek.Create(Key);
    4: Result := TGroqLLM.Create(Key);
    5: Result := TMistral.Create(Key);
    6: Result := TXGrokAI.Create(Key);
    7: Result := TOpenRouter.Create(Key);
  else
    raise ELLMException.Create('Unknown provider');
  end;

  // Wire up logging - TOnLog is a reference to procedure, so use anonymous method
  Result.OnLog :=
    procedure(inLog: string)
    begin
      mmoLog.Lines.Add(inLog);
    end;
end;

function TfrmStructuredOutputDemo.GetSchemaClass: TClass;
begin
  case cboSchema.ItemIndex of
    0: Result := TRecipe;
    1: Result := TMovieReview;
    2: Result := TContactInfo;
  else
    Result := TRecipe;
  end;
end;

function TfrmStructuredOutputDemo.GetSchemaName: string;
begin
  case cboSchema.ItemIndex of
    0: Result := 'recipe';
    1: Result := 'movie_review';
    2: Result := 'contact_info';
  else
    Result := 'response';
  end;
end;

function TfrmStructuredOutputDemo.GetDefaultPrompt: string;
begin
  case cboSchema.ItemIndex of
    0: Result := 'Give me a recipe for a classic Italian carbonara pasta.';
    1: Result := 'Write a review of the movie "The Shawshank Redemption" (1994).';
    2: Result := 'Extract the contact information from this text:' + sLineBreak + sLineBreak +
       'Hi, my name is Sarah Johnson. I work as a Senior Developer at TechCorp Industries. ' +
       'You can reach me at sarah.johnson@techcorp.com or call me at +61 412 345 678.';
  else
    Result := '';
  end;
end;

procedure TfrmStructuredOutputDemo.cboSchemaChange(Sender: TObject);
begin
  mmoPrompt.Text := GetDefaultPrompt;
end;

procedure TfrmStructuredOutputDemo.btnExecuteClick(Sender: TObject);
var
  LLM: TBaseLLM;
  Messages: TObjectList<TChatMessage>;
  Config: TChatSettings;
  Response: TChatResponse;
  SystemMsg, UserMsg: TChatMessage;
  ProviderIdx: Integer;
  SchemaClass: TClass;
begin
  btnExecute.Enabled := False;
  mmoResponse.Clear;
  mmoLog.Clear;
  lblStatus.Caption := 'Sending request...';
  lblTokens.Caption := '';
  Application.ProcessMessages;

  ProviderIdx := cboProvider.ItemIndex;
  SchemaClass := GetSchemaClass;

  LLM := CreateLLM(ProviderIdx);
  try
    Messages := TObjectList<TChatMessage>.Create;
    try
      // System message
      SystemMsg := TChatMessage.Create;
      SystemMsg.Role := 'system';
      SystemMsg.Content := 'You are a helpful assistant that provides structured data responses.';

      // For providers without native structured output, inject schema into prompt
      if not LLM.SupportsStructuredOutput then
      begin
        SystemMsg.Content := SystemMsg.Content + sLineBreak + sLineBreak +
          TBaseLLM.SchemaFallbackPrompt(SchemaClass);
        mmoLog.Lines.Add('Provider does not support native structured output.');
        mmoLog.Lines.Add('Using schema fallback prompt in system message.');
        mmoLog.Lines.Add('');
      end;

      Messages.Add(SystemMsg);

      // User message
      UserMsg := TChatMessage.Create;
      UserMsg.Role := 'user';
      UserMsg.Content := mmoPrompt.Text;
      Messages.Add(UserMsg);

      // Configure chat settings
      Config.model := FProviders[ProviderIdx].DefaultModel;
      Config.max_tokens := 2000;

      if LLM.SupportsStructuredOutput then
      begin
        // Native structured output - set the schema directly
        Config.ResponseSchema := SchemaClass;
        Config.ResponseSchemaName := GetSchemaName;
        mmoLog.Lines.Add('Using native structured output (response_format: json_schema)');
      end
      else
      begin
        // Fallback - use json_mode where available
        Config.json_mode := True;
        mmoLog.Lines.Add('Using fallback: json_mode + schema in system prompt');
      end;

      mmoLog.Lines.Add('Model: ' + Config.model);
      mmoLog.Lines.Add('Schema: ' + GetSchemaName);
      mmoLog.Lines.Add('');

      lblStatus.Caption := 'Waiting for ' + FProviders[ProviderIdx].Name + '...';
      Application.ProcessMessages;

      // Execute the chat completion
      Response := LLM.ChatCompletion(Config, Messages);

      // Display response
      FormatJSONResponse(Response.Content);
      lblStatus.Caption := 'Complete (' + Response.Model + ')';
      lblTokens.Caption := Format('Tokens - Prompt: %d  Completion: %d  Total: %d',
        [Response.Prompt_Tokens, Response.Completion_Tokens, Response.Total_Tokens]);

    finally
      FreeAndNil(Messages);
    end;
  finally
    FreeAndNil(LLM);
    btnExecute.Enabled := True;
  end;
end;

procedure TfrmStructuredOutputDemo.FormatJSONResponse(const JSONStr: string);
var
  JSONValue: TJSONValue;
begin
  JSONValue := TJSONObject.ParseJSONValue(JSONStr);
  if Assigned(JSONValue) then
  try
    mmoResponse.Lines.Text := JSONValue.Format;
  finally
    JSONValue.Free;
  end
  else
    mmoResponse.Lines.Text := JSONStr;
end;

procedure TfrmStructuredOutputDemo.btnViewSchemaClick(Sender: TObject);
var
  Schema: TJSONObject;
begin
  Schema := GenerateJSONSchema(GetSchemaClass);
  try
    mmoResponse.Clear;
    mmoResponse.Lines.Add('JSON Schema for: ' + GetSchemaName);
    mmoResponse.Lines.Add('');
    mmoResponse.Lines.Add(Schema.Format);
  finally
    Schema.Free;
  end;
end;

procedure TfrmStructuredOutputDemo.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmStructuredOutputDemo.miAPIKeysClick(Sender: TObject);
var
  frm: TfrmApiKeyStores;
begin
  frm := TfrmApiKeyStores.Create(nil);
  try
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

end.
