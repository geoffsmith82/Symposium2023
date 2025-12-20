unit frmTestAPI;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Generics.Collections,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Menus,
  IniFiles,
  uAttributes,
  uTTS.GoogleSpeech,
  uLLM.OpenAI.Assistants,
  uTTS.Coqui,
  ApiKeyStore
  ;

type
  TTestProcedure = record
    Method: TRttiMethod;
    Instance: TObject;
  end;

  TfrmTestApiWindow = class(TForm)
    Memo1: TMemo;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    TestMenuItem: TMenuItem;
    miSettings: TMenuItem;
    miAPIKeys: TMenuItem;
    miGoogleAuthenticate: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miGoogleAuthenticateClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure miAPIKeysClick(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    FApiKeyStore: TApiKeyStore;
    Fgooglespeech : TGoogleSpeechService;
    FProcedures : TList<TTestProcedure>;
    procedure MenuItemClick(Sender: TObject);
    procedure FindProcedures;
  public
    { Public declarations }
    procedure ListOpenAIModels;
    procedure ListDeekSeekModels;
    procedure ListMistralAIModels;
    procedure ListReplicateImageGenerators;
    procedure ListReplicateLLM;
    procedure ListAzureLLMModels;
    procedure ListGoogleLLMModels;
    procedure TestGoogleLLM;
    procedure ListAmazonVoices;
    procedure ListAnthropicModels;
    procedure ListElevenLabsVoices;
    procedure ListWindowsVoices;
    procedure ListMicrosoftVoices;
    procedure ListAmazonTranslationLanguages;
    procedure ListMicrosoftTranslateLanguages;
    procedure ListGoogleVoices;
    procedure ListOpenAIVoices;

    procedure TestOpenAITTS;
    procedure TestElevenLabsTTS;
    procedure TestConquiVoices;
    procedure TestAmazonPolly;
    procedure TestWindowsVoice;
    procedure TestMicrosoftVoices;
    procedure TestGoogleVoices;

    procedure TestOpenAIVision;
    procedure TestDeepSeekVision;
    procedure TestGoogleAIVision;
    procedure TestAnthropicClaudeVision;
    procedure TestAzureVision;
    procedure TestXaiGrokVision;
    procedure TestMistralVision;

    procedure TestHunggingFaceLLM;
    procedure TestGroqLLM;
    procedure TestGroqVisionLLM;
    procedure ListGroqModels;
    procedure TestGrokLLM;
    procedure TestMistralLLM;

    procedure TestOpenAIFunctionCalling;
    procedure TestDeepSeekFunctionCalling;
    procedure TestAzureFunctionCalling;
    procedure TestGroqFunctionCalling;
    procedure TestGoogleGeminiFunctionCalling;
    procedure TestAnthropicClaudeFunctionCalling;
    procedure TestXaiGrokFunctionCalling;
    procedure TestHuggingFaceFunctionCalling;
    procedure TestMistralFunctionCalling;


    [FunctionDescription('Get the weather forecast for a place in Australia')]
    function GetWeather([ParamDescription('State of the location')]const state: string; [ParamDescription('Location for the weather forecast')]const location: string): string;
    [FunctionDescription('Get the time')]
    function GetTimeAt([ParamDescription('State of the location')]const state: string; [ParamDescription('Location for the time')]const location: string): string;

  end;

var
  frmTestApiWindow: TfrmTestApiWindow;

const AzureOpenAIEndpoint = 'https://symposiumdemoopenai.openai.azure.com';

implementation

{$R *.dfm}

uses
  uTTS,
  uTTS.ElevenLabs,
  uTTS.Amazon.Polly,
  uTTS.OpenAI,
  uTTS.Windows.Engine,
  uTTS.Microsoft.Cognitive,
  uTranslate,
  uTranslate.LanguageCodes,
  uTranslate.Microsoft,
  uTranslate.Amazon,
  uLLM,
  uLLM.OpenAI,
  uLLM.Azure,
  uLLM.Google.Gemini,
  uLLM.Anthropic,
  uLLM.HuggingFace,
  uLLM.Replicate,
  uLLM.Mistral,
  uLLM.Groq,
  uLLM.X.Ai,
  uLLM.DeepSeek,
  uDALLe2.DTO,
  uImageGeneration,
  uImageGeneration.Replicate,
  frmApiKeyStore
  ;

procedure TfrmTestApiWindow.FindProcedures;
var
  ctx: TRttiContext;
  typ: TRttiType;
  method: TRttiMethod;
  procedureRec: TTestProcedure;
  menuItem: TMenuItem;
begin
  ctx := TRttiContext.Create;
  try
    typ := ctx.GetType(Self.ClassType);

    for method in typ.GetMethods do
    begin
      if (method.Visibility = mvPublic) and
         (Length(method.GetParameters) = 0) and
         (method.Parent = typ) and
         (method.Name <> 'FindProcedures') and
         (method.Name <> 'MenuItemClick') then
      begin
        procedureRec.Method := method;
        procedureRec.Instance := Self;

        FProcedures.Add(procedureRec);

        menuItem := TMenuItem.Create(Self);
        menuItem.Caption := method.Name;
        menuItem.Tag := FProcedures.Count - 1;
        menuItem.OnClick := MenuItemClick;
        TestMenuItem.Add(menuItem);
      end;
    end;
  finally
    ctx.Free;
  end;
end;

procedure TfrmTestApiWindow.TestAnthropicClaudeVision;
var
  anthropic: TAnthropic;
  Local_modelObj: TBaseModelInfo;
  settings: TChatSettings;
  messages: System.Generics.Collections.TObjectList<TChatMessage>;
  msg: TChatVisionMessage;
  answer: string;
begin
  Memo1.Lines.Add('======== Model Anthropic');
  anthropic := TAnthropic.Create(FApiKeyStore.LoadApiKey('Claude_APIKey'));
  try
    for Local_modelObj in anthropic.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + Local_modelObj.modelName);
    end;
    settings.json_mode := False;
    settings.model := 'claude-3-7-sonnet-20250219';
    settings.max_tokens := 1024;
    messages := TObjectList<TChatMessage>.Create;
    msg := TAnthropic.CreateChatVisionMessage;
    msg.Role := 'user';
    msg.AddImageFile('C:\Users\geoff\Pictures\Chickens  035.jpg', 'image/jpeg');
    msg.Content := 'Describe the following image. It does not contain popcorn. If it is not popcorn what can it be?';
    messages.Add(msg);
    answer := anthropic.ChatCompletion(settings, messages).Content;
    Memo1.Lines.Add('Answer: ' + answer);
  finally
    FreeAndNil(anthropic);
    FreeAndNil(messages);
  end;
end;

procedure TfrmTestApiWindow.TestAzureVision;
var
  azureOpenAI: TMicrosoftOpenAI;
  Local_modelObj: TBaseModelInfo;
  settings: TChatSettings;
  messages: System.Generics.Collections.TObjectList<TChatMessage>;
  msg: TChatVisionMessage;
  answer: string;
begin
  Memo1.Lines.Add('======== Model Azure');
  azureOpenAI := TMicrosoftOpenAI.Create(FApiKeyStore.LoadApiKey('AzureAPIKey'), AzureOpenAIEndpoint, 'gpt-4o');
  try
    for Local_modelObj in azureOpenAI.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + Local_modelObj.modelName);
    end;
    settings.json_mode := False;
    settings.model := 'gpt-4o';
    settings.max_tokens := 1024;
    messages := TObjectList<TChatMessage>.Create;
    msg := TChatVisionMessage.Create;
    msg.Role := 'user';
    msg.AddImageFile('C:\Users\geoff\Pictures\Chickens  035.jpg', 'image/jpeg');
    msg.Content := 'Describe the following image';
    messages.Add(msg);
    answer := azureOpenAI.ChatCompletion(settings, messages).Content;
    Memo1.Lines.Add('Answer: ' + answer);
  finally
    FreeAndNil(azureOpenAI);
    FreeAndNil(messages);
  end;
end;


procedure TfrmTestApiWindow.TestXaiGrokVision;
var
  grokAI: TXGrokAI;
  Local_modelObj: TBaseModelInfo;
  settings: TChatSettings;
  messages: System.Generics.Collections.TObjectList<TChatMessage>;
  msg: TChatVisionMessage;
  answer: string;
begin
  Memo1.Lines.Add('======== Model X.AI');
  grokAI := TXGrokAI.Create(FApiKeyStore.LoadApiKey('X_AI'));
  try
    for Local_modelObj in grokAI.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + Local_modelObj.modelName);
    end;
    settings.json_mode := False;
    settings.model := 'grok-vision-beta';
    settings.max_tokens := 1024;
    messages := TObjectList<TChatMessage>.Create;
    msg := TChatVisionMessage.Create;
    msg.Role := 'user';
    msg.AddImageFile('C:\Users\geoff\Pictures\Chickens  035.jpg', 'image/jpeg');
    msg.Content := 'Describe the following image';
    messages.Add(msg);
    answer := grokAI.ChatCompletion(settings, messages).Content;
    Memo1.Lines.Add('Answer: ' + answer);
  finally
    FreeAndNil(grokAI);
    FreeAndNil(messages);
  end;
end;

procedure TfrmTestApiWindow.TestMistralVision;
var
  mistralLLM: TMistral;
  Local_modelObj: TBaseModelInfo;
  settings: TChatSettings;
  messages: System.Generics.Collections.TObjectList<TChatMessage>;
  msg: TChatVisionMessage;
  answer: string;
begin
  Memo1.Lines.Add('======== Model Mistral Vision');
  mistralLLM := TMistral.Create(FApiKeyStore.LoadApiKey('Mistral_APIKey'));
  try
    for Local_modelObj in mistralLLM.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + Local_modelObj.modelName);
    end;
    settings.json_mode := False;
    settings.model := 'pixtral-12b-2409';
    settings.max_tokens := 1024;
    messages := TObjectList<TChatMessage>.Create;
    msg := TChatVisionMessage.Create;
    msg.Role := 'user';
    msg.AddImageFile('C:\Users\geoff\Pictures\Chickens  035.jpg', 'image/jpeg');
    msg.Content := 'Describe the following image';
    messages.Add(msg);
    answer := mistralLLM.ChatCompletion(settings, messages).Content;
    Memo1.Lines.Add('Answer: ' + answer);
  finally
    FreeAndNil(mistralLLM);
    FreeAndNil(messages);
  end;
end;

procedure TfrmTestApiWindow.TestOpenAIVision;
var
  openAIVision: TOpenAI;
  config: TChatSettings;
  AMessages: System.Generics.Collections.TObjectList<TChatMessage>;
  MessageVision: TChatVisionMessage;
  response: TChatResponse;
begin
  openAIVision := TOpenAI.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey'));
  try
    config.model := 'gpt-4o';
    config.json_mode := False;
    AMessages := TObjectList<TChatMessage>.Create;
    MessageVision := TChatVisionMessage.Create;
    MessageVision.Role := 'system';
    MessageVision.Content := 'You are a useful assistant';
    AMessages.Add(MessageVision);
    MessageVision := TChatVisionMessage.Create;
    MessageVision.Role := 'user';
    MessageVision.Content := 'Describe the following image';
    MessageVision.AddImageFile('C:\Users\geoff\Pictures\Chickens  035.jpg', 'image/jpeg');
    AMessages.Add(MessageVision);
    response := openAIVision.ChatCompletion(Config, AMessages);
    Memo1.Lines.Add(response.Content);
  finally
    FreeAndNil(openAIVision);
    FreeAndNil(AMessages);
  end;
end;



procedure TfrmTestApiWindow.TestDeepSeekVision;
var
  deepseek: TDeepSeek;
  config: TChatSettings;
  AMessages: System.Generics.Collections.TObjectList<TChatMessage>;
  MessageVision: TChatVisionMessage;
  response: TChatResponse;
begin
  deepseek := TDeepSeek.Create(FApiKeyStore.LoadApiKey('Deepseek_Key'));
  try
    config.model := 'deepseek-chat';
    config.json_mode := False;
    AMessages := TObjectList<TChatMessage>.Create;
    MessageVision := TChatVisionMessage.Create;
    MessageVision.Role := 'system';
    MessageVision.Content := 'You are a useful assistant';
    AMessages.Add(MessageVision);
    MessageVision := TChatVisionMessage.Create;
    MessageVision.Role := 'user';
    MessageVision.Content := 'Describe the following image';
    MessageVision.AddImageFile('C:\Users\geoff\Pictures\Chickens  035.jpg', 'image/jpeg');
    AMessages.Add(MessageVision);
    response := deepseek.ChatCompletion(Config, AMessages);
    Memo1.Lines.Add(response.Content);
  finally
    FreeAndNil(deepseek);
    FreeAndNil(AMessages);
  end;
end;


procedure TfrmTestApiWindow.TestGoogleAIVision;
var
  gemini: TGemini;
  config: TChatSettings;
  AMessages: System.Generics.Collections.TObjectList<TChatMessage>;
  MessageVision: TChatVisionMessage;
  response: TChatResponse;
begin
  gemini := TGemini.Create(FApiKeyStore.LoadApiKey('google_AI_APIKey'));
  try
    config.model := 'gemini-2.0-flash-exp';
    config.json_mode := False;
    config.top_p := 0;
    config.top_k := 0;
    AMessages := TObjectList<TChatMessage>.Create;
    MessageVision := TGemini.CreateChatVisionMessage;
    MessageVision.Role := 'system';
    MessageVision.Content := 'You are a useful assistant';
    AMessages.Add(MessageVision);
    MessageVision := TGemini.CreateChatVisionMessage;
    MessageVision.Role := 'user';
    MessageVision.Content := 'Describe the following image';
    MessageVision.AddImageFile('C:\Users\geoff\Pictures\Chickens  035.jpg', 'image/jpeg');
    AMessages.Add(MessageVision);
    response := gemini.ChatCompletion(Config, AMessages);
    Memo1.Lines.Add(response.Content);
  finally
    FreeAndNil(gemini);
    FreeAndNil(AMessages);
  end;
end;


procedure TfrmTestApiWindow.TestGoogleVoices;
var
  voice: TVoiceInfo;
  ticks: UInt64;
begin
  Memo1.Lines.Add('======== Google Voices');
  voice := Fgooglespeech.Voices[1];
  Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
  Fgooglespeech.PlayText('Hello from voice ' + voice.VoiceName, voice.VoiceId);
  ticks := GetTickCount64;
  repeat
    Application.ProcessMessages;
  until (GetTickCount64 - ticks) > 10000;
end;

procedure TfrmTestApiWindow.TestMicrosoftVoices;
var
  voice: TVoiceInfo;
  ticks: UInt64;
  msvoice: TMicrosoftCognitiveService;
begin
  Memo1.Lines.Add('======== Microsoft Voices');
  msvoice := TMicrosoftCognitiveService.Create(FApiKeyStore.LoadApiKey('ms_cognative_service_resource_key'), 'australiaeast.tts.speech.microsoft.com');
  try
    voice := msvoice.Voices[1];
    Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    msvoice.PlayText('Hello from voice ' + voice.VoiceName, voice.VoiceId);
    ticks := GetTickCount64;
    repeat
      Application.ProcessMessages;
    until (GetTickCount64 - ticks) > 10000;
  finally
    FreeAndNil(msvoice);
  end;
end;

procedure TfrmTestApiWindow.TestOpenAIFunctionCalling;
var
  openAI: TOpenAI;
  settings: TChatSettings;
  messages: System.Generics.Collections.TObjectList<TChatMessage>;
  msg: TChatMessage;
  chatAnswer: TChatResponse;
  answer: string;
begin
  openAI := TOpenAI.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey'));
  settings := Default(TChatSettings);
  try
    openAI.Functions.RegisterFunction(@TfrmTestApiWindow.GetWeather, Self);
    openAI.Functions.RegisterFunction(@TfrmTestApiWindow.GetTimeAt, Self);
    settings.model := 'gpt-4o';
    settings.json_mode := False;
    settings.max_tokens := 4096;
    messages := TObjectList<TChatMessage>.Create(True);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the weather for Bendigo?';
    messages.Add(msg);
    chatAnswer := openAI.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time at Bendigo?';
    messages.Add(msg);
    chatAnswer := openAI.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    Memo1.Lines.Add('=============  Test Multiple function calls at once  ====================');
    messages.Clear;
    msg := TChatMessage.Create;
    msg.Role := 'system';
    msg.Content := 'You are a helpful assistant';
    messages.Add(msg);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time and weather for Bendigo?';
    messages.Add(msg);
    chatAnswer := openAI.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);


  finally
    FreeAndNil(messages);
    FreeAndNil(openAI);
  end;
end;

procedure TfrmTestApiWindow.TestDeepSeekFunctionCalling;
var
  deepseek: TDeepSeek;
  settings: TChatSettings;
  messages: System.Generics.Collections.TObjectList<TChatMessage>;
  msg: TChatMessage;
  chatAnswer: TChatResponse;
  answer: string;
begin
  deepseek := TDeepSeek.Create(FApiKeyStore.LoadApiKey('Deepseek_Key'));
  settings := Default(TChatSettings);
  try
    deepseek.Functions.RegisterFunction(@TfrmTestApiWindow.GetWeather, Self);
    deepseek.Functions.RegisterFunction(@TfrmTestApiWindow.GetTimeAt, Self);
    settings.model := 'deepseek-chat';
    settings.json_mode := False;
    settings.max_tokens := 4096;
    messages := TObjectList<TChatMessage>.Create(True);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the weather for Bendigo?';
    messages.Add(msg);
    chatAnswer := deepseek.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time at Bendigo?';
    messages.Add(msg);
    chatAnswer := deepseek.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    Memo1.Lines.Add('=============  Test Multiple function calls at once  ====================');
    messages.Clear;
    msg := TChatMessage.Create;
    msg.Role := 'system';
    msg.Content := 'You are a helpful assistant';
    messages.Add(msg);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time and weather for Bendigo?';
    messages.Add(msg);
    chatAnswer := deepseek.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);


  finally
    FreeAndNil(messages);
    FreeAndNil(deepseek);
  end;
end;


procedure TfrmTestApiWindow.TestHuggingFaceFunctionCalling;
var
  openAI: THuggingFaceLLM;
  settings: TChatSettings;
  messages: System.Generics.Collections.TObjectList<TChatMessage>;
  msg: TChatMessage;
  chatAnswer: TChatResponse;
  answer: string;
begin
  openAI := THuggingFaceLLM.Create(FApiKeyStore.LoadApiKey('HuggingFace_APIKey'));
  settings := Default(TChatSettings);
  try
    openAI.Functions.RegisterFunction(@TfrmTestApiWindow.GetWeather, Self);
    openAI.Functions.RegisterFunction(@TfrmTestApiWindow.GetTimeAt, Self);
    settings.model := 'meta-llama/Llama-3.1-70B-Instruct';
    settings.json_mode := False;
    settings.max_tokens := 3400;
    messages := TObjectList<TChatMessage>.Create(True);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the weather for Bendigo, Victoria?';
    messages.Add(msg);
    chatAnswer := openAI.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time at Bendigo, victoria?';
    messages.Add(msg);
    chatAnswer := openAI.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    Memo1.Lines.Add('=============  Test Multiple function calls at once  ====================');
    messages.Clear;
    msg := TChatMessage.Create;
    msg.Role := 'system';
    msg.Content := 'You are a helpful assistant';
    messages.Add(msg);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time and weather for Bendigo?';
    messages.Add(msg);
    chatAnswer := openAI.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);


  finally
    FreeAndNil(messages);
    FreeAndNil(openAI);
  end;
end;

procedure TfrmTestApiWindow.TestAzureFunctionCalling;
var
  openAI: TMicrosoftOpenAI;
  settings: TChatSettings;
  messages: System.Generics.Collections.TObjectList<TChatMessage>;
  msg: TChatMessage;
  chatAnswer: TChatResponse;
  answer: string;
begin
  openAI := TMicrosoftOpenAI.Create(FApiKeyStore.LoadApiKey('AzureAPIKey'), AzureOpenAIEndpoint, 'gpt-4o');
  settings := Default(TChatSettings);
  try
    openAI.Functions.RegisterFunction(@TfrmTestApiWindow.GetWeather, Self);
    openAI.Functions.RegisterFunction(@TfrmTestApiWindow.GetTimeAt, Self);
    settings.model := 'gpt-4o';
    settings.json_mode := False;
    settings.max_tokens := 4096;
    messages := TObjectList<TChatMessage>.Create(True);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the weather for Bendigo?';
    messages.Add(msg);
    chatAnswer := openAI.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time at Bendigo?';
    messages.Add(msg);
    chatAnswer := openAI.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    Memo1.Lines.Add('=============  Test Multiple function calls at once  ====================');
    messages.Clear;
    msg := TChatMessage.Create;
    msg.Role := 'system';
    msg.Content := 'You are a helpful assistant';
    messages.Add(msg);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time and weather for Bendigo?';
    messages.Add(msg);
    chatAnswer := openAI.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);


  finally
    FreeAndNil(messages);
    FreeAndNil(openAI);
  end;
end;

procedure TfrmTestApiWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Fgooglespeech);
  FreeAndNil(FSettings);
  FreeAndNil(FProcedures);
end;


procedure TfrmTestApiWindow.MenuItemClick(Sender: TObject);
var
  index: Integer;
  procedureRec: TTestProcedure;
begin
  if Sender is TMenuItem then
  begin
    index := TMenuItem(Sender).Tag;
    if (index >= 0) and (index < FProcedures.Count) then
    begin
      procedureRec := FProcedures[index];
      Memo1.Lines.Add('+====' + TMenuItem(Sender).Caption);
      procedureRec.Method.Invoke(procedureRec.Instance, []);
      Memo1.Lines.Add('-====' + TMenuItem(Sender).Caption);
      Memo1.Lines.Add('');
    end;
  end;
end;

procedure TfrmTestApiWindow.FormCreate(Sender: TObject);
begin
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  FApiKeyStore := TApiKeyStore.GetInstance;
  Fgooglespeech := TGoogleSpeechService.Create(FApiKeyStore.LoadApiKey('google_clientid'), FApiKeyStore.LoadApiKey('google_clientsecret'), 'ADUG Demo', '', FSettings);
  FProcedures := TList<TTestProcedure>.Create;
  FindProcedures;
end;

procedure TfrmTestApiWindow.miAPIKeysClick(Sender: TObject);
var
  frmApiKeyStores : TfrmApiKeyStores;
begin
  frmApiKeyStores := TfrmApiKeyStores.Create(nil);
  try
    frmApiKeyStores.ShowModal;
  finally
    FreeAndNil(frmApiKeyStores);
  end;
end;

procedure TfrmTestApiWindow.miGoogleAuthenticateClick(Sender: TObject);
begin
  Fgooglespeech.Authenticate;
end;


{*
  Just a simple stub function to demo function calling
*}

function TfrmTestApiWindow.GetTimeAt(const state, location: string): string;
begin
  Result := DateTimeToStr(now);
end;

{*
  Just a simple stub function to demo function calling
*}

function TfrmTestApiWindow.GetWeather(const state: string; const location: string): string;
begin
  ShowMessage('GetWeather for ' + location + ', ' + state);
  Result := 'The Temperature is 28 degrees';
end;


procedure TfrmTestApiWindow.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmTestApiWindow.ListOpenAIModels;
var
  openAI: TOpenAI;
  Local_modelObj: TBaseModelInfo;
begin
  Memo1.Lines.Add('======== Model OpenAI');
  openAI := TOpenAI.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey'));
  try
    for Local_modelObj in openAI.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + Local_modelObj.modelName);
    end;
  finally
    FreeAndNil(openAI);
  end;
end;

procedure TfrmTestApiWindow.ListDeekSeekModels;
var
  deepseek: TDeepSeek;
  Local_modelObj: TBaseModelInfo;
begin
  Memo1.Lines.Add('======== Model DeekSeek');
  deepseek := TDeepSeek.Create(FApiKeyStore.LoadApiKey('Deepseek_Key'));
  try
    for Local_modelObj in deepseek.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + Local_modelObj.modelName);
    end;
  finally
    FreeAndNil(deepseek);
  end;
end;




procedure TfrmTestApiWindow.ListMistralAIModels;
var
  mistralAI: TMistral;
  Local_modelObj: TBaseModelInfo;
begin
  Memo1.Lines.Add('======== Model Mistral AI');
  mistralAI := TMistral.Create(FApiKeyStore.LoadApiKey('Mistral_APIKey'));
  try
    for Local_modelObj in mistralAI.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + Local_modelObj.modelName);
    end;
  finally
    FreeAndNil(mistralAI);
  end;
end;

procedure TfrmTestApiWindow.ListGroqModels;
var
  groqLLM: TGroqLLM;
  modelObj: TBaseModelInfo;
begin
  Memo1.Lines.Add('======== Model Groq');
  groqLLM := TGroqLLM.Create(FApiKeyStore.LoadApiKey('groq_apikey'));
  try
    for modelObj in groqLLM.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelObj.modelName);
    end;
  finally
    FreeAndNil(groqLLM);
  end;
end;



procedure TfrmTestApiWindow.ListReplicateLLM;
var
  replicate: TReplicateLLM;
  modelObj: TBaseModelInfo;
  answer : string;
begin
  Memo1.Lines.Add('======== Model Replicate LLM');
  replicate := TReplicateLLM.Create(FApiKeyStore.LoadApiKey('Replicate_APIKey'));
  try
    for modelObj in replicate.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelObj.modelName + ' ' + modelObj.version);
    end;

    answer := replicate.Completion('How long is a piece of string', 'llama-2-70b-chat');
    Memo1.Lines.Add('Answer : ' + answer);
  finally
    FreeAndNil(replicate);
  end;
end;

procedure TfrmTestApiWindow.ListAzureLLMModels;
var
  microsoftOpenAI: TMicrosoftOpenAI;
  Local_modelObj: TBaseModelInfo;
begin
  Memo1.Lines.Add('======== Microsoft OpenAI');
  microsoftOpenAI := TMicrosoftOpenAI.Create(FApiKeyStore.LoadApiKey('AzureAPIKey'), AzureOpenAIEndpoint, 'gpt-4o');
  try
    for Local_modelObj in microsoftOpenAI.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + Local_modelObj.modelName);
    end;
  finally
    FreeAndNil(microsoftOpenAI);
  end;
end;

procedure TfrmTestApiWindow.ListGoogleLLMModels;
var
  gemini: TGemini;
  Local_modelObj: TBaseModelInfo;
begin
  Memo1.Lines.Add('======== Model Google');
  gemini := TGemini.Create(FApiKeyStore.LoadApiKey('google_AI_APIKey'));
  try
    for Local_modelObj in gemini.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + Local_modelObj.modelName);
    end;
  finally
    FreeAndNil(gemini);
  end;
end;

procedure TfrmTestApiWindow.TestGoogleLLM;
var
  gemini: TGemini;
  modelObj: TBaseModelInfo;
  answer: string;
  settings: TChatSettings;
  messages: TObjectList<TChatMessage>;
  msg : TChatMessage;
begin
  Memo1.Lines.Add('======== Model Google Gemini');
  messages := nil;
  gemini := nil;

  try
    gemini := TGemini.Create(FApiKeyStore.LoadApiKey('google_AI_APIKey'));
    for modelObj in gemini.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelObj.modelName + ' ' + modelObj.version);
    end;
    settings.model := 'gemini-2.0-flash-exp';
    settings.json_mode := False;



    messages := TObjectList<TChatMessage>.Create;
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'How long is a piece of string';
    messages.Add(msg);


    answer := gemini.ChatCompletion(settings, messages).Content;
    Memo1.Lines.Add('Answer : ' + answer);
  finally
    FreeAndNil(gemini);
    FreeAndNil(messages);
  end;
end;

procedure TfrmTestApiWindow.TestOpenAITTS;
var
  openAIVoice: TOpenAITextToSpeech;
  voice: TVoiceInfo;
  ticks: DWORD;
begin
  Memo1.Lines.Add('======== OpenAI Voices');
  openAIVoice := TOpenAITextToSpeech.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey'));
  try
    voice := openAIVoice.Voices[1];
    Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    openAIVoice.PlayText('Hello from voice ' + voice.VoiceName, voice.VoiceId);
    ticks := GetTickCount64;
    repeat
      Application.ProcessMessages;
    until (GetTickCount64 - ticks) > 10000;
  finally
    FreeAndNil(openAIVoice);
  end;
end;

procedure TfrmTestApiWindow.TestElevenLabsTTS;
var
  elevenlabs: TElevenLabsService;
  voice: TVoiceInfo;
  ticks: UInt64;
begin
  Memo1.Lines.Add('======== ElevenLabs Voices');
  elevenlabs := TElevenLabsService.Create(FApiKeyStore.LoadApiKey('ElevenLabsAPIKey'));
  try
    voice := elevenlabs.Voices[1];
    Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    elevenlabs.PlayText('Hello from voice ' + voice.VoiceName, voice.VoiceId);
    ticks := GetTickCount64;
    repeat
      Application.ProcessMessages;
    until (GetTickCount64 - ticks) > 10000;
  finally
    FreeAndNil(elevenlabs);
  end;
end;


procedure TfrmTestApiWindow.TestGrokLLM;
var
  grok: TXGrokAI;
  modelObj: TBaseModelInfo;
  answer: string;
  settings: TChatSettings;
  messages: TObjectList<TChatMessage>;
  msg : TChatMessage;
begin
  Memo1.Lines.Add('======== Model X.AI Grok');
  messages := nil;
  grok := nil;

  try
    grok := TXGrokAI.Create(FApiKeyStore.LoadApiKey('X_AI'));
    for modelObj in grok.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelObj.modelName + ' ' + modelObj.version);
    end;
    settings.model := 'grok-beta';
    settings.json_mode := False;



    messages := TObjectList<TChatMessage>.Create;
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'How long is a piece of string';
    messages.Add(msg);


    answer := grok.ChatCompletion(settings, messages).Content;
    Memo1.Lines.Add('Answer : ' + answer);
  finally
    FreeAndNil(grok);
    FreeAndNil(messages);
  end;
end;

procedure TfrmTestApiWindow.TestGroqFunctionCalling;
var
  groq: TGroqLLM;
  settings: TChatSettings;
  messages: System.Generics.Collections.TObjectList<TChatMessage>;
  msg: TChatMessage;
  chatAnswer: TChatResponse;
  answer: string;
begin
  groq := TGroqLLM.Create(FApiKeyStore.LoadApiKey('groq_apikey'));
  settings := Default(TChatSettings);
  try
    groq.Functions.RegisterFunction(@TfrmTestApiWindow.GetWeather, Self);
    groq.Functions.RegisterFunction(@TfrmTestApiWindow.GetTimeAt, Self);
    settings.model := 'llama-3.2-90b-vision-preview';
    settings.json_mode := False;
    settings.max_tokens := 4096;
    messages := TObjectList<TChatMessage>.Create(True);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the weather for Bendigo?';
    messages.Add(msg);
    chatAnswer := groq.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time at Bendigo?';
    messages.Add(msg);
    chatAnswer := groq.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    Memo1.Lines.Add('=============  Test Multiple function calls at once  ====================');
    messages.Clear;
    msg := TChatMessage.Create;
    msg.Role := 'system';
    msg.Content := 'You are a helpful assistant';
    messages.Add(msg);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time and weather for Bendigo?';
    messages.Add(msg);
    chatAnswer := groq.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);
    messages.Clear;

  finally
    FreeAndNil(messages);
    FreeAndNil(groq);
  end;
end;

procedure TfrmTestApiWindow.TestMistralFunctionCalling;
var
  mistral: TMistral;
  settings: TChatSettings;
  messages: System.Generics.Collections.TObjectList<TChatMessage>;
  msg: TChatMessage;
  chatAnswer: TChatResponse;
  answer: string;
begin
  mistral := TMistral.Create(FApiKeyStore.LoadApiKey('Mistral_APIKey'));
  settings := Default(TChatSettings);
  try
    mistral.Functions.RegisterFunction(@TfrmTestApiWindow.GetWeather, Self);
    mistral.Functions.RegisterFunction(@TfrmTestApiWindow.GetTimeAt, Self);
    settings.model := 'mistral-large-latest';
    settings.json_mode := False;
    settings.max_tokens := 32384;
    messages := TObjectList<TChatMessage>.Create(True);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the weather for Bendigo?';
    messages.Add(msg);
    chatAnswer := mistral.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    msg := TChatMessage.Create;
    msg.Role := chatAnswer.Role;
    msg.Content := chatAnswer.Content;
    messages.Add(msg);
    Memo1.Lines.Add('Answer : ' + answer);
    Sleep(2000);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time at Bendigo?';
    messages.Add(msg);
    chatAnswer := mistral.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    Memo1.Lines.Add('=============  Test Multiple function calls at once  ====================');
    messages.Clear;
    Sleep(2000);
    msg := TChatMessage.Create;
    msg.Role := 'system';
    msg.Content := 'You are a helpful assistant';
    messages.Add(msg);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time and weather for Bendigo?';
    messages.Add(msg);
    chatAnswer := mistral.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);
    messages.Clear;

  finally
    FreeAndNil(messages);
    FreeAndNil(mistral);
  end;
end;

procedure TfrmTestApiWindow.TestGoogleGeminiFunctionCalling;
var
  gemini: TGemini;
  settings: TChatSettings;
  messages: System.Generics.Collections.TObjectList<TChatMessage>;
  msg: TChatMessage;
  chatAnswer: TChatResponse;
  answer: string;
begin
  gemini := TGemini.Create(FApiKeyStore.LoadApiKey('google_AI_APIKey'));
{  gemini.OnLog := procedure(inLog: string)
                  begin
                    Memo1.Lines.Add(inLog);
                  end; }
  settings := Default(TChatSettings);
  try
    gemini.Functions.RegisterFunction(@TfrmTestApiWindow.GetWeather, Self);
    gemini.Functions.RegisterFunction(@TfrmTestApiWindow.GetTimeAt, Self);
    settings.model := 'gemini-2.0-flash-exp';
    settings.json_mode := False;
    settings.max_tokens := 4096;
    settings.top_p := 0;
    settings.top_k := 0;
    messages := TObjectList<TChatMessage>.Create(True);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the weather for Bendigo, Victoria?';
    Memo1.Lines.Add('Question : ' + msg.Content);
    messages.Add(msg);
    chatAnswer := gemini.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    messages.Clear;
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time at Bendigo, Victoria?';
    Memo1.Lines.Add('Question : ' + msg.Content);
    messages.Add(msg);
    chatAnswer := gemini.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    Memo1.Lines.Add('=============  Test Multiple function calls at once  ====================');
    messages.Clear;
    msg := TChatMessage.Create;
    msg.Role := 'system';
    msg.Content := 'You are a helpful assistant';
    messages.Add(msg);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time and weather for Bendigo, Victoria?';
    Memo1.Lines.Add('Question : ' + msg.Content);
    messages.Add(msg);
    chatAnswer := gemini.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);
    messages.Clear;

  finally
    FreeAndNil(messages);
    FreeAndNil(gemini);
  end;
end;

procedure TfrmTestApiWindow.TestAnthropicClaudeFunctionCalling;
var
  claude: TAnthropic;
  settings: TChatSettings;
  messages: System.Generics.Collections.TObjectList<TChatMessage>;
  msg: TChatMessage;
  chatAnswer: TChatResponse;
  answer: string;
begin
  claude := TAnthropic.Create(FApiKeyStore.LoadApiKey('Claude_APIKey'));
{  claude.OnLog := procedure(inLog: string)
                  begin
                    Memo1.Lines.Add(inLog);
                  end; }
  settings := Default(TChatSettings);
  try
    claude.Functions.RegisterFunction(@TfrmTestApiWindow.GetWeather, Self);
    claude.Functions.RegisterFunction(@TfrmTestApiWindow.GetTimeAt, Self);
    settings.model := 'claude-3-5-sonnet-20241022';
    settings.json_mode := False;
    settings.max_tokens := 4096;
    messages := TObjectList<TChatMessage>.Create(True);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the weather for Bendigo, Victoria?';
    messages.Add(msg);
    chatAnswer := claude.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    messages.Clear;
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time at Bendigo, Victoria?';
    messages.Add(msg);
    chatAnswer := claude.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    Memo1.Lines.Add('=============  Test Multiple function calls at once  ====================');
    messages.Clear;
    msg := TChatMessage.Create;
    msg.Role := 'system';
    msg.Content := 'You are a helpful assistant';
    messages.Add(msg);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time and weather for Bendigo?';
    messages.Add(msg);
    chatAnswer := claude.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);
    messages.Clear;

  finally
    FreeAndNil(messages);
    FreeAndNil(claude);
  end;
end;


procedure TfrmTestApiWindow.TestXaiGrokFunctionCalling;
var
  grok: TXGrokAI;
  settings: TChatSettings;
  messages: System.Generics.Collections.TObjectList<TChatMessage>;
  msg: TChatMessage;
  chatAnswer: TChatResponse;
  answer: string;
begin
  grok := TXGrokAI.Create(FApiKeyStore.LoadApiKey('X_AI'));
  settings := Default(TChatSettings);
  try
    grok.Functions.RegisterFunction(@TfrmTestApiWindow.GetWeather, Self);
    grok.Functions.RegisterFunction(@TfrmTestApiWindow.GetTimeAt, Self);
    settings.model := 'grok-beta';
    settings.json_mode := False;
    settings.max_tokens := 4096;
    messages := TObjectList<TChatMessage>.Create(True);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the weather for Bendigo, Victoria?';
    messages.Add(msg);
    chatAnswer := grok.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time at Bendigo, Victoria?';
    messages.Add(msg);
    chatAnswer := grok.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);

    Memo1.Lines.Add('=============  Test Multiple function calls at once  ====================');
    messages.Clear;
    msg := TChatMessage.Create;
    msg.Role := 'system';
    msg.Content := 'You are a helpful assistant';
    messages.Add(msg);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the time and weather for Bendigo?';
    messages.Add(msg);
    chatAnswer := grok.ChatCompletion(settings, messages);
    answer := chatAnswer.Content;
    Memo1.Lines.Add('Answer : ' + answer);
    messages.Clear;

  finally
    FreeAndNil(messages);
    FreeAndNil(grok);
  end;
end;

procedure TfrmTestApiWindow.TestGroqLLM;
var
  groq: TGroqLLM;
  modelObj: TBaseModelInfo;
  answer: string;
  settings: TChatSettings;
  messages: TObjectList<TChatMessage>;
  msg : TChatMessage;
begin
  Memo1.Lines.Add('======== Model Groq');
  messages := nil;
  groq := nil;

  try
    groq := TGroqLLM.Create(FApiKeyStore.LoadApiKey('groq_apikey'));
    for modelObj in groq.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelObj.modelName + ' ' + modelObj.version);
    end;
    settings.model := 'mixtral-8x7b-32768';
    settings.json_mode := False;
    messages := TObjectList<TChatMessage>.Create;
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'How long is a piece of string';
    messages.Add(msg);


    answer := groq.ChatCompletion(settings, messages).Content;
    Memo1.Lines.Add('Answer : ' + answer);
  finally
    FreeAndNil(groq);
    FreeAndNil(messages);
  end;
end;

procedure TfrmTestApiWindow.TestMistralLLM;
var
  mistral: TMistral;
  modelObj: TBaseModelInfo;
  answer: string;
  settings: TChatSettings;
  messages: TObjectList<TChatMessage>;
  msg : TChatMessage;
begin
  Memo1.Lines.Add('======== Model Mistral');
  messages := nil;
  mistral := nil;

  try
    mistral := TMistral.Create(FApiKeyStore.LoadApiKey('Mistral_APIKey'));
    for modelObj in mistral.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelObj.modelName + ' ' + modelObj.version);
    end;
    settings.model := 'mistral-large-latest';
    settings.json_mode := False;
    messages := TObjectList<TChatMessage>.Create;
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'How long is a piece of string';
    messages.Add(msg);

    Memo1.Lines.Add('Question : ' + msg.Content);
    answer := mistral.ChatCompletion(settings, messages).Content;
    Memo1.Lines.Add('Answer : ' + answer);
  finally
    FreeAndNil(mistral);
    FreeAndNil(messages);
  end;
end;

procedure TfrmTestApiWindow.TestGroqVisionLLM;
var
  groq: TGroqLLM;
  modelObj: TBaseModelInfo;
  answer: string;
  settings: TChatSettings;
  messages: TObjectList<TChatMessage>;
  MessageVision : TChatVisionMessage;
begin
  Memo1.Lines.Add('======== Model Groq');
  messages := nil;
  groq := nil;

  try
    groq := TGroqLLM.Create(FApiKeyStore.LoadApiKey('groq_apikey'));
    for modelObj in groq.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelObj.modelName + ' ' + modelObj.version);
    end;
    settings.model := 'llama-3.2-90b-vision-preview';
    settings.json_mode := False;
    messages := TObjectList<TChatMessage>.Create;
    MessageVision := TChatVisionMessage.Create;
    MessageVision.Role := 'user';
    MessageVision.Content := 'Describe the farm scene in the photo';
    MessageVision.AddImageFile('C:\Users\geoff\Pictures\Chickens  035.jpg', 'image/jpeg');
    messages.Add(MessageVision);

    answer := groq.ChatCompletion(settings, messages).Content;
    Memo1.Lines.Add('Answer : ' + answer);
  finally
    FreeAndNil(groq);
    FreeAndNil(messages);
  end;
end;

procedure TfrmTestApiWindow.ListAmazonVoices;
var
  polly: TAmazonPollyService;
  Local_voice: TVoiceInfo;
begin
  Memo1.Lines.Add('======== Amazon Polly Voices');
  polly := TAmazonPollyService.Create(FApiKeyStore.LoadApiKey('AWSAccessKey'), FApiKeyStore.LoadApiKey('AWSSecretKey'), FApiKeyStore.LoadSetting('AWSRegion'));
  try
    for Local_voice in polly.Voices do
    begin
      Memo1.Lines.Add(Local_voice.VoiceId + ' | ' + Local_voice.VoiceName + ' | ' + Local_voice.VoiceGender);
    end;
  finally
    FreeAndNil(polly);
  end;
end;

procedure TfrmTestApiWindow.ListAnthropicModels;
var
  anthropic: TAnthropic;
  Local_modelObj: TBaseModelInfo;
begin
  Memo1.Lines.Add('======== Model Anthropic');
  anthropic := TAnthropic.Create(FApiKeyStore.LoadApiKey('Claude_APIKey'));
  try
    for Local_modelObj in anthropic.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + Local_modelObj.modelName);
    end;
  finally
    FreeAndNil(anthropic);
  end;
end;

procedure TfrmTestApiWindow.ListElevenLabsVoices;
var
  elevenlabs: TElevenLabsService;
  Local_voice: TVoiceInfo;
begin
  Memo1.Lines.Add('======== ElevenLabs Voices');
  elevenlabs := TElevenLabsService.Create(FApiKeyStore.LoadApiKey('ElevenLabsAPIKey'));
  try
    for Local_voice in elevenlabs.Voices do
    begin
      Memo1.Lines.Add(Local_voice.VoiceId + ' | ' + Local_voice.VoiceName + ' | ' + Local_voice.VoiceGender);
    end;
  finally
    FreeAndNil(elevenlabs);
  end;
end;

procedure TfrmTestApiWindow.TestConquiVoices;
var
  voice: TVoiceInfo;
  conqui : TCoquiTTSService;
begin
  Memo1.Lines.Add('======== Conqui Polly Voices');
  conqui := TCoquiTTSService.Create('key', 'http://172.24.24.116:5002');
  try
    for voice in conqui.Voices do
    begin
      Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    end;
    conqui.PlayText('How Long is a piece of string?', '');
  finally
    FreeAndNil(conqui);
  end;
end;

procedure TfrmTestApiWindow.TestAmazonPolly;
var
  polly: TAmazonPollyService;
  voice: TVoiceInfo;
  ticks: UInt64;
begin
  Memo1.Lines.Add('======== Amazon Polly Voices');
  polly := TAmazonPollyService.Create(FApiKeyStore.LoadApiKey('AWSAccessKey'), FApiKeyStore.LoadApiKey('AWSSecretKey'), FApiKeyStore.LoadSetting('AWSRegion'));
  try
    voice := polly.Voices[1];
    Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    polly.PlayText('Hello from voice ' + voice.VoiceName, voice.VoiceId);
    ticks := GetTickCount64;
    repeat
      Application.ProcessMessages;
    until (GetTickCount64 - ticks) > 10000;
  finally
    FreeAndNil(polly);
  end;
end;

procedure TfrmTestApiWindow.ListWindowsVoices;
var
  mswindows: TWindowsSpeechService;
  Local_voice: TVoiceInfo;
begin
  Memo1.Lines.Add('======== Windows Voices');
  mswindows := TWindowsSpeechService.Create;
  try
    for Local_voice in mswindows.Voices do
    begin
      Memo1.Lines.Add(Local_voice.VoiceId + ' | ' + Local_voice.VoiceName + ' | ' + Local_voice.VoiceGender);
    end;
  finally
    FreeAndNil(mswindows);
  end;
end;

procedure TfrmTestApiWindow.TestWindowsVoice;
var
  mswindows: TWindowsSpeechService;
  voice: TVoiceInfo;
  ticks: UInt64;
begin
  Memo1.Lines.Add('======== Windows Voices');
  mswindows := TWindowsSpeechService.Create;
  try
    voice := mswindows.Voices[1];
    Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    mswindows.PlayText('Hello from voice ' + voice.VoiceName, voice.VoiceId);
    ticks := GetTickCount64;
    repeat
      Application.ProcessMessages;
    until (GetTickCount64 - ticks) > 10000;
  finally
    FreeAndNil(mswindows);
  end;
end;


procedure TfrmTestApiWindow.ListMicrosoftVoices;
var
  msvoice: TMicrosoftCognitiveService;
  Local_voice: TVoiceInfo;
begin
  Memo1.Lines.Add('======== Microsoft Voices');
  msvoice := TMicrosoftCognitiveService.Create(FApiKeyStore.LoadApiKey('ms_cognative_service_resource_key'), 'australiaeast.tts.speech.microsoft.com');
  try
    for Local_voice in msvoice.Voices do
    begin
      Memo1.Lines.Add(Local_voice.VoiceId + ' | ' + Local_voice.VoiceName + ' | ' + Local_voice.VoiceGender);
    end;
  finally
    FreeAndNil(msvoice);
  end;
end;

procedure TfrmTestApiWindow.TestHunggingFaceLLM;
var
  huggingFace: THuggingFaceLLM;
  modelObj: TBaseModelInfo;
  answer: string;
  messages : TObjectList<TChatMessage>;
  msg : TChatMessage;
  config : TChatSettings;
begin
  Memo1.Lines.Add('======== Model HuggingFace');
  huggingFace := THuggingFaceLLM.Create(FApiKeyStore.LoadApiKey('HuggingFace_APIKey'));
  try
    for modelObj in huggingFace.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelObj.modelName + ' ' + modelObj.version);
    end;
    messages := TObjectList<TChatMessage>.Create(True);
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'How long is a piece of string?';
    messages.Add(msg);

    config := Default(TChatSettings);
    config.model := 'meta-llama/Llama-3.2-1B-Instruct';
    answer := huggingFace.ChatCompletion(config, messages).Content;
    Memo1.Lines.Add('Answer : ' + answer);
  finally
    FreeAndNil(huggingFace);
    FreeAndNil(messages);
  end;
end;

procedure TfrmTestApiWindow.ListAmazonTranslationLanguages;
var
  amazonEngine: TAmazonTranslate;
  langlist: System.Generics.Collections.TObjectList<TLanguageInfo>;
  Local_lang: TLanguageInfo;
  Local_lang1: TLanguageInfo;
begin
  amazonEngine := TAmazonTranslate.Create(FApiKeyStore.LoadApiKey('AWSAccessKey'), FApiKeyStore.LoadApiKey('AWSSecretkey'), FApiKeyStore.LoadSetting('AWSRegion'));
  try
    Memo1.Lines.Add('======== Amazon Translate');
    langlist := amazonEngine.FromLanguages;
    for Local_lang in langlist do
    begin
      Memo1.Lines.Add('lang= ' + Local_lang.LanguageCode + ' | ' + Local_lang.LanguageName);
    end;
    langlist := amazonEngine.ToLanguages;
    for Local_lang1 in langlist do
    begin
      Memo1.Lines.Add('lang= ' + Local_lang1.LanguageCode + ' | ' + Local_lang1.LanguageName);
    end;
  finally
    FreeAndNil(amazonEngine);
  end;
end;

procedure TfrmTestApiWindow.ListMicrosoftTranslateLanguages;
var
  msTranslate: TMicrosoftTranslate;
  lang: TLanguageInfo;
begin
  msTranslate := TMicrosoftTranslate.Create(FApiKeyStore.LoadApiKey('ms_translate_key'), 'https://api.cognitive.microsofttranslator.com/');
  try
    Memo1.Lines.Add('======== Microsoft Translate');
    for lang in msTranslate.FromLanguages do
    begin
      Memo1.Lines.Add('lang= ' + lang.LanguageCode + ' | ' + lang.LanguageName);
    end;
    for lang in msTranslate.ToLanguages do
    begin
      Memo1.Lines.Add('lang= ' + lang.LanguageCode + ' | ' + lang.LanguageName);
    end;
  finally
    FreeAndNil(msTranslate);
  end;
end;

procedure TfrmTestApiWindow.ListGoogleVoices;
var
  voice: TVoiceInfo;
begin
  Memo1.Lines.Add('======== Google Voices');
  for voice in Fgooglespeech.Voices do
  begin
    Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
  end;
end;

procedure TfrmTestApiWindow.ListOpenAIVoices;
var
  openAIVoice: TOpenAITextToSpeech;
  voice: TVoiceInfo;
begin
  openAIVoice := TOpenAITextToSpeech.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey'));
  try
    for voice in openAIVoice.Voices do
    begin
      Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    end;
  finally
    FreeAndNil(openAIVoice);
  end;
end;

procedure TfrmTestApiWindow.ListReplicateImageGenerators;
var
  imageGenReplicate: TImageGenerationReplicate;
  modelImg: TImageModelInfo;
begin
  Memo1.Lines.Add('======== Model Replicate ImageGen');
  imageGenReplicate := TImageGenerationReplicate.Create(FApiKeyStore.LoadApiKey('Replicate_APIKey'));
  try
    for modelImg in imageGenReplicate.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelImg.modelName + ' ' + modelImg.version);
    end;
  finally
    FreeAndNil(imageGenReplicate);
  end;
end;

end.
