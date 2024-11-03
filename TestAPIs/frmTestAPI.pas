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
    Button2: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    Fgooglespeech : TGoogleSpeechService;
    FProcedures : TList<TTestProcedure>;
    procedure MenuItemClick(Sender: TObject);
  public
    { Public declarations }
    procedure ListOpenAIModels;
    procedure ListReplicateImageGenerators;
    procedure ListReplicateLLM;
    procedure ListAzureLLMModels;
    procedure ListGoogleLLMModels;
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
    procedure TestAnthropicClaudeVision;
    procedure TestHunggingFaceLLM;
    procedure TestGroqLLM;
    procedure TestGroqVisionLLM;
    procedure ListGroqModels;
    procedure TestGrokLLM;

    procedure TestOpenAIFunctionCalling;

    procedure TestAPIKeyStore;

    [FunctionDescription('Get the weather forecast')]
    function GetWeather([ParamDescription('State of the location')]const state: string; [ParamDescription('Location for the weather forecast')]const location: string): string;
    [FunctionDescription('Get the time')]
    function GetTimeAt([ParamDescription('State of the location')]const state: string; [ParamDescription('Location for the time')]const location: string): string;



    procedure FindProcedures;
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
  uLLM.Google.PaLM,
  uLLM.Anthropic,
  uLLM.HuggingFace,
  uLLM.Replicate,
  uLLM.Groq,
  uLLM.X.Ai,
  uDALLe2.DTO,
  uImageGeneration,
  uImageGeneration.Replicate
  ;

{$I ..\Libs\apikey.inc}

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
  msg: TClaudeVisionMessage;
  answer: string;
begin
  Memo1.Lines.Add('======== Model Anthropic');
  anthropic := TAnthropic.Create(Claude_APIKey);
  try
    for Local_modelObj in anthropic.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + Local_modelObj.modelName);
    end;
    settings.json_mode := False;
    settings.model := 'claude-3-5-sonnet-20240620';
    settings.max_tokens := 1024;
    messages := TObjectList<TChatMessage>.Create;
    msg := TClaudeVisionMessage.Create;
    msg.Role := 'user';
    msg.AddImageFile('C:\Users\geoff\Pictures\Chickens  035.jpg', 'image/jpeg');
    msg.Content := 'Describe the following image';
    messages.Add(msg);
    answer := anthropic.ChatCompletion(settings, messages).Content;
    Memo1.Lines.Add('Answer: ' + answer);
  finally
    FreeAndNil(anthropic);
    FreeAndNil(messages);
  end;
end;

procedure TfrmTestApiWindow.TestAPIKeyStore;
var
  KeyStore: TApiKeyStore;
begin
  KeyStore := TApiKeyStore.GetInstance;
  try
    KeyStore.SaveApiKey('chatgpt_apikey', chatgpt_apikey); // Store API key
    KeyStore.SaveApiKey('X_AI', X_AI);
    KeyStore.SaveApiKey('groq_apikey', groq_apikey);
    ShowMessage(KeyStore.LoadApiKey('chatgpt_apikey')); // Retrieve API key
  finally
    FreeAndNil(KeyStore);
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
  openAIVision := TOpenAI.Create(chatgpt_apikey);
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
  msvoice := TMicrosoftCognitiveService.Create(Self, ms_cognative_service_resource_key, 'australiaeast.tts.speech.microsoft.com');
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
  openAI := TOpenAI.Create(chatgpt_apikey);
  settings := Default(TChatSettings);
  try
    openAI.Functions.RegisterFunction(@TfrmTestApiWindow.GetWeather, Self);
    openAI.Functions.RegisterFunction(@TfrmTestApiWindow.GetTimeAt, Self);
    settings.model := 'gpt-4o';
    settings.json_mode := False;
    settings.max_tokens := 4096;
    messages := TObjectList<TChatMessage>.Create;
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
  Fgooglespeech := TGoogleSpeechService.Create(Self, google_clientid, google_clientsecret,'ADUG Demo', '', FSettings);
  FProcedures := TList<TTestProcedure>.Create;

  FindProcedures;
end;

procedure TfrmTestApiWindow.Button2Click(Sender: TObject);
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
  openAI := TOpenAI.Create(chatgpt_apikey);
  try
    for Local_modelObj in openAI.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + Local_modelObj.modelName);
    end;
  finally
    FreeAndNil(openAI);
  end;
end;

procedure TfrmTestApiWindow.ListGroqModels;
var
  groqLLM: TGroqLLM;
  modelObj: TBaseModelInfo;
begin
  Memo1.Lines.Add('======== Model Groq');
  groqLLM := TGroqLLM.Create(groq_apikey);
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
  replicate := TReplicateLLM.Create(Replicate_APIKey);
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
  microsoftOpenAI := TMicrosoftOpenAI.Create(AzureAPIKey, AzureOpenAIEndpoint);
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
  palm: TGooglePaLM;
  Local_modelObj: TBaseModelInfo;
begin
  Memo1.Lines.Add('======== Model Google');
  palm := TGooglePaLM.Create(google_makersuite);
  try
    for Local_modelObj in palm.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + Local_modelObj.modelName);
    end;
  finally
    FreeAndNil(palm);
  end;
end;

procedure TfrmTestApiWindow.TestOpenAITTS;
var
  openAIVoice: TOpenAITextToSpeech;
  voice: TVoiceInfo;
  ticks: DWORD;
begin
  Memo1.Lines.Add('======== OpenAI Voices');
  openAIVoice := TOpenAITextToSpeech.Create(Self, chatgpt_apikey);
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
  elevenlabs := TElevenLabsService.Create(Self, ElevenLabsAPIKey);
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
    grok := TXGrokAI.Create(X_AI);
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
    groq := TGroqLLM.Create(groq_apikey);
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
    groq := TGroqLLM.Create(groq_apikey);
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
  polly := TAmazonPollyService.Create(Self, AWSAccessKey, AWSSecretKey, AWSRegion);
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
  anthropic := TAnthropic.Create(Claude_APIKey);
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
  elevenlabs := TElevenLabsService.Create(Self, ElevenLabsAPIKey);
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
  conqui := TCoquiTTSService.Create(Self, 'key', 'http://172.24.24.116:5002');
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
  polly := TAmazonPollyService.Create(Self, AWSAccessKey, AWSSecretKey, AWSRegion);
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
  mswindows := TWindowsSpeechService.Create(Self);
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
  mswindows := TWindowsSpeechService.Create(Self);
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
  msvoice := TMicrosoftCognitiveService.Create(Self, ms_cognative_service_resource_key, 'australiaeast.tts.speech.microsoft.com');
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
begin
  //    imgs := imageGenReplicate.Generate('A bird sitting on a branch in a tree', 1, TDALLESIZE.DALLE1024, 'stable-diffusion');
  //    try
  //      for I := 0 to length(imgs.data) - 1 do
  //      begin
  //        Memo1.Lines.Add('ImageURL: ' + imgs.data[i].url);
  //      end;
  //    finally
  //      FreeAndNil(imgs);
  //    end;
  Memo1.Lines.Add('======== Model HuggingFace');
  huggingFace := THuggingFaceLLM.Create(HuggingFace_APIKey);
  try
    for modelObj in huggingFace.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelObj.modelName + ' ' + modelObj.version);
    end;
    answer := huggingFace.Completion('How long is a piece of string', 'gpt2');
    Memo1.Lines.Add('Answer : ' + answer);
  finally
    FreeAndNil(huggingFace);
  end;
end;

procedure TfrmTestApiWindow.ListAmazonTranslationLanguages;
var
  amazonEngine: TAmazonTranslate;
  langlist: System.Generics.Collections.TObjectList<TLanguageInfo>;
  Local_lang: TLanguageInfo;
  Local_lang1: TLanguageInfo;
begin
  amazonEngine := TAmazonTranslate.Create(AWSAccessKey, AWSSecretkey, AWSRegion);
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
  msTranslate := TMicrosoftTranslate.Create(ms_translate_key, 'https://api.cognitive.microsofttranslator.com/');
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
  openAIVoice := TOpenAITextToSpeech.Create(Self, chatgpt_apikey);
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
  imageGenReplicate := TImageGenerationReplicate.Create(Replicate_APIKey);
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
