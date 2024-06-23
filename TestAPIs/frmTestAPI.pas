unit frmTestAPI;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Generics.Collections,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  IniFiles,
  uAttributes,
  uTTS.GoogleSpeech,
  uTTS.Coqui
  ;

type
  TfrmTestApiWindow = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    Fgooglespeech : TGoogleSpeechService;
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
    procedure TestHunggingFaceLLM;
    procedure TestGroqLLM;
    procedure ListGroqModels;
  public
    { Public declarations }
    [FunctionDescription('Get the weather forecast')]
    function GetWeather([ParamDescription('State of the location')]const state: string; [ParamDescription('Location for the weather forecast')]const location: string): string;
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
  uDALLe2.DTO,
  uImageGeneration,
  uImageGeneration.Replicate
  ;

{$I ..\Libs\apikey.inc}

procedure TfrmTestApiWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Fgooglespeech);
  FreeAndNil(FSettings);
end;

procedure TfrmTestApiWindow.FormCreate(Sender: TObject);
begin
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  Fgooglespeech := TGoogleSpeechService.Create(Self, google_clientid, google_clientsecret,'ADUG Demo', '', FSettings);
end;

procedure TfrmTestApiWindow.Button1Click(Sender: TObject);
begin
  ListOpenAIModels;
  ListReplicateImageGenerators;
  TestHunggingFaceLLM;
  ListAzureLLMModels;
  ListGoogleLLMModels;
  ListAnthropicModels;
  ListElevenLabsVoices;
  ListAmazonVoices;
  ListOpenAIVoices;
  ListWindowsVoices;
  ListMicrosoftVoices;
  ListGoogleVoices;
  ListMicrosoftTranslateLanguages;
  ListAmazonTranslationLanguages;
end;

procedure TfrmTestApiWindow.Button2Click(Sender: TObject);
begin
  Fgooglespeech.Authenticate;
end;

procedure TfrmTestApiWindow.Button3Click(Sender: TObject);
var
  voice : TVoiceInfo;
  msvoice : TMicrosoftCognitiveService;
  ticks : UInt64;
begin
  TestOpenAITTS;
  TestElevenLabsTTS;
  TestAmazonPolly;
  TestWindowsVoice;

  Memo1.Lines.Add('======== Microsoft Voices');
  msvoice := TMicrosoftCognitiveService.Create(Self, ms_cognative_service_resource_key, 'australiaeast.tts.speech.microsoft.com');
  try
    voice := msvoice.Voices[1];
    Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    msvoice.PlayText('Hello from voice ' + voice.VoiceName,voice.VoiceId);
    ticks := GetTickCount64;
    repeat
      Application.ProcessMessages;
    until (GetTickCount64 - ticks) > 10000;
  finally
    FreeAndNil(msvoice);
  end;

  Memo1.Lines.Add('======== Google Voices');
  voice := Fgooglespeech.Voices[1];
  Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
  Fgooglespeech.PlayText('Hello from voice ' + voice.VoiceName,voice.VoiceId);
  ticks := GetTickCount64;
  repeat
    Application.ProcessMessages;
  until (GetTickCount64 - ticks) > 10000;
end;

procedure TfrmTestApiWindow.Button4Click(Sender: TObject);
begin
  TestConquiVoices;
end;

procedure TfrmTestApiWindow.Button5Click(Sender: TObject);
var
  openAIVision : TOpenAI;
  config : TChatSettings;
  AMessages: TObjectList<TChatMessage>;
  MessageVision : TChatVisionMessage;
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

procedure TfrmTestApiWindow.Button6Click(Sender: TObject);
begin
  TestGroqLLM;
end;

function TfrmTestApiWindow.GetWeather(const state: string; const location: string): string;
begin
  ShowMessage('GetWeather for ' + location + ', ' + state);
  Result := 'The Temperature is 28 degrees';
end;


procedure TfrmTestApiWindow.Button7Click(Sender: TObject);
var
  openAI : TOpenAI;
  messages : TObjectList<TChatMessage>;
  msg : TChatMessage;
  settings : TChatSettings;
  answer : string;
begin
  openAI := TOpenAI.Create(chatgpt_apikey);
  try
    openAI.Functions.RegisterFunction(@TfrmTestApiWindow.GetWeather, Self);

    settings.model := 'gpt-4o';
    settings.json_mode := False;
    settings.max_tokens := 4096;
    messages := TObjectList<TChatMessage>.Create;
    msg := TChatMessage.Create;
    msg.Role := 'user';
    msg.Content := 'What is the weather for Bendigo, Victoria?';
    messages.Add(msg);
    answer := openAI.ChatCompletion(settings, messages).Content;
    Memo1.Lines.Add('Answer : ' + answer);
  finally
    FreeAndNil(messages);
    FreeAndNil(openAI);
  end;
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
