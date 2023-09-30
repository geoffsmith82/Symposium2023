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
  uGoogleSpeech
  ;

type
  TfrmTestApiWindow = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    Fgooglespeech : TGoogleSpeechService;
  public
    { Public declarations }
  end;

var
  frmTestApiWindow: TfrmTestApiWindow;

const AzureOpenAIEndpoint = 'https://symposiumdemoopenai.openai.azure.com';

implementation

{$R *.dfm}

uses
  uElevenLabs.REST,
  uAmazon.Polly,
  uBaseSpeech,
  uWindows.Engine,
  LanguageCodes,
  uBaseTranslate,
  uMicrosoft.Cognitive.REST,
  uMicrosoft.Translate,
  uAmazon.Translate,
  OpenAI,
  uAzureGPT,
  uGoogle.PaLM,
  uAnthropic,
  uHuggingFace.LLM,
  uReplicate.LLM,
  uLLM,
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
var
  elevenlabs : TElevenLabsService;
  polly: TAmazonPollyService;
  voice : TVoiceInfo;
  mswindows : TWindowsSpeechService;
  msvoice : TMicrosoftCognitiveService;
  msTranslate : TMicrosoftTranslate;
  amazonEngine : TAmazonTranslate;
  lang : TLanguageInfo;
  langlist : TObjectList<TLanguageInfo>;
  openAI: TOpenAI;
  model: TBaseModelInfo;
  modelObj : TBaseModelInfo;
  modelImg : TImageModelInfo;
  palm : TGooglePaLM;
  anthropic : TAnthropic;
  microsoftOpenAI : TMicrosoftOpenAI;
  replicate: TReplicateLLM;
  huggingFace: THuggingFaceLLM;
  imageGenReplicate : TImageGenerationReplicate;
  answer : string;
  imgs : TGeneratedImagesClass;
  i : Integer;
begin
  Memo1.Lines.Add('======== Model OpenAI');
  openAI := TOpenAI.Create(chatgpt_apikey);
  try
    for modelObj in openAI.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelObj.modelName);
    end;
  finally
    FreeAndNil(openAI);
  end;

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

  Memo1.Lines.Add('======== Model Replicate ImageGen');
  imageGenReplicate := TImageGenerationReplicate.Create(Replicate_APIKey);
  try
    for modelImg in imageGenReplicate.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelImg.modelName + ' ' + modelImg.version);
    end;

    imgs := imageGenReplicate.Generate('A bird sitting on a branch in a tree', 1, TDALLESIZE.DALLE1024, 'stable-diffusion');
    try
      for I := 0 to length(imgs.data) - 1 do
      begin
        Memo1.Lines.Add('ImageURL: ' + imgs.data[i].url);
      end;
    finally
      FreeAndNil(imgs);
    end;

  finally
    FreeAndNil(imageGenReplicate);
  end;

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

  Memo1.Lines.Add('======== Microsoft OpenAI');
  microsoftOpenAI := TMicrosoftOpenAI.Create(AzureAPIKey, AzureOpenAIEndpoint);
  try
    for modelObj in microsoftOpenAI.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelObj.modelName);
    end;
  finally
    FreeAndNil(microsoftOpenAI);
  end;

  Memo1.Lines.Add('======== Model Google');
  palm := TGooglePaLM.Create(google_makersuite);
  try
    for modelObj in palm.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelObj.modelName);
    end;
  finally
    FreeAndNil(palm);
  end;

  Memo1.Lines.Add('======== Model Anthropic');
  anthropic := TAnthropic.Create(Claude_APIKey);
  try
    for modelObj in anthropic.ModelInfo do
    begin
      Memo1.Lines.Add('Model:' + modelObj.modelName);
    end;
  finally
    FreeAndNil(anthropic);
  end;

  Memo1.Lines.Add('======== ElevenLabs Voices');
  elevenlabs := TElevenLabsService.Create(Self, ElevenLabsAPIKey);
  try
    for voice in elevenlabs.Voices do
    begin
      Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    end;
  finally
    FreeAndNil(elevenlabs);
  end;

  Memo1.Lines.Add('======== Amazon Polly Voices');
  polly := TAmazonPollyService.Create(Self, AWSAccessKey, AWSSecretKey, AWSRegion);
  try
    for voice in polly.Voices do
    begin
      Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    end;
  finally
    FreeAndNil(polly);
  end;

  Memo1.Lines.Add('======== Windows Voices');
  mswindows := TWindowsSpeechService.Create(Self);
  try
    for voice in mswindows.Voices do
    begin
      Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    end;
  finally
    FreeAndNil(mswindows);
  end;

  Memo1.Lines.Add('======== Microsoft Voices');
  msvoice := TMicrosoftCognitiveService.Create(Self, ms_cognative_service_resource_key, 'australiaeast.tts.speech.microsoft.com');
  try
    for voice in msvoice.Voices do
    begin
      Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    end;
  finally
    FreeAndNil(msvoice);
  end;

  Memo1.Lines.Add('======== Google Voices');
  for voice in Fgooglespeech.Voices do
  begin
    Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
  end;

  msTranslate := TMicrosoftTranslate.Create(ms_translate_key,'https://api.cognitive.microsofttranslator.com/');
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

  amazonEngine := TAmazonTranslate.Create(AWSAccessKey, AWSSecretkey, AWSRegion);
  try
    Memo1.Lines.Add('======== Amazon Translate');
    langlist := amazonEngine.FromLanguages;
    for lang in langlist do
    begin
      Memo1.Lines.Add('lang= ' + lang.LanguageCode + ' | ' + lang.LanguageName);
    end;

    langlist := amazonEngine.ToLanguages;
    for lang in langlist do
    begin
      Memo1.Lines.Add('lang= ' + lang.LanguageCode + ' | ' + lang.LanguageName);
    end;

  finally
    FreeAndNil(amazonEngine);
  end;

end;

procedure TfrmTestApiWindow.Button2Click(Sender: TObject);
begin
  Fgooglespeech.Authenticate;
end;

procedure TfrmTestApiWindow.Button3Click(Sender: TObject);
var
  elevenlabs : TElevenLabsService;
  polly: TAmazonPollyService;
  voice : TVoiceInfo;
  mswindows : TWindowsSpeechService;
  msvoice : TMicrosoftCognitiveService;
  ticks : UInt64;
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

end.
