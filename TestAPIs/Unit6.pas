unit Unit6;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
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
  TForm6 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    Fgooglespeech : TGoogleSpeechService;
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

uses
  uElevenLabs.REST,
  uAmazon.Polly,
  uBaseSpeech,
  uWindows.Engine,
  uMicrosoft.Cognitive.REST,
  uMicrosoft.Translate,
  uAmazon.Translate
  ;

{$I ..\Libs\apikey.inc}

procedure TForm6.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Fgooglespeech);
  FreeAndNil(FSettings);
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  Fgooglespeech := TGoogleSpeechService.Create(Self, google_clientid, google_clientsecret,'ADUG Demo', '', FSettings);
end;

procedure TForm6.Button1Click(Sender: TObject);
var
  elevenlabs : TElevenLabsService;
  polly: TAmazonPollyService;
  voice : TVoiceInfo;
  mswindows : TWindowsSpeechService;
  msvoice : TMicrosoftCognitiveService;
  msTranslate : TMicrosoftTranslate;
  amazonEngine : TAmazonTranslate;
  lang : string;
  langlist : TArray<string>;
begin
  elevenlabs := TElevenLabsService.Create(Self, ElevenLabsAPIKey);
  try
    for voice in elevenlabs.Voices do
    begin
      Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    end;
  finally
    FreeAndNil(elevenlabs);
  end;

  polly := TAmazonPollyService.Create(Self, AWSAccessKey, AWSSecretKey);
  try
    for voice in polly.Voices do
    begin
      Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    end;
  finally
    FreeAndNil(polly);
  end;

  mswindows := TWindowsSpeechService.Create(Self);
  try
    for voice in mswindows.Voices do
    begin
      Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    end;
  finally
    FreeAndNil(mswindows);
  end;


  msvoice := TMicrosoftCognitiveService.Create(Self, ms_cognative_service_resource_key, 'australiaeast.tts.speech.microsoft.com');
  try
    for voice in msvoice.Voices do
    begin
      Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
    end;
  finally
    FreeAndNil(msvoice);
  end;


  for voice in Fgooglespeech.Voices do
  begin
    Memo1.Lines.Add(voice.VoiceId + ' | ' + voice.VoiceName + ' | ' + voice.VoiceGender);
  end;

  msTranslate := TMicrosoftTranslate.Create(ms_translate_key,'https://api.cognitive.microsofttranslator.com/');
  try
    Memo1.Lines.Add('Microsoft Translate');
    for lang in msTranslate.FromLanguages do
    begin
      Memo1.Lines.Add('lang= ' + lang);
    end;

    for lang in msTranslate.ToLanguages do
    begin
      Memo1.Lines.Add('lang= ' + lang);
    end;

  finally
    FreeAndNil(msTranslate);
  end;

  amazonEngine := TAmazonTranslate.Create(AWSAccessKey, AWSSecretkey, '');
  try
    Memo1.Lines.Add('Amazon Translate');
    langlist := amazonEngine.FromLanguages;
    for lang in langlist do
    begin
      Memo1.Lines.Add('lang= ' + lang);
    end;

    langlist := amazonEngine.ToLanguages;
    for lang in langlist do
    begin
      Memo1.Lines.Add('lang= ' + lang);
    end;

  finally
    FreeAndNil(amazonEngine);
  end;

end;

procedure TForm6.Button2Click(Sender: TObject);
begin
  Fgooglespeech.Authenticate;
end;

end.
