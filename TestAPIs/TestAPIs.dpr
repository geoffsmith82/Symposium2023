program TestAPIs;

uses
  {$IFDEF EurekaLog}
  EMemLeaks,
  EResLeaks,
  EDebugExports,
  EDebugJCL,
  EFixSafeCallException,
  EMapWin32,
  EAppVCL,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  ExceptionLog7,
  {$ENDIF EurekaLog}
  Vcl.Forms,
  Unit6 in 'Unit6.pas' {Form6},
  uAmazon.Polly in '..\Libs\TextToSpeech\uAmazon.Polly.pas',
  uBaseSpeech in '..\Libs\TextToSpeech\uBaseSpeech.pas',
  uElevenLabs.REST in '..\Libs\TextToSpeech\uElevenLabs.REST.pas',
  uGoogleSpeech in '..\Libs\TextToSpeech\uGoogleSpeech.pas',
  uMicrosoft.Cognitive.REST in '..\Libs\TextToSpeech\uMicrosoft.Cognitive.REST.pas',
  uWindows.Engine in '..\Libs\TextToSpeech\uWindows.Engine.pas',
  SpeechLib_TLB in '..\Libs\TextToSpeech\SpeechLib_TLB.pas',
  uGoogleSpeech.DTO in '..\Libs\TextToSpeech\uGoogleSpeech.DTO.pas',
  uMicrosoft.Cognitive.Voices.DTO in '..\Libs\TextToSpeech\uMicrosoft.Cognitive.Voices.DTO.pas',
  uMicrosoft.Translate in '..\Libs\Translate\uMicrosoft.Translate.pas',
  uGoogle.Translate in '..\Libs\Translate\uGoogle.Translate.pas',
  uBaseTranslate in '..\Libs\Translate\uBaseTranslate.pas',
  uAmazon.Translate in '..\Libs\Translate\uAmazon.Translate.pas',
  LanguageCodes in '..\Libs\Translate\LanguageCodes.pas',
  uLLM in '..\Libs\uLLM.pas',
  uAzureGPT in '..\Libs\uAzureGPT.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  OpenAI in '..\Libs\OpenAI.pas',
  uGoogle.PaLM in '..\Libs\uGoogle.PaLM.pas',
  uAnthropic in '..\Libs\uAnthropic.pas',
  uDALLe2.DTO in '..\Libs\uDALLe2.DTO.pas',
  uReplicate.LLM in '..\Libs\uReplicate.LLM.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.

