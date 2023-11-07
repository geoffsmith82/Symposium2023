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
  frmTestAPI in 'frmTestAPI.pas' {frmTestApiWindow},
  uAmazon.Polly in '..\Libs\TextToSpeech\uAmazon.Polly.pas',
  uTTS in '..\Libs\TextToSpeech\uTTS.pas',
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
  uLLM in '..\Libs\LLM\uLLM.pas',
  uAzureGPT in '..\Libs\LLM\uAzureGPT.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  OpenAI in '..\Libs\LLM\OpenAI.pas',
  uGoogle.PaLM in '..\Libs\LLM\uGoogle.PaLM.pas',
  uAnthropic in '..\Libs\LLM\uAnthropic.pas',
  uReplicate.LLM in '..\Libs\LLM\uReplicate.LLM.pas',
  uHuggingFace.LLM in '..\Libs\LLM\uHuggingFace.LLM.pas',
  uImageGeneration.Replicate in '..\Libs\ImageGeneration\uImageGeneration.Replicate.pas',
  uImageGeneration in '..\Libs\ImageGeneration\uImageGeneration.pas',
  uImageGeneration.OpenAI in '..\Libs\ImageGeneration\uImageGeneration.OpenAI.pas',
  uDALLe2.DTO in '..\Libs\ImageGeneration\uDALLe2.DTO.pas',
  uEmbeddings in '..\Libs\Embeddings\uEmbeddings.pas',
  uEmbeddings.OpenAI in '..\Libs\Embeddings\uEmbeddings.OpenAI.pas',
  uEmbeddings.Microsoft.OpenAI in '..\Libs\Embeddings\uEmbeddings.Microsoft.OpenAI.pas',
  uCoqui.TTS in '..\Libs\TextToSpeech\uCoqui.TTS.pas',
  uOpenAI.TextToSpeech in '..\Libs\TextToSpeech\uOpenAI.TextToSpeech.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestApiWindow, frmTestApiWindow);
  Application.Run;
end.

