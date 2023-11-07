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
  uTTS.Amazon.Polly in '..\Libs\TextToSpeech\uTTS.Amazon.Polly.pas',
  uTTS in '..\Libs\TextToSpeech\uTTS.pas',
  uTTS.ElevenLabs in '..\Libs\TextToSpeech\uTTS.ElevenLabs.pas',
  uTTS.GoogleSpeech in '..\Libs\TextToSpeech\uTTS.GoogleSpeech.pas',
  uTTS.Microsoft.Cognitive in '..\Libs\TextToSpeech\uTTS.Microsoft.Cognitive.pas',
  uTTS.Windows.Engine in '..\Libs\TextToSpeech\uTTS.Windows.Engine.pas',
  SpeechLib_TLB in '..\Libs\TextToSpeech\SpeechLib_TLB.pas',
  uTTS.GoogleSpeech.DTO in '..\Libs\TextToSpeech\uTTS.GoogleSpeech.DTO.pas',
  uTTS.Microsoft.Cognitive.Voices.DTO in '..\Libs\TextToSpeech\uTTS.Microsoft.Cognitive.Voices.DTO.pas',
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
  uTTS.Coqui in '..\Libs\TextToSpeech\uTTS.Coqui.pas',
  uTTS.OpenAI in '..\Libs\TextToSpeech\uTTS.OpenAI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestApiWindow, frmTestApiWindow);
  Application.Run;
end.

