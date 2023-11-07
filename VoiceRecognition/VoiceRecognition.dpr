program VoiceRecognition;

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
  ufrmVoiceRecognition in 'ufrmVoiceRecognition.pas' {frmVoiceRecognition},
  OpenAI in '..\Libs\LLM\OpenAI.pas',
  uWindows.Engine in '..\Libs\TextToSpeech\uWindows.Engine.pas',
  uMicrosoft.Cognitive.Voices.DTO in '..\Libs\TextToSpeech\uMicrosoft.Cognitive.Voices.DTO.pas',
  uMicrosoft.Cognitive.REST in '..\Libs\TextToSpeech\uMicrosoft.Cognitive.REST.pas',
  uGoogleSpeech in '..\Libs\TextToSpeech\uGoogleSpeech.pas',
  uGoogleSpeech.DTO in '..\Libs\TextToSpeech\uGoogleSpeech.DTO.pas',
  uOpenAI.TextToSpeech in '..\Libs\TextToSpeech\uOpenAI.TextToSpeech.pas',
  uElevenLabs.Voices.DTO in '..\Libs\TextToSpeech\uElevenLabs.Voices.DTO.pas',
  uElevenLabs.REST in '..\Libs\TextToSpeech\uElevenLabs.REST.pas',
  uTTS in '..\Libs\TextToSpeech\uTTS.pas',
  uAmazon.Polly in '..\Libs\TextToSpeech\uAmazon.Polly.pas',
  SpeechLib_TLB in '..\Libs\TextToSpeech\SpeechLib_TLB.pas',
  uAssemblyAI.SpeechToText in '..\Libs\SpeechToText\uAssemblyAI.SpeechToText.pas',
  uDeepGram.SpeechToText in '..\Libs\SpeechToText\uDeepGram.SpeechToText.pas',
  uBaseSpeechRecognition in '..\Libs\SpeechToText\uBaseSpeechRecognition.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  ufrmChatSession in 'ufrmChatSession.pas' {frmNewChatSession},
  uAzureGPT in '..\Libs\LLM\uAzureGPT.pas',
  uLLM in '..\Libs\LLM\uLLM.pas',
  uEngineManager in '..\Libs\uEngineManager.pas',
  BubbleText in '..\Libs\Bubble\BubbleText.pas',
  uAudioRecorder in '..\Libs\SpeechToText\uAudioRecorder.pas',
  uRevAI.SpeechToText in '..\Libs\SpeechToText\uRevAI.SpeechToText.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmVoiceRecognition, frmVoiceRecognition);
  Application.CreateForm(TfrmNewChatSession, frmNewChatSession);
  Application.Run;
end.
