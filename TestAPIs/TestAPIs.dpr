program TestAPIs;

uses
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
  uTranslate in '..\Libs\Translate\uTranslate.pas',
  uTranslate.Microsoft in '..\Libs\Translate\uTranslate.Microsoft.pas',
  uTranslate.Google in '..\Libs\Translate\uTranslate.Google.pas',
  uTranslate.Amazon in '..\Libs\Translate\uTranslate.Amazon.pas',
  uTranslate.LanguageCodes in '..\Libs\Translate\uTranslate.LanguageCodes.pas',
  uLLM in '..\Libs\LLM\uLLM.pas',
  uLLM.Azure in '..\Libs\LLM\uLLM.Azure.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  uLLM.OpenAI in '..\Libs\LLM\uLLM.OpenAI.pas',
  uLLM.Google.Gemini in '..\Libs\LLM\uLLM.Google.Gemini.pas',
  uLLM.Anthropic in '..\Libs\LLM\uLLM.Anthropic.pas',
  uLLM.Replicate in '..\Libs\LLM\uLLM.Replicate.pas',
  uLLM.HuggingFace in '..\Libs\LLM\uLLM.HuggingFace.pas',
  uLLM.Groq in '..\Libs\LLM\uLLM.Groq.pas',
  uImageGeneration.Replicate in '..\Libs\ImageGeneration\uImageGeneration.Replicate.pas',
  uImageGeneration in '..\Libs\ImageGeneration\uImageGeneration.pas',
  uImageGeneration.OpenAI in '..\Libs\ImageGeneration\uImageGeneration.OpenAI.pas',
  uDALLe2.DTO in '..\Libs\ImageGeneration\uDALLe2.DTO.pas',
  uEmbeddings in '..\Libs\Embeddings\uEmbeddings.pas',
  uEmbeddings.OpenAI in '..\Libs\Embeddings\uEmbeddings.OpenAI.pas',
  uEmbeddings.Microsoft.OpenAI in '..\Libs\Embeddings\uEmbeddings.Microsoft.OpenAI.pas',
  uTTS.Coqui in '..\Libs\TextToSpeech\uTTS.Coqui.pas',
  uTTS.OpenAI in '..\Libs\TextToSpeech\uTTS.OpenAI.pas',
  uLLM.Functions in '..\Libs\LLM\uLLM.Functions.pas',
  uAttributes in '..\Libs\LLM\uAttributes.pas',
  uLLM.X.Ai in '..\Libs\LLM\uLLM.X.Ai.pas',
  uLLM.OpenAI.Assistants in '..\Libs\LLM\uLLM.OpenAI.Assistants.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas',
  ApiKeyStore.Android in '..\Libs\ApiKeyStore\ApiKeyStore.Android.pas',
  uAPIKeyNameList in '..\Libs\ApiKeyStore\uAPIKeyNameList.pas',
  uLLM.Mistral in '..\Libs\LLM\uLLM.Mistral.pas',
  frmApiKeyStore in '..\Libs\ApiKeyStore\frmApiKeyStore.pas' {frmApiKeyStores};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestApiWindow, frmTestApiWindow);
  Application.Run;
end.

