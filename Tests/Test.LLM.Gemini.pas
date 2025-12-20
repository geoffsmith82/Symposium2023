unit Test.LLM.Gemini;

interface

uses
  DUnitX.TestFramework,
  uLLM,
  uLLM.Google.Gemini,
  ApiKeyStore,
  TestBase.LLM;

type
  [TestFixture]
  TGeminiLLMTests = class(TBaseLLMTests)
  protected
    function CreateLLM: TBaseLLM; override;
    function DefaultModel: string; override;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
  end;

implementation

function TGeminiLLMTests.DefaultModel: string;
begin
  Result := 'FIXME_GEMINI_MODEL';
end;

procedure TGeminiLLMTests.Setup;
begin
end;

procedure TGeminiLLMTests.TearDown;
begin
end;

function TGeminiLLMTests.CreateLLM: TBaseLLM;
var
  FKeys: TApiKeyStore;
begin
  FKeys := TApiKeyStore.GetInstance;
  Result := TGemini.Create(FKeys.LoadApiKey('google_AI_APIKey'));
end;

initialization
  TDUnitX.RegisterTestFixture(TGeminiLLMTests);

end.

