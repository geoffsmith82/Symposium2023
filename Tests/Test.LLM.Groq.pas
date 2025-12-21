unit Test.LLM.Groq;

interface

uses
  DUnitX.TestFramework,
  uLLM,
  uLLM.Groq,
  ApiKeyStore,
  TestBase.LLM;

type
  [TestFixture]
  TGroqLLMTests = class(TBaseLLMTests)
  protected
    function CreateLLM: TBaseLLM; override;
    function DefaultModel: string; override;
    function DefaultVisionModel: string; override;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
  end;

implementation

function TGroqLLMTests.DefaultModel: string;
begin
  Result := 'openai/gpt-oss-120b';
end;

function TGroqLLMTests.DefaultVisionModel: string;
begin
  Result := 'meta-llama/llama-4-scout-17b-16e-instruct';
end;

procedure TGroqLLMTests.Setup;
begin

end;

procedure TGroqLLMTests.TearDown;
begin

end;

function TGroqLLMTests.CreateLLM: TBaseLLM;
begin
  FKeys := TApiKeyStore.GetInstance;
  Result := TGroqLLM.Create(FKeys.LoadApiKey('groq_apikey'));

end;

initialization
  TDUnitX.RegisterTestFixture(TGroqLLMTests);

end.

