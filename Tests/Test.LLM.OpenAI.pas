unit Test.LLM.OpenAI;

interface

uses
  DUnitX.TestFramework,
  uLLM,
  uLLM.OpenAI,
  ApiKeyStore,
  TestBase.LLM;

type
  [TestFixture]
  TOpenAILLMTests = class(TBaseLLMTests)
  protected
    function CreateLLM: TBaseLLM; override;
    function DefaultModel: string; override;
    function DefaultVisionModel: string; override;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
  end;

implementation

function TOpenAILLMTests.DefaultModel: string;
begin
  Result := 'gpt-5.2';
end;

function TOpenAILLMTests.DefaultVisionModel: string;
begin
  Result := 'gpt-4o';
end;

procedure TOpenAILLMTests.Setup;
begin

end;

procedure TOpenAILLMTests.TearDown;
begin

end;

function TOpenAILLMTests.CreateLLM: TBaseLLM;
begin
  FKeys := TApiKeyStore.GetInstance;
  Result := TOpenAI.Create(FKeys.LoadApiKey('chatgpt_apikey'));
end;

initialization
  TDUnitX.RegisterTestFixture(TOpenAILLMTests);

end.

