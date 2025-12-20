unit Test.LLM.Anthropic;

interface

uses
  DUnitX.TestFramework,
  uLLM,
  uLLM.Anthropic,
  ApiKeyStore,
  TestBase.LLM;

type
  [TestFixture]
  TAnthropicLLMTests = class(TBaseLLMTests)
  protected
    function CreateLLM: TBaseLLM; override;
    function DefaultModel: string; override;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
  end;

implementation

function TAnthropicLLMTests.DefaultModel: string;
begin
  Result := 'claude-sonnet-4';
end;

procedure TAnthropicLLMTests.Setup;
begin
end;

procedure TAnthropicLLMTests.TearDown;
begin
end;

function TAnthropicLLMTests.CreateLLM: TBaseLLM;
var
  FKeys: TApiKeyStore;
begin
  FKeys := TApiKeyStore.GetInstance;
  Result := TAnthropic.Create(FKeys.LoadApiKey('Claude_APIKey'));
end;

initialization
  TDUnitX.RegisterTestFixture(TAnthropicLLMTests);

end.

