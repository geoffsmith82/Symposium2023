unit Test.LLM.DeepSeek;

interface

uses
  DUnitX.TestFramework,
  uLLM,
  uLLM.DeepSeek,
  ApiKeyStore,
  TestBase.LLM;

type
  [TestFixture]
  TDeepSeekLLMTests = class(TBaseLLMTests)
  protected
    function CreateLLM: TBaseLLM; override;
    function DefaultModel: string; override;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
  end;

implementation

function TDeepSeekLLMTests.DefaultModel: string;
begin
  Result := 'deepseek-chat';
end;

procedure TDeepSeekLLMTests.Setup;
begin
end;

procedure TDeepSeekLLMTests.TearDown;
begin
end;

function TDeepSeekLLMTests.CreateLLM: TBaseLLM;
var
  FKeys: TApiKeyStore;
begin
  FKeys := TApiKeyStore.GetInstance;
  Result := TDeepSeek.Create(FKeys.LoadApiKey('Deepseek_Key'));
end;

initialization
  TDUnitX.RegisterTestFixture(TDeepSeekLLMTests);

end.

