unit Test.LLM.AzureOpenAI;

interface

uses
  DUnitX.TestFramework,
  uLLM,
  uLLM.Azure,
  ApiKeyStore,
  TestBase.LLM;

type
  [TestFixture]
  TAzureOpenAILLMTests = class(TBaseLLMTests)
  protected
    function CreateLLM: TBaseLLM; override;
    function DefaultModel: string; override;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
  end;

implementation

function TAzureOpenAILLMTests.DefaultModel: string;
begin
  Result := 'FIXME_AZURE_DEPLOYMENT';
end;

procedure TAzureOpenAILLMTests.Setup;
begin
end;

procedure TAzureOpenAILLMTests.TearDown;
begin
end;

function TAzureOpenAILLMTests.CreateLLM: TBaseLLM;
var
  FKeys: TApiKeyStore;
begin
  FKeys := TApiKeyStore.GetInstance;
  Result := TMicrosoftOpenAI.Create(
    FKeys.LoadApiKey('AzureAPIKey'),
    FKeys.LoadSetting('AzureOpenAIEndpoint'),
    DefaultModel
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TAzureOpenAILLMTests);

end.

