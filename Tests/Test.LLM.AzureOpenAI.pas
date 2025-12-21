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
    FDeploymentId: string;
    function CreateLLM: TBaseLLM; override;
    function DefaultModel: string; override;
    function DefaultVisionModel: string; override;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
  end;

implementation

function TAzureOpenAILLMTests.DefaultModel: string;
begin
  Result := 'gpt-4o';
end;

function TAzureOpenAILLMTests.DefaultVisionModel: string;
begin
  Result := 'gpt-4o';
end;

procedure TAzureOpenAILLMTests.Setup;
begin
  FDeploymentId := 'gpt-4o';
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
    FDeploymentId
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TAzureOpenAILLMTests);

end.

