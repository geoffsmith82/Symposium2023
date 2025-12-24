unit Test.LLM.Mistral;

interface

uses
  DUnitX.TestFramework,
  uLLM,
  uLLM.Mistral,
  ApiKeyStore,
  TestBase.LLM;

type
  [TestFixture]
  TMistralLLMTests = class(TBaseLLMTests)
  protected
    function CreateLLM: TBaseLLM; override;
    function DefaultModel: string; override;
    function DefaultVisionModel: string; override;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
  end;

implementation

function TMistralLLMTests.DefaultModel: string;
begin
  Result := 'mistral-medium';
end;

function TMistralLLMTests.DefaultVisionModel: string;
begin
  Result := 'mistral-medium';
end;

procedure TMistralLLMTests.Setup;
begin
end;

procedure TMistralLLMTests.TearDown;
begin
end;

function TMistralLLMTests.CreateLLM: TBaseLLM;
var
  FKeys: TApiKeyStore;
begin
  FKeys := TApiKeyStore.GetInstance;
  Result := TMistral.Create(FKeys.LoadApiKey('Mistral_APIKey'));
end;

initialization
  TDUnitX.RegisterTestFixture(TMistralLLMTests);

end.

