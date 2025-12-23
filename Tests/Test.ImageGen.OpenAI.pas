unit Test.ImageGen.OpenAI;

interface

uses
  DUnitX.TestFramework,
  uImageGeneration,
  uImageGeneration.OpenAI,
  ApiKeyStore,
  TestBase.ImageGen;

type
  [TestFixture]
  TXAiImageGenTests = class(TBaseImageGenTests)
  protected
    function CreateImageGen: TBaseImageGeneration; override;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
  end;

implementation

procedure TXAiImageGenTests.Setup;
begin

end;

procedure TXAiImageGenTests.TearDown;
begin

end;

function TXAiImageGenTests.CreateImageGen: TBaseImageGeneration;
begin
  FKeys := TApiKeyStore.GetInstance;
  Result := TImageGenerationOpenAI.Create(FKeys.LoadApiKey('chatgpt_apikey'));
end;

initialization
  TDUnitX.RegisterTestFixture(TXAiImageGenTests);

end.

