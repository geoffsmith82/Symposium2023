unit Test.ImageGen.XAI;

interface

uses
  DUnitX.TestFramework,
  uImageGeneration,
  uImageGeneration.Replicate,
  uImageGeneration.XAI,
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
  Result := TImageGenerationXAI.Create(FKeys.LoadApiKey('X_AI'));
end;

initialization
  TDUnitX.RegisterTestFixture(TXAiImageGenTests);

end.

