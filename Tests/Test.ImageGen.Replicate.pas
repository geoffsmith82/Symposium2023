unit Test.ImageGen.Replicate;

interface

uses
  DUnitX.TestFramework,
  uImageGeneration,
  uImageGeneration.Replicate,
  ApiKeyStore,
  TestBase.ImageGen;

type
  [TestFixture]
  TReplicateImageGenTests = class(TBaseImageGenTests)
  protected
    function CreateImageGen: TBaseImageGeneration; override;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
  end;

implementation

procedure TReplicateImageGenTests.Setup;
begin
//  FKeys := TfrmApiKeyStore.Create(nil);
end;

procedure TReplicateImageGenTests.TearDown;
begin
//  FKeys.Free;
end;

function TReplicateImageGenTests.CreateImageGen: TBaseImageGeneration;
begin
  FKeys := TApiKeyStore.GetInstance;
  Result := TImageGenerationReplicate.Create(FKeys.LoadApiKey('Replicate_APIKey'));
end;

initialization
  TDUnitX.RegisterTestFixture(TReplicateImageGenTests);

end.

