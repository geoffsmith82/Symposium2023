unit TestBase.ImageGen;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  uImageGeneration,
  ApiKeyStore,
  TestBase.AI;

type
  TBaseImageGenTests = class abstract(TBaseAITest)
  protected
    FKeys: TApiKeyStore;
    function CreateImageGen: TBaseImageGeneration; virtual; abstract;

  public
    [Test] procedure List_Models;
  end;

implementation

procedure TBaseImageGenTests.List_Models;
var
  ImageApi : TBaseImageGeneration;
begin
  RequireLiveTests;
  ImageApi := CreateImageGen;
  try
    Assert.IsTrue(ImageApi.ModelInfo.Count > 0);
  finally
    FreeAndNil(ImageApi);
  end;
end;

end.

