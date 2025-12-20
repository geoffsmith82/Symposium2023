unit TestBase.AI;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils;

type
  TBaseAITest = class abstract
  protected
    procedure RequireLiveTests;
  end;

implementation

procedure TBaseAITest.RequireLiveTests;
begin
//  if GetEnvironmentVariable('RUN_LIVE_API_TESTS') <> '1' then
//    Assert.Ignore('Live API tests disabled');
end;

end.

