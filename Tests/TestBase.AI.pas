unit TestBase.AI;

interface

uses
  DUnitX.TestFramework,
  System.IOUtils,
  System.SysUtils;

type
  TBaseAITest = class abstract
  protected
    function DataPath: string;
    procedure RequireLiveTests;
  end;

implementation

function TBaseAITest.DataPath: string;
begin
  Result := GetEnvironmentVariable('DataPath');
  if Result.IsEmpty then
    Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\..\Data');

end;

procedure TBaseAITest.RequireLiveTests;
begin
//  if GetEnvironmentVariable('RUN_LIVE_API_TESTS') <> '1' then
//    Assert.Ignore('Live API tests disabled');
end;

end.

