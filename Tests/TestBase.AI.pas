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
  public
    class procedure EnableRESTDebug; static;
    class procedure DisableRESTDebug; static;
  end;

implementation

uses
  REST.Client,
  REST.Types,
  uLLM;

const
  RESTDEBUG_PROXY_HOST = 'localhost';
  RESTDEBUG_PROXY_PORT = 8888;
  RESTDEBUG_HEADER_ID = 'X-Debug-Id';
  RESTDEBUG_HEADER_TARGET = 'X-Debug-Target';

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

class procedure TBaseAITest.EnableRESTDebug;
begin
  OnRESTClientCreated := procedure(AClient: TObject; ARequest: TObject)
    var
      LClient: TRESTClient;
      LRequest: TRESTRequest;
      LOriginalBaseURL: string;
      LDebugId: string;
    begin
      if (AClient is TRESTClient) and (ARequest is TRESTRequest) then
      begin
        LClient := TRESTClient(AClient);
        LRequest := TRESTRequest(ARequest);

        LOriginalBaseURL := LClient.BaseURL;
        LClient.BaseURL := 'http://' + RESTDEBUG_PROXY_HOST + ':' + RESTDEBUG_PROXY_PORT.ToString;

        LDebugId := TGUID.NewGuid.ToString.Trim(['{', '}']).ToLower;
        LRequest.AddParameter(RESTDEBUG_HEADER_ID, LDebugId,
          TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);
        LRequest.AddParameter(RESTDEBUG_HEADER_TARGET, LOriginalBaseURL,
          TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);
      end;
    end;
end;

class procedure TBaseAITest.DisableRESTDebug;
begin
  OnRESTClientCreated := nil;
end;

end.

