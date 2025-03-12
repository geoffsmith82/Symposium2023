program WeatherMCPService;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Net.HttpClient,
  System.NetEncoding,
  Winapi.Windows,
  System.Generics.Collections,
  System.IOUtils,
  System.RTTI,
  BaseService in 'BaseService.pas',
  MCPAttributes in 'MCPAttributes.pas',
  MCPInterface in 'MCPInterface.pas',
  MCPMethodProcessor in 'MCPMethodProcessor.pas',
  WeatherApi in 'WeatherApi.pas',
  WeatherService in 'WeatherService.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas';

// Main program entry point
var
  MCPService: TWeatherMCPService;
  APIKeyStore: TApiKeyStore;
  APIKey: string;
begin
  try
    // Check for API key in command line arguments
    if ParamCount >= 1 then
      APIKey := ParamStr(1)
    else
    begin
      // If no API key provided, check for environment variable
      APIKey := GetEnvironmentVariable('WEATHER_API_KEY');
      if APIKey.IsEmpty then
      begin
        APIKeyStore := TApiKeyStore.GetInstance;
        APIKey := APIKeyStore.LoadApiKey('WEATHER_API_KEY');
      end;
    end;

    // Create and run the service
    MCPService := TWeatherMCPService.Create(APIKey);
    try
      MCPService.Run;
    finally
      MCPService.Free;
    end;
  except
    on E: Exception do
    begin
      var ErrorObj := TJSONObject.Create;
      try
        ErrorObj.AddPair('jsonrpc', '2.0');
        ErrorObj.AddPair('id', TJSONNull.Create);

        var Error := TJSONObject.Create;
        Error.AddPair('code', TJSONNumber.Create(-32603));
        Error.AddPair('message', 'Service initialization error: ' + E.Message);

        ErrorObj.AddPair('error', Error);

        WriteLn(ErrorObj.ToString);
      finally
        ErrorObj.Free;
      end;
    end;
  end;
end.
