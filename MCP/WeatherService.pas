unit WeatherService;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.Rtti,
  MCP.BaseService,
  MCP.Attributes,
  MCP.ServiceInterface,
  WeatherAPI;

type
  /// <summary>
  /// Weather MCP Service implementation
  /// </summary>
  TWeatherMCPService = class(TMCPBaseService, IMCPService)
  private
    // Weather-specific properties
    FWeatherAPI: TWeatherAPI;
    FCachedWeatherData: TDictionary<string, TWeatherData>;
    FCacheExpiry: TDictionary<string, TDateTime>;
    const CACHE_DURATION = 30; // Minutes

    // IMCPService implementation
    function GetServiceName: string;
    function GetServiceVersion: string;

    // Weather-related methods
    procedure CleanExpiredCache;
    function GetContextualWeather(const Location: string): TWeatherData;
  public
    constructor Create(const APIKey: string);
    destructor Destroy; override;

    // Tool methods (will be discovered via RTTI)
    [MCPTool('getCurrentWeather', 'Gets current weather conditions for a specified location')]
    function GetCurrentWeather([MCPParameter('The name of the location (city, region, etc.) to get weather for')] const Location: string): TWeatherData;

    [MCPTool('enrichContext', 'Adds weather information to the conversation context')]
    function EnrichContext([MCPParameter('The name of the location to get weather context for')] const Location: string): TJSONArray;
  end;

implementation

{ TWeatherMCPService }

constructor TWeatherMCPService.Create(const APIKey: string);
begin
  inherited Create(
    'WeatherMCPService',
    '1.0',
    'This service provides weather information for specified locations. ' +
    'You can use the "getCurrentWeather" tool to get current weather conditions ' +
    'for a location by passing the location name as a parameter.'
  );

  FWeatherAPI := TWeatherAPI.Create(APIKey);
  FCachedWeatherData := TDictionary<string, TWeatherData>.Create;
  FCacheExpiry := TDictionary<string, TDateTime>.Create;
end;

destructor TWeatherMCPService.Destroy;
var
  WeatherData: TWeatherData;
begin
  for WeatherData in FCachedWeatherData.Values do
    WeatherData.Free;

  FCachedWeatherData.Free;
  FCacheExpiry.Free;
  FWeatherAPI.Free;

  inherited;
end;

function TWeatherMCPService.GetServiceName: string;
begin
  Result := FServiceName;
end;

function TWeatherMCPService.GetServiceVersion: string;
begin
  Result := FServiceVersion;
end;

function TWeatherMCPService.GetCurrentWeather(const Location: string): TWeatherData;
begin
  // Delegate to contextual weather function
  Result := GetContextualWeather(Location);
end;

function TWeatherMCPService.EnrichContext(const Location: string): TJSONArray;
var
  WeatherData: TWeatherData;
begin
  // Get weather data
  WeatherData := GetContextualWeather(Location);

  // Create context data array
  Result := TJSONArray.Create;

  // Add weather information as context lines
  Result.Add(Format('Current weather for %s:', [WeatherData.Location]));
  Result.Add(Format('Temperature: %.1f°C', [WeatherData.Temperature]));
  Result.Add(Format('Condition: %s', [WeatherData.Condition]));
  Result.Add(Format('Humidity: %d%%', [WeatherData.Humidity]));
  Result.Add(Format('Wind Speed: %.1f km/h', [WeatherData.WindSpeed]));
  Result.Add(Format('Last Updated: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', WeatherData.LastUpdated)]));
end;

procedure TWeatherMCPService.CleanExpiredCache;
var
  Location: string;
  ExpiryTime: TDateTime;
  ExpiredLocations: TList<string>;
  WeatherData: TWeatherData;
begin
  ExpiredLocations := TList<string>.Create;
  try
    for Location in FCacheExpiry.Keys do
    begin
      if FCacheExpiry.TryGetValue(Location, ExpiryTime) then
      begin
        if Now > ExpiryTime then
          ExpiredLocations.Add(Location);
      end;
    end;

    for Location in ExpiredLocations do
    begin
      if FCachedWeatherData.TryGetValue(Location, WeatherData) then
      begin
        WeatherData.Free;
        FCachedWeatherData.Remove(Location);
      end;
      FCacheExpiry.Remove(Location);
    end;
  finally
    ExpiredLocations.Free;
  end;
end;

function TWeatherMCPService.GetContextualWeather(const Location: string): TWeatherData;
var
  ExpiryTime: TDateTime;
begin
  // Clean expired cache entries
  CleanExpiredCache;

  // Check if we have a valid cached entry
  if FCachedWeatherData.TryGetValue(Location, Result) and
     FCacheExpiry.TryGetValue(Location, ExpiryTime) and
     (Now < ExpiryTime) then
  begin
    // Return cached data
    Exit;
  end;

  // Get fresh data
  Result := FWeatherAPI.GetCurrentWeather(Location);

  // Cache the result
  if FCachedWeatherData.ContainsKey(Location) then
  begin
    FCachedWeatherData[Location].Free;
    FCachedWeatherData[Location] := Result;
  end
  else
    FCachedWeatherData.Add(Location, Result);

  // Set cache expiry
  if FCacheExpiry.ContainsKey(Location) then
    FCacheExpiry[Location] := Now + (CACHE_DURATION / (24 * 60))
  else
    FCacheExpiry.Add(Location, Now + (CACHE_DURATION / (24 * 60)));
end;

end.
