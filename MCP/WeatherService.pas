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
  MCP.Resources,
  MCP.Prompts,
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
    FResourceManager: TMCPResourceManager;
    FPromptManager: TMCPPromptManager;

    const CACHE_DURATION = 30; // Minutes


    // IMCPService implementation
    function GetServiceName: string;
    function GetServiceVersion: string;

    // Custom resources and prompts implementations
    procedure InitializeResources;
    procedure InitializePrompts;

    // Weather-related methods
    procedure CleanExpiredCache;
    function GetContextualWeather(const Location: string): TWeatherData;
    // Override base methods
    function GetResources: TJSONArray; override;
    function GetPrompts: TJSONArray; override;

  public
    constructor Create(const APIKey: string);
    destructor Destroy; override;

    // Tool methods (will be discovered via RTTI)
    [MCPTool('getCurrentWeather', 'Gets current weather conditions for a specified location')]
    function GetCurrentWeather([MCPParameter('The name of the location (city, region, etc.) to get weather for')] const Location: string): TWeatherData;

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

  // Create resource and prompt managers
  FResourceManager := TMCPResourceManager.Create;
  FPromptManager := TMCPPromptManager.Create;

  // Initialize resources and prompts
  InitializeResources;
  InitializePrompts;
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

procedure TWeatherMCPService.InitializeResources;
var
  Resource: TMCPResource;
begin
  // Weather data sources
  Resource := FResourceManager.AddResource(
    'weather-data-sources',
    'Weather Data Sources',
    'Information about the sources of weather data used by this service',
    rtDocument
  );
  Resource.URL := 'https://www.weatherapi.com/docs/';
  Resource.AddTag('documentation');
  Resource.AddTag('reference');
  Resource.AddMetadata('provider', TJSONString.Create('WeatherAPI.com'));

  // Weather icons
  Resource := FResourceManager.AddResource(
    'weather-icons',
    'Weather Icons',
    'Icons used to represent different weather conditions',
    rtImage
  );
  Resource.URL := 'https://www.weatherapi.com/docs/weather_conditions.json';
  Resource.ContentType := 'application/json';
  Resource.AddTag('icons');
  Resource.AddTag('visual');

  // Weather maps
  Resource := FResourceManager.AddResource(
    'weather-maps',
    'Weather Maps',
    'Interactive maps showing weather patterns and forecasts',
    rtLink
  );
  Resource.URL := 'https://www.weatherapi.com/weather/maps.aspx';
  Resource.AddTag('maps');
  Resource.AddTag('interactive');
end;

procedure TWeatherMCPService.InitializePrompts;
var
  Prompt: TMCPPrompt;
begin
  // Weather forecast prompt
  Prompt := FPromptManager.AddPrompt(
    'weather-forecast',
    'Weather Forecast',
    'Generate a weather forecast description',
    'Generate a friendly, conversational weather forecast for {location} with the following information:' + sLineBreak +
    'Temperature: {temperature}°C' + sLineBreak +
    'Condition: {condition}' + sLineBreak +
    'Humidity: {humidity}%' + sLineBreak +
    'Wind Speed: {wind_speed} km/h' + sLineBreak + sLineBreak +
    'Keep it concise, informative, and helpful for someone planning their day.'
  );

  Prompt.AddVariable('location', 'The name of the location');
  Prompt.AddVariable('temperature', 'Current temperature in Celsius');
  Prompt.AddVariable('condition', 'Weather condition description');
  Prompt.AddVariable('humidity', 'Current humidity percentage');
  Prompt.AddVariable('wind_speed', 'Wind speed in km/h');

  Prompt.AddTag('forecast');
  Prompt.AddTag('current');

  // Validate that all variables are used in the prompt text
  if not Prompt.ValidatePromptText then
    raise Exception.Create('Weather forecast prompt validation failed');

  // Weather alert prompt
  Prompt := FPromptManager.AddPrompt(
    'weather-alert',
    'Weather Alert',
    'Generate a weather alert or warning message',
    'WEATHER ALERT for {location}:' + sLineBreak + sLineBreak +
    '{alert_type} alert issued for your area.' + sLineBreak +
    'Current conditions: {condition}, {temperature}°C' + sLineBreak +
    'Expected impact: {impact_description}' + sLineBreak + sLineBreak +
    'Safety recommendations:' + sLineBreak +
    '{safety_recommendations}' + sLineBreak + sLineBreak +
    'Stay safe and monitor local updates for more information.'
  );

  Prompt.AddVariable('location', 'The name of the location');
  Prompt.AddVariable('alert_type', 'Type of weather alert (e.g., Storm, Flood, Heat Wave)');
  Prompt.AddVariable('condition', 'Current weather condition');
  Prompt.AddVariable('temperature', 'Current temperature in Celsius');
  Prompt.AddVariable('impact_description', 'Description of potential impacts');
  Prompt.AddVariable('safety_recommendations', 'Safety recommendations for this weather event');

  Prompt.AddTag('alert');
  Prompt.AddTag('warning');
  Prompt.AddTag('safety');

  // Validate that all variables are used in the prompt text
  if not Prompt.ValidatePromptText then
    raise Exception.Create('Weather alert prompt validation failed');

  // Weather travel advice prompt
  Prompt := FPromptManager.AddPrompt(
    'travel-advice',
    'Weather Travel Advice',
    'Generate travel recommendations based on weather conditions',
    'TRAVEL ADVISORY for {location}:' + sLineBreak + sLineBreak +
    'Current weather: {condition}, {temperature}°C' + sLineBreak +
    'Visibility: {visibility}' + sLineBreak +
    'Precipitation: {precipitation}' + sLineBreak + sLineBreak +
    'Travel Recommendations:' + sLineBreak +
    '{travel_recommendations}' + sLineBreak + sLineBreak +
    'Road Conditions:' + sLineBreak +
    '{road_conditions}' + sLineBreak + sLineBreak +
    'Always check local forecasts before traveling and adjust plans as needed.'
  );

  Prompt.AddVariable('location', 'The name of the location');
  Prompt.AddVariable('condition', 'Current weather condition');
  Prompt.AddVariable('temperature', 'Current temperature in Celsius');
  Prompt.AddVariable('visibility', 'Visibility conditions (good, moderate, poor, etc.)');
  Prompt.AddVariable('precipitation', 'Current precipitation level or forecast');
  Prompt.AddVariable('travel_recommendations', 'General travel recommendations based on conditions');
  Prompt.AddVariable('road_conditions', 'Description of expected road conditions');

  Prompt.AddTag('travel');
  Prompt.AddTag('advisory');

  // Validate that all variables are used in the prompt text
  if not Prompt.ValidatePromptText then
    raise Exception.Create('Weather travel advice prompt validation failed');
end;

function TWeatherMCPService.GetResources: TJSONArray;
begin
  // Return resources using the resource manager
  Result := FResourceManager.ToJSON;
end;

function TWeatherMCPService.GetPrompts: TJSONArray;
begin
  // Return prompts using the prompt manager
  Result := FPromptManager.ToJSON;
end;

function TWeatherMCPService.GetCurrentWeather(const Location: string): TWeatherData;
begin
  // Delegate to contextual weather function
  Result := GetContextualWeather(Location);
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
