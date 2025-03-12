unit WeatherAPI;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Net.HttpClient,
  System.NetEncoding;

type
  /// <summary>
  /// Weather data container
  /// </summary>
  TWeatherData = class
  private
    FTemperature: Double;
    FCondition: string;
    FHumidity: Integer;
    FWindSpeed: Double;
    FLocation: string;
    FLastUpdated: TDateTime;
  public
    property Temperature: Double read FTemperature write FTemperature;
    property Condition: string read FCondition write FCondition;
    property Humidity: Integer read FHumidity write FHumidity;
    property WindSpeed: Double read FWindSpeed write FWindSpeed;
    property Location: string read FLocation write FLocation;
    property LastUpdated: TDateTime read FLastUpdated write FLastUpdated;

    function ToJSON: TJSONObject;
  end;

  /// <summary>
  /// Weather API client
  /// </summary>
  TWeatherAPI = class
  private
    FAPIKey: string;
    FBaseURL: string;
    FClient: THTTPClient;
    function ParseWeatherResponse(const ResponseJSON: string): TWeatherData;
  public
    constructor Create(const APIKey: string);
    destructor Destroy; override;
    function GetCurrentWeather(const Location: string): TWeatherData;
  end;

implementation

{ TWeatherData }

function TWeatherData.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;

  // Add all properties as JSON pairs
  Result.AddPair('location', Location);
  Result.AddPair('temperature', TJSONNumber.Create(Temperature));
  Result.AddPair('condition', Condition);
  Result.AddPair('humidity', TJSONNumber.Create(Humidity));
  Result.AddPair('wind_speed', TJSONNumber.Create(WindSpeed));
  Result.AddPair('last_updated', FormatDateTime('yyyy-mm-dd hh:nn:ss', LastUpdated));

  // Format human-readable text for display
  var ReadableText := Format(
    'Current weather for %s:%s' +
    'Temperature: %.1f°C%s' +
    'Condition: %s%s' +
    'Humidity: %d%%%s' +
    'Wind Speed: %.1f km/h%s' +
    'Last Updated: %s',
    [
      Location, sLineBreak,
      Temperature, sLineBreak,
      Condition, sLineBreak,
      Humidity, sLineBreak,
      WindSpeed, sLineBreak,
      FormatDateTime('yyyy-mm-dd hh:nn:ss', LastUpdated)
    ]
  );

  // Add readable text property for display purposes
  Result.AddPair('display_text', ReadableText);
end;

{ TWeatherAPI }

constructor TWeatherAPI.Create(const APIKey: string);
begin
  inherited Create;
  FAPIKey := APIKey;
  FBaseURL := 'https://api.weatherapi.com/v1';
  FClient := THTTPClient.Create;
end;

destructor TWeatherAPI.Destroy;
begin
  FClient.Free;
  inherited;
end;

function TWeatherAPI.GetCurrentWeather(const Location: string): TWeatherData;
var
  Response: IHTTPResponse;
  URL: string;
begin
  URL := Format('%s/current.json?key=%s&q=%s', [FBaseURL, FAPIKey, TNetEncoding.URL.Encode(Location)]);

  try
    Response := FClient.Get(URL);

    if Response.StatusCode = 200 then
      Result := ParseWeatherResponse(Response.ContentAsString)
    else
      raise Exception.Create(
        Format('Weather API error: %d - %s', [Response.StatusCode, Response.ContentAsString])
      );
  except
    on E: Exception do
    begin
      // Create empty weather data with error information
      Result := TWeatherData.Create;
      Result.Condition := 'Error: ' + E.Message;
    end;
  end;
end;

function TWeatherAPI.ParseWeatherResponse(const ResponseJSON: string): TWeatherData;
var
  JSONObj, CurrentObj, ConditionObj, LocationObj: TJSONObject;
begin
  Result := TWeatherData.Create;

  JSONObj := TJSONObject.ParseJSONValue(ResponseJSON) as TJSONObject;
  try
    CurrentObj := JSONObj.GetValue<TJSONObject>('current');
    LocationObj := JSONObj.GetValue<TJSONObject>('location');
    ConditionObj := CurrentObj.GetValue<TJSONObject>('condition');

    Result.Temperature := CurrentObj.GetValue<Double>('temp_c');
    Result.Condition := ConditionObj.GetValue<string>('text');
    Result.Humidity := CurrentObj.GetValue<Integer>('humidity');
    Result.WindSpeed := CurrentObj.GetValue<Double>('wind_kph');
    Result.Location := LocationObj.GetValue<string>('name') + ', ' +
                      LocationObj.GetValue<string>('region') + ', ' +
                      LocationObj.GetValue<string>('country');
    Result.LastUpdated := Now;
  finally
    JSONObj.Free;
  end;
end;

end.
