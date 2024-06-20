unit Controller.Weather;

interface

uses
    SysUtils
  , System.Generics.Collections
  , System.DateUtils
  , System.Classes
  , MVCFramework
  , MVCFramework.Commons
  , MVCFramework.DataSet.Utils
  , MVCFramework.Serializer.Commons
  , MVCFramework.Swagger.Commons
  ;

type
  [MVCPath('/api')]
  TWeatherController = class (TMVCController)
    // Get current weather
    [MVCPath('/current-weather/($state)/($location)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCurrentWeather;
  end;

implementation

uses
  udmWeather,
  Controller.Weather.DataObjects,
  uXMLBOMPrecis;


{ TWeatherController }

procedure TWeatherController.GetCurrentWeather;
var
  weather : TdmWeather;
  location, state: string;
  data : IXMLAreaType;
  responseData : TCurrentWeatherResponseObject;
begin
  Context.Request.SegmentParam('state', state);
  Context.Request.SegmentParam('location', location);
  weather := TdmWeather.Create(nil);
  try
    data := weather.Getproduct(location, state);
    responseData := TCurrentWeatherResponseObject.Create;
    responseData.report := data.XML;
    Render(responseData, False);
  finally
    FreeAndNil(responseData);
    FreeAndNil(weather);
  end;
end;

end.
