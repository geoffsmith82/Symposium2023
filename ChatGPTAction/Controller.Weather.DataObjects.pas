unit Controller.Weather.DataObjects;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Swagger.Commons,
  MVCFramework.Serializer.Commons,
  System.SysUtils,
  System.Generics.Collections;

type
  TCurrentWeatherResponseObject = class
  private
    Freport: string;
  public
    [MVCNameAs('report')]
    property report: string read Freport write Freport;
  end;

implementation

end.
