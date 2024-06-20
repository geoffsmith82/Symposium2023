unit Controller.Personal.DataObjects;

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
  TCurrentTimeObjbect = class
  private
    FCurrentTime: string;

  public
    [MVCNameAs('currentTime')]
    property CurrentTime: string read FCurrentTime write FCurrentTime;
  end;


  TAlarmBaseObject = class
  private
    FDate : TDateTime;
    FDescription: string;
    procedure SetDateTime(const Value: string);
    function GetDateTime: string;
  public
    [MVCNameAs('time')]
    property Time: string read GetDateTime write SetDateTime;
    [MVCNameAs('description')]
    property Description: string read FDescription write FDescription;
    function AsDateTime: TDateTime;
  end;

  TAlarmUpdateObject = class(TAlarmBaseObject)
    [MVCNameAs('time')]
    property Time;
    [MVCNameAs('description')]
    property Description;
  end;

  TAlarmObject = class(TAlarmBaseObject)
  private
    FId : Integer;
  public
    [MVCNameAs('id')]
    property Id: Integer read FId write FId;
    [MVCNameAs('time')]
    property Time;
    [MVCNameAs('description')]
    property Description;
  end;

  TAlarmsResponse = class
  private
    FAlarms: TObjectList<TAlarmObject>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Alarms: TObjectList<TAlarmObject> read FAlarms write FAlarms;
  end;


implementation


{ TAlarmsResponse }

constructor TAlarmsResponse.Create;
begin
  FAlarms := TObjectList<TAlarmObject>.Create;
end;

destructor TAlarmsResponse.Destroy;
begin
  FreeAndNil(FAlarms);
  inherited;
end;

{ TAlarmObject }

function TAlarmBaseObject.AsDateTime: TDateTime;
begin
  Result := FDate;
end;

function TAlarmBaseObject.GetDateTime: string;
begin
  Result := DateToISO8601(FDate, False);
end;

procedure TAlarmBaseObject.SetDateTime(const Value: string);
begin
  if not Value.IsEmpty then
    FDate := ISO8601ToDate(Value, False)
  else
    FDate := 0;
end;

end.
