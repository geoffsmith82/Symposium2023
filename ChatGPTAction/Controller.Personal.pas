unit Controller.Personal;

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
  TPersonalController = class (TMVCController)
  public
    // Get current time
    [MVCPath('/current-time')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCurrentTime;

    // Get all alarms
    [MVCPath('/alarms')]
    [MVCHTTPMethod([httpGET])]
    procedure GetAlarms;

    // Create a new alarm
    [MVCPath('/alarms')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateAlarm;

    // Update an existing alarm
    [MVCPath('/alarms/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateAlarm(id: Integer);

    // Delete an existing alarm
    [MVCPath('/alarms/($id)')]
    [MVCHTTPMethod([httpDELETE])]
    procedure DeleteAlarm(id: String);

    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string;
      var AHandled: Boolean); override;

    procedure LogMessage(msg: string);
  end;

implementation

uses
  FormUnit1,
  Controller.Personal.DataObjects,
  System.JSON,
  udmPersonalAlarms;


procedure TPersonalController.GetCurrentTime;
var
  TimeObject : TCurrentTimeObject;
begin
  TimeObject := TCurrentTimeObject.Create;
  try
    TimeObject.CurrentTime := DateToISO8601(Now.NowUTC, True);
    Render(TimeObject, False);
  finally
    FreeAndNil(TimeObject);
  end;
end;

procedure TPersonalController.LogMessage(msg: string);
begin
  TThread.Queue(nil, procedure
     begin
       Form1.mmoRequests.Lines.Add(msg);
     end);
end;

procedure TPersonalController.OnBeforeAction(AContext: TWebContext;
  const AActionName: string; var AHandled: Boolean);
begin
  inherited;
  LogMessage(DateTimeToStr(now) + ' ' + AContext.Request.PathInfo);
end;

procedure TPersonalController.GetAlarms;
var
//  Alarms: TJSONArray;
  alarmList : TObjectList<TAlarmObject>;
  alarms : TAlarmsResponse;
  dmPersonalAlarms: TdmPersonalAlarms;
begin
  alarms := nil;
  alarmList := nil;
  dmPersonalAlarms := nil;
  try
    dmPersonalAlarms := TdmPersonalAlarms.Create(nil);
    alarms := TAlarmsResponse.Create;
    alarmList := dmPersonalAlarms.GetAlarmsList;
    alarms.Alarms.AddRange(alarmList);
    Render(Alarms, False);
  finally
    FreeAndNil(alarmList);
 //   FreeAndNil(Alarms);
    FreeAndNil(dmPersonalAlarms)
  end;
end;

procedure TPersonalController.CreateAlarm;
var
  Alarm: TAlarmObject;
  dmPersonalAlarms: TdmPersonalAlarms;
//  content : TJSONObject;
begin
  Alarm := Context.Request.BodyAs<TAlarmObject>;
//  content := Context.Request.BodyAs<TJSONObject>;
  try
    try
//      content.Values['time']
      dmPersonalAlarms := TdmPersonalAlarms.Create(nil);
      dmPersonalAlarms.CreateAlarm(Alarm);
      Render201Created('','');
//    Render(Alarm, 201);  // Return the created alarm with 201 status
    finally
      FreeAndNil(dmPersonalAlarms);
    end;

  except
    on E: Exception do
    begin
      BadRequestResponse(e.Message);
    end;
  end;
end;

procedure TPersonalController.UpdateAlarm(id: Integer);
var
  Alarm: TAlarmUpdateObject;
  dmPersonalAlarms: TdmPersonalAlarms;
begin
  Alarm := Context.Request.BodyAs<TAlarmUpdateObject>;
  try
    try
      dmPersonalAlarms := TdmPersonalAlarms.Create(nil);
      dmPersonalAlarms.UpdateAlarm(Id, Alarm);
      Render(Alarm);  // Return the updated alarm
    finally
      FreeAndNil(dmPersonalAlarms);
    end;
  except
    on E: Exception do
    begin
      BadRequestResponse(e.Message);
    end;
  end;
end;

procedure TPersonalController.DeleteAlarm(id: String);
var
  dmPersonalAlarms: TdmPersonalAlarms;
begin
  try
    // Code to delete the existing alarm identified by id from storage (e.g., database)
    try
      dmPersonalAlarms := TdmPersonalAlarms.Create(nil);
      dmPersonalAlarms.DeleteAlarm(id.ToInteger);
      Render(204, '');  // Return no content with 204 status
    finally
      FreeAndNil(dmPersonalAlarms);
    end;
  except
    on E: Exception do
    begin
      NotFoundResponse(e.Message);
    end;
  end;
end;


end.
