unit udmPersonalAlarms;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys.Intf,
  FireDAC.Phys,
  FireDAC.Comp.Client,
  FireDAC.Phys.MSAcc,
  FireDAC.Phys.MSAccDef,
  FireDAC.UI.Intf,
  FireDAC.VCLUI.Wait,
  Data.DB,
  Controller.Personal.DataObjects
  ;

type
  TdmPersonalAlarms = class(TDataModule)
    Connection: TFDConnection;
  private
    { Private declarations }
  public
    { Public declarations }
    function GetAlarmsList():TObjectList<TAlarmObject>;
    procedure UpdateAlarm(alarmID: Integer; alarm: TAlarmUpdateObject);
    procedure DeleteAlarm(alarmID: Integer);
    function CreateAlarm(alarm: TAlarmObject): Integer;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  System.DateUtils;

{ TdmPersonalAlarms }

function TdmPersonalAlarms.CreateAlarm(alarm: TAlarmObject): Integer;
var
  query : TFDQuery;
begin
  query := TFDQuery.Create(nil);
  try
    query.Connection := Connection;
    Result := query.ExecSQL('INSERT INTO Alarms ("time", description) VALUES (?,?);', [alarm.AsDateTime, alarm.Description]);
  finally
    FreeAndNil(query);
  end;
end;

procedure TdmPersonalAlarms.DeleteAlarm(alarmID: Integer);
var
  query : TFDQuery;
begin
  query := TFDQuery.Create(nil);
  try
    query.Connection := Connection;
    query.ExecSQL('DELETE FROM Alarms WHERE id=?;', [alarmId]);
  finally
    FreeAndNil(query);
  end;
end;

function TdmPersonalAlarms.GetAlarmsList: TObjectList<TAlarmObject>;
var
  query : TFDQuery;
  alarm : TAlarmObject;
begin
  query := TFDQuery.Create(nil);
  try
    query.Connection := Connection;
    query.SQL.Text := 'SELECT * FROM Alarms';
    query.Active := True;
    Result := TObjectList<TAlarmObject>.Create;
    repeat
      if query.RecordCount = 0 then
        Exit;
      alarm := TAlarmObject.Create;
      alarm.Id := query.FieldByName('Id').AsInteger;
      alarm.Time := DateToISO8601(query.FieldByName('Time').AsDateTime, False);
      alarm.Description := query.FieldByName('Description').AsString;
      Result.Add(alarm);
      query.Next;
    until query.Eof;
  finally
    FreeAndNil(query);
  end;
end;

procedure TdmPersonalAlarms.UpdateAlarm(alarmID: Integer; alarm: TAlarmUpdateObject);
var
  query : TFDQuery;
begin
  query := TFDQuery.Create(nil);
  try
    query.Connection := Connection;
    query.ExecSQL('UPDATE Alarms SET "Time"=?, Description=? WHERE id=? ;', [alarm.AsDateTime, alarm.Description, alarmId]);
  finally
    FreeAndNil(query);
  end;
end;

end.
