unit uEngineManager;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections
  ;

type
  TEngineManager<T: class; M: class> = class
  public
    FEngines : TObjectDictionary<string, T>;
    FEngineMenuItems : TDictionary<string, M>;
    FEngineNames: TDictionary<M, string>;
    FEngineOnSelect: TDictionary<T, TNotifyEvent>;
    FActiveEngine : T;
    FActiveMenu: M;
  public
    function ActiveEngine: T;
    function ActiveMenuItem: M;
    procedure SelectEngine(menuItem: M); overload;
    procedure SelectEngine(engineName: string); overload;
    procedure RegisterEngine(engineClass: T; menuItem: M; OnSelect: TNotifyEvent = nil);
    constructor Create;
    destructor Destroy; override;
  end;




implementation

{ TEngineManager<T> }

function TEngineManager<T, M>.ActiveEngine: T;
begin
  Result := FActiveEngine;
end;

function TEngineManager<T, M>.ActiveMenuItem: M;
begin
  Result := FActiveMenu;
end;

constructor TEngineManager<T, M>.Create;
begin
  FEngines := TObjectDictionary<string, T>.Create([doOwnsValues]);
  FEngineMenuItems := TDictionary<string, M>.Create;
  FEngineNames := TDictionary<M, string>.Create;
  FEngineOnSelect := TDictionary<T, TNotifyEvent>.Create;
end;

destructor TEngineManager<T, M>.Destroy;
begin
  FreeAndNil(FEngines);
  FreeAndNil(FEngineMenuItems);
  FreeAndNil(FEngineNames);
  FreeAndNil(FEngineOnSelect);
  inherited;
end;

procedure TEngineManager<T, M>.RegisterEngine(engineClass: T; menuItem: M; OnSelect: TNotifyEvent);
var
  LEngineName: string;
begin
  LEngineName := engineClass.ClassName;
  FEngines.AddOrSetValue(LEngineName, engineClass);
  FEngineMenuItems.Add(LEngineName, menuItem);
  FEngineNames.Add(menuItem, LEngineName);
  FEngineOnSelect.Add(engineClass, OnSelect);
  if FEngines.Count = 1 then
  begin
    FActiveEngine := engineClass;
    FActiveMenu := menuItem;
  end;
end;

procedure TEngineManager<T, M>.SelectEngine(menuItem: M);
begin
  SelectEngine(FEngineNames[menuItem]);
end;

procedure TEngineManager<T, M>.SelectEngine(engineName: string);
var
  LNewEngine : T;
  LOnSelect : TNotifyEvent;
begin
  if FEngines.TryGetValue(engineName, LNewEngine) then
  begin
    FActiveEngine := LNewEngine;
    FEngineMenuItems.TryGetValue(engineName, FActiveMenu);
    if FEngineOnSelect.TryGetValue(FActiveEngine, LOnSelect) then
    begin
      if Assigned(LOnSelect) then
        LOnSelect(nil);
    end;
  end;
end;

end.
