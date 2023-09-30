unit uEngineManager;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Menus,
  System.Generics.Collections
  ;

type
  TEngineManager<T: class> = class
  public
    FEngines : TObjectDictionary<string, T>;
    FEngineMenuItems : TDictionary<string, TMenuItem>;
    FEngineNames: TDictionary<TMenuItem, string>;
    FEngineOnSelect: TDictionary<T, TNotifyEvent>;
    FActiveEngine : T;
    FActiveMenu: TMenuItem;
  public
    function ActiveEngine: T;
    function ActiveMenuItem: TMenuItem;
    procedure SelectEngine(menuItem: TMenuItem); overload;
    procedure SelectEngine(engineName: string); overload;
    procedure RegisterEngine(engineClass: T; menuItem: TMenuItem; OnSelect: TNotifyEvent = nil);
    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TEngineManager<T> }

function TEngineManager<T>.ActiveEngine: T;
begin
  Result := FActiveEngine;
end;

function TEngineManager<T>.ActiveMenuItem: TMenuItem;
begin
  Result := FActiveMenu;
end;

constructor TEngineManager<T>.Create;
begin
  FEngines := TObjectDictionary<string, T>.Create([doOwnsValues]);
  FEngineMenuItems := TDictionary<string, TMenuItem>.Create;
  FEngineNames := TDictionary<TMenuItem, string>.Create;
  FEngineOnSelect := TDictionary<T, TNotifyEvent>.Create;
end;

destructor TEngineManager<T>.Destroy;
begin
  FreeAndNil(FEngines);
  FreeAndNil(FEngineMenuItems);
  FreeAndNil(FEngineNames);
  FreeAndNil(FEngineOnSelect);
  inherited;
end;

procedure TEngineManager<T>.RegisterEngine(engineClass: T; menuItem: TMenuItem; OnSelect: TNotifyEvent);
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

procedure TEngineManager<T>.SelectEngine(menuItem: TMenuItem);
begin
  SelectEngine(FEngineNames[menuItem]);
end;

procedure TEngineManager<T>.SelectEngine(engineName: string);
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
