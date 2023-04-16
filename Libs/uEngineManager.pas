unit uEngineManager;

interface

uses
  System.SysUtils,
  Vcl.Menus,
  System.Generics.Collections
  ;

type
  TEngineManager<T: class> = class
  public
    FEngines : TObjectDictionary<string, T>;
    FEngineMenuItems : TDictionary<string, TMenuItem>;
    FEngineNames: TDictionary<TMenuItem, string>;
    FActiveEngine : T;
    FActiveMenu: TMenuItem;
  public
    function ActiveEngine: T;
    function ActiveMenuItem: TMenuItem;
    procedure SelectEngine(menuItem: TMenuItem); overload;
    procedure SelectEngine(engineName: string); overload;
    procedure RegisterEngine(engineClass : T; menuItem: TMenuItem);
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
end;

destructor TEngineManager<T>.Destroy;
begin
  FreeAndNil(FEngines);
  FreeAndNil(FEngineMenuItems);
  FreeAndNil(FEngineNames);
  inherited;
end;

procedure TEngineManager<T>.RegisterEngine(engineClass: T; menuItem: TMenuItem);
var
  engineName: string;
begin
  engineName := engineClass.ClassName;
  FEngines.AddOrSetValue(engineName, engineClass);
  FEngineMenuItems.Add(engineName, menuItem);
  FEngineNames.Add(menuItem, engineName);
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
  newEngine : T;
begin
  if FEngines.TryGetValue(engineName, newEngine) then
  begin
    FActiveEngine := newEngine;
    FEngineMenuItems.TryGetValue(engineName, FActiveMenu);
  end;
end;

end.
