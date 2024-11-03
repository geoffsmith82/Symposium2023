unit ApiKeyStore;

interface

uses
  System.SysUtils, System.Classes;

type
  TApiKeyStore = class
  private
    class var FInstance: TApiKeyStore;
    constructor Create; virtual;
    destructor Destroy; override;
    class destructor Destroy;
  public


    // Saves an API key with a specified name
    procedure SaveApiKey(const Name, ApiKey: string); virtual; abstract;

    // Loads an API key by name
    function LoadApiKey(const Name: string): string; virtual; abstract;

    // Singleton instance access
    class function GetInstance: TApiKeyStore;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  ApiKeyStore.Windows; // Platform-specific unit for Windows
{$ENDIF}
{$IFDEF ANDROID}
uses
  AndroidApiKeyStore; // Platform-specific unit for Android
{$ENDIF}

constructor TApiKeyStore.Create;
begin
  inherited Create;
end;

destructor TApiKeyStore.Destroy;
begin
  inherited Destroy;
end;

class destructor TApiKeyStore.Destroy;
begin
  FreeAndNil(FInstance);
end;

class function TApiKeyStore.GetInstance: TApiKeyStore;
begin
  if not Assigned(FInstance) then
  begin
    {$IFDEF MSWINDOWS}
    FInstance := TWindowsApiKeyStore.Create;
    {$ENDIF}
    {$IFDEF ANDROID}
    FInstance := TAndroidApiKeyStore.Create;
    {$ENDIF}
  end;
  Result := FInstance;
end;

end.

