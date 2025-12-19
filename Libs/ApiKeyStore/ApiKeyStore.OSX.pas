unit ApiKeyStore.OSX;

interface

{$IFDEF MACOS}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  ApiKeyStore;

type
  // Exception raised on file errors
  EApiKeyStoreException = class(Exception);

  TApiKeyStoreOSX = class(TApiKeyStore)
  private
    function GetConfigFilePath: string;
    function LoadConfigFile: TJSONObject;
    procedure SaveConfigFile(const Config: TJSONObject);
    procedure SaveValue(const Name, Value: string);
    function LoadValue(const Name: string): string;
  public
    // API key methods
    procedure SaveApiKey(const Name, ApiKey: string); override;
    function LoadApiKey(const Name: string): string; override;
    // Settings methods
    procedure SaveSetting(const Name, Value: string); override;
    function LoadSetting(const Name: string): string; override;
  end;

{$ENDIF}

implementation


{$IFDEF MACOS}
uses
  DW.Keychain.Mac
  ;

{ TApiKeyStoreOSX }

function TApiKeyStoreOSX.GetConfigFilePath: string;
var
  DocumentsPath: string;
begin
  DocumentsPath := TPath.GetDocumentsPath;
  Result := TPath.Combine(DocumentsPath, 'app_config.json');
end;

function TApiKeyStoreOSX.LoadConfigFile: TJSONObject;
var
  ConfigPath: string;
  JSONText: string;
begin
  ConfigPath := GetConfigFilePath;

  if TFile.Exists(ConfigPath) then
  begin
    try
      JSONText := TFile.ReadAllText(ConfigPath, TEncoding.UTF8);
      Result := TJSONObject.ParseJSONValue(JSONText) as TJSONObject;
      if Result = nil then
      begin
        // Invalid JSON, create new empty object
        Result := TJSONObject.Create;
      end;
    except
      on E: Exception do
      begin
        // If we can't parse the file, create a new empty object
        Result := TJSONObject.Create;
      end;
    end;
  end
  else
  begin
    // File doesn't exist, create new empty object
    Result := TJSONObject.Create;
  end;
end;

procedure TApiKeyStoreOSX.SaveConfigFile(const Config: TJSONObject);
var
  ConfigPath: string;
  JSONText: string;
  DocumentsPath: string;
begin
  ConfigPath := GetConfigFilePath;
  DocumentsPath := TPath.GetDirectoryName(ConfigPath);

  // Ensure documents directory exists
  if not TDirectory.Exists(DocumentsPath) then
    TDirectory.CreateDirectory(DocumentsPath);

  try
    JSONText := Config.Format;
    TFile.WriteAllText(ConfigPath, JSONText, TEncoding.UTF8);
  except
    on E: Exception do
      raise EApiKeyStoreException.CreateFmt('Failed to save config file: %s', [E.Message]);
  end;
end;

procedure TApiKeyStoreOSX.SaveValue(const Name, Value: string);
var
  Config: TJSONObject;
begin
  if Name = '' then
    raise EApiKeyStoreException.Create('Name cannot be empty');

  Config := LoadConfigFile;
  try
    Config.RemovePair(Name); // Remove existing value if present
    Config.AddPair(Name, Value);
    SaveConfigFile(Config);
  finally
    Config.Free;
  end;
end;

function TApiKeyStoreOSX.LoadValue(const Name: string): string;
var
  Config: TJSONObject;
  JSONValue: TJSONValue;
begin
  Result := '';

  if Name = '' then
    Exit;

  Config := LoadConfigFile;
  try
    JSONValue := Config.GetValue(Name);
    if Assigned(JSONValue) then
      Result := JSONValue.Value;
  finally
    Config.Free;
  end;
end;

procedure TApiKeyStoreOSX.SaveApiKey(const Name, ApiKey: string);
begin
  try
//    SaveValue('apikey_' + Name, ApiKey);

    TKeychain.SetGenericPassword('apikey_' + Name, ApiKey, 'AIapikey_' + Name);
  except
    on E: Exception do
      raise EApiKeyStoreException.CreateFmt('Failed to save API key "%s": %s', [Name, E.Message]);
  end;
end;

function TApiKeyStoreOSX.LoadApiKey(const Name: string): string;
begin
  try
     if not TKeychain.GetGenericPassword('apikey_' + Name, Result, 'AIapikey_' + Name) then
       Result := '';
//    Result := LoadValue('apikey_' + Name);
  except
    on E: Exception do
    begin
      // Return empty string on error instead of raising
      Result := '';
    end;
  end;
end;

procedure TApiKeyStoreOSX.SaveSetting(const Name, Value: string);
begin
  try
    SaveValue('setting_' + Name, Value);
  except
    on E: Exception do
      raise EApiKeyStoreException.CreateFmt('Failed to save setting "%s": %s', [Name, E.Message]);
  end;
end;

function TApiKeyStoreOSX.LoadSetting(const Name: string): string;
begin
  try
    Result := LoadValue('setting_' + Name);
  except
    on E: Exception do
    begin
      // Return empty string on error instead of raising
      Result := '';
    end;
  end;
end;

{$ENDIF}

end.
