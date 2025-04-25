unit ApiKeyStore.Windows;

interface
{$IFDEF MSWINDOWS}
uses
  System.SysUtils,
  System.Classes,
  ApiKeyStore,
  Winapi.Windows;

type
  TWindowsApiKeyStore = class(TApiKeyStore)
  private
    function ProtectData(const Data: TBytes): TBytes;
    function UnprotectData(const ProtectedData: TBytes): TBytes;
  public
    procedure SaveApiKey(const Name, ApiKey: string); override;
    function LoadApiKey(const Name: string): string; override;

    // Saves an Setting with a specified name
    procedure SaveSetting(const Name, Value: string); override;
    function LoadSetting(const Name: string): string; override;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
uses
  System.IOUtils,
  System.NetEncoding,
  System.JSON;

type
  DATA_BLOB = record
    cbData: DWORD;   // Length of the data in bytes
    pbData: PByte;   // Pointer to the data buffer
  end;

const
  CRYPTPROTECT_UI_FORBIDDEN = $1;          // Prevents any user interface prompts
  CRYPTPROTECT_LOCAL_MACHINE = $4;         // Data is encrypted for the local machine context (any user on the machine)

function CryptProtectData(pDataIn: PDATA_BLOB; szDataDescr: LPCWSTR;
  pOptionalEntropy: PDATA_BLOB; pvReserved: Pointer;
  pPromptStruct: Pointer; dwFlags: DWORD;
  pDataOut: PDATA_BLOB): BOOL; stdcall; external 'Crypt32.dll';

function CryptUnprotectData(pDataIn: PDATA_BLOB; ppszDataDescr: PLPWSTR;
  pOptionalEntropy: PDATA_BLOB; pvReserved: Pointer;
  pPromptStruct: Pointer; dwFlags: DWORD;
  pDataOut: PDATA_BLOB): BOOL; stdcall; external 'Crypt32.dll';

{ TWindowsApiKeyStore }



function TWindowsApiKeyStore.ProtectData(const Data: TBytes): TBytes;
var
  DataIn, DataOut: DATA_BLOB;
begin
  DataIn.cbData := Length(Data);
  DataIn.pbData := @Data[0];

  if CryptProtectData(@DataIn, nil, nil, nil, nil, CRYPTPROTECT_LOCAL_MACHINE, @DataOut) then
  try
    SetLength(Result, DataOut.cbData);
    Move(DataOut.pbData^, Result[0], DataOut.cbData);
  finally
    LocalFree(HLOCAL(DataOut.pbData));
  end
  else
    raise Exception.Create('Failed to encrypt data using DPAPI');
end;

function TWindowsApiKeyStore.UnprotectData(const ProtectedData: TBytes): TBytes;
var
  DataIn, DataOut: DATA_BLOB;
begin
  DataIn.cbData := Length(ProtectedData);
  DataIn.pbData := @ProtectedData[0];

  if CryptUnprotectData(@DataIn, nil, nil, nil, nil, 0, @DataOut) then
  try
    SetLength(Result, DataOut.cbData);
    Move(DataOut.pbData^, Result[0], DataOut.cbData);
  finally
    LocalFree(HLOCAL(DataOut.pbData));
  end
  else
    raise Exception.Create('Failed to decrypt data using DPAPI');
end;

procedure TWindowsApiKeyStore.SaveApiKey(const Name, ApiKey: string);
var
  JsonData: TJSONObject;
  EncryptedData: TBytes;
  FileName: string;
  data : string;
begin
  FileName := TPath.Combine(TPath.GetDocumentsPath, 'ApiKeys.json');
  data := TFile.ReadAllText(FileName);
  JsonData := TJSONObject.ParseJSONValue(data) as TJSONObject;
  try
    if APIKey.Length > 0 then
    begin
      EncryptedData := ProtectData(TEncoding.UTF8.GetBytes(ApiKey));
      JsonData.RemovePair(Name);
      JsonData.AddPair(Name, TNetEncoding.Base64String.EncodeBytesToString(EncryptedData));
    end
    else
    begin
      JsonData.RemovePair(Name);
    end;
    TFile.WriteAllText(FileName, JsonData.ToJSON);
  finally
    JsonData.Free;
  end;
end;

procedure TWindowsApiKeyStore.SaveSetting(const Name, Value: string);
var
  JsonData: TJSONObject;
  FileName: string;
  data : string;
begin
  FileName := TPath.Combine(TPath.GetDocumentsPath, 'ApiKeys.json');
  data := TFile.ReadAllText(FileName);
  JsonData := TJSONObject.ParseJSONValue(data) as TJSONObject;
  try
    if Value.Length > 0 then
    begin
      JsonData.RemovePair(Name);
      JsonData.AddPair(Name, Value);
    end
    else
    begin
      JsonData.RemovePair(Name);
    end;
    TFile.WriteAllText(FileName, JsonData.ToJSON);
  finally
    JsonData.Free;
  end;
end;

function TWindowsApiKeyStore.LoadSetting(const Name: string): string;
var
  JsonData: TJSONObject;
  base64Data : string;
  FileName: string;
begin
  Result := '';
  if Name.IsEmpty then
  begin
    Exit;
  end;

  FileName := TPath.Combine(TPath.GetDocumentsPath, 'ApiKeys.json');
  if TFile.Exists(FileName) then
  begin
    JsonData := TJSONObject.Create;
    try
      JsonData.Parse(BytesOf(TFile.ReadAllText(FileName)), 0);
      if JsonData.TryGetValue(Name, base64Data) then
      begin
        Result := base64Data;
      end;
    finally
      JsonData.Free;
    end;
  end
  else
    raise Exception.CreateFmt('Setting for "%s" not found.', [Name]);
end;

function TWindowsApiKeyStore.LoadApiKey(const Name: string): string;
var
  JsonData: TJSONObject;
  EncryptedData, DecryptedData: TBytes;
  base64Data : string;
  FileName: string;
begin
  Result := '';
  FileName := TPath.Combine(TPath.GetDocumentsPath, 'ApiKeys.json');
  if TFile.Exists(FileName) then
  begin
    JsonData := TJSONObject.Create;
    try
      JsonData.Parse(BytesOf(TFile.ReadAllText(FileName)), 0);
      if JsonData.TryGetValue(Name, base64Data) then
      begin
        EncryptedData := TNetEncoding.Base64String.DecodeStringToBytes(base64Data);
        DecryptedData := UnprotectData(EncryptedData);
        Result := TEncoding.UTF8.GetString(DecryptedData);
      end;
    finally
      JsonData.Free;
    end;
  end
  else
    raise Exception.CreateFmt('API Key for "%s" not found.', [Name]);
end;

{$ENDIF}

end.

