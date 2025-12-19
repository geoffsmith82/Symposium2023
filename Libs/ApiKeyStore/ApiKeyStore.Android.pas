unit ApiKeyStore.Android;

interface

{$IFDEF ANDROID}

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.NetEncoding,
  ApiKeyStore;

type
  TAndroidApiKeyStore = class(TApiKeyStore)
  private
    function EncryptData(const PlainText: string): string;
    function DecryptData(const EncryptedText: string): string;
    function GetStorageFile: string;
  public
    procedure SaveApiKey(const Name, ApiKey: string); override;
    function LoadApiKey(const Name: string): string; override;

    procedure SaveSetting(const Name, Value: string); override;
    function LoadSetting(const Name: string): string; override;
  end;

{$ENDIF}

implementation

{$IFDEF ANDROID}

uses
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Security,
  Androidapi.JNI.Java.Security,
  Androidapi.JNI.Javax.Crypto,
  Androidapi.JNI.Javax.Crypto.Spec,
  Androidapi.JNIBridge;

const
  AES_KEY_ALIAS = 'ApiKeyStoreKey';
  AES_MODE = 'AES/GCM/NoPadding';
  IV_SEPARATOR = ':'; // separates IV and encrypted data

function TJavaArrayToBytes(A: TJavaArray<Byte>): TBytes;
begin
  SetLength(Result, A.Length);
  if A.Length > 0 then
    Move(A.Data^, Result[0], A.Length);
end;

function BytesToTJavaArray(const Bytes: TBytes): TJavaArray<Byte>;
var
  I: Integer;
begin
  Result := TJavaArray<Byte>.Create(Length(Bytes));
  for I := 0 to High(Bytes) do
    Result[I] := Bytes[I];
end;

function GenerateOrGetSecretKey: JSecretKey;
var
  KeyGenerator: JKeyGenerator;
  KeyGenSpec: JKeyGenParameterSpec;
  KeyStore: JKeyStore;
begin
  KeyStore := TJKeyStore.JavaClass.getInstance(StringToJString('AndroidKeyStore'));
  KeyStore.load(nil);

  if not KeyStore.containsAlias(StringToJString(AES_KEY_ALIAS)) then
  begin
    KeyGenerator := TJKeyGenerator.JavaClass.getInstance(StringToJString('AES'), StringToJString('AndroidKeyStore'));
    KeyGenSpec := TJKeyGenParameterSpec_Builder.JavaClass.init(
      StringToJString(AES_KEY_ALIAS),
      TJKeyProperties.JavaClass.PURPOSE_ENCRYPT or TJKeyProperties.JavaClass.PURPOSE_DECRYPT
    ).setBlockModes(TJKeyProperties.JavaClass.BLOCK_MODE_GCM)
     .setEncryptionPaddings(TJKeyProperties.JavaClass.ENCRYPTION_PADDING_NONE)
     .build;

    KeyGenerator.init(KeyGenSpec);
    KeyGenerator.generateKey;
  end;

  Result := TJSecretKey.Wrap(KeyStore.getKey(StringToJString(AES_KEY_ALIAS), nil));
end;

function TAndroidApiKeyStore.GetStorageFile: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'ApiKeys.json');
end;

function TAndroidApiKeyStore.EncryptData(const PlainText: string): string;
var
  Cipher: JCipher;
  SecretKey: JSecretKey;
  IVBytes, CipherText: TJavaArray<Byte>;
  IVStr, CipherStr: string;
begin
  SecretKey := GenerateOrGetSecretKey;
  Cipher := TJCipher.JavaClass.getInstance(StringToJString(AES_MODE));
  Cipher.init(TJCipher.JavaClass.ENCRYPT_MODE, SecretKey);

  IVBytes := Cipher.getIV;
  CipherText := Cipher.doFinal(StringToJString(PlainText).getBytes(TJCharset.JavaClass.forName(StringToJString('UTF-8'))));

  IVStr := TNetEncoding.Base64.EncodeBytesToString(TJavaArrayToBytes(IVBytes));
  CipherStr := TNetEncoding.Base64.EncodeBytesToString(TJavaArrayToBytes(CipherText));

  Result := IVStr + IV_SEPARATOR + CipherStr;
end;

function TAndroidApiKeyStore.DecryptData(const EncryptedText: string): string;
var
  Cipher: JCipher;
  SecretKey: JSecretKey;
  IVBase64, CipherBase64: string;
  IVBytes, CipherBytes: TJavaArray<Byte>;
  GCMSpec: JGCMParameterSpec;
  DecryptedBytes: TJavaArray<Byte>;
begin
  IVBase64 := EncryptedText.Substring(0, EncryptedText.IndexOf(IV_SEPARATOR));
  CipherBase64 := EncryptedText.Substring(EncryptedText.IndexOf(IV_SEPARATOR) + 1);

  IVBytes := BytesToTJavaArray(TNetEncoding.Base64.DecodeStringToBytes(IVBase64));
  CipherBytes := BytesToTJavaArray(TNetEncoding.Base64.DecodeStringToBytes(CipherBase64));

  SecretKey := GenerateOrGetSecretKey;
  Cipher := TJCipher.JavaClass.getInstance(StringToJString(AES_MODE));
  GCMSpec := TJGCMParameterSpec.JavaClass.init(128, IVBytes);
  Cipher.init(TJCipher.JavaClass.DECRYPT_MODE, SecretKey, GCMSpec);

  DecryptedBytes := Cipher.doFinal(CipherBytes);
  Result := JStringToString(TJString.Wrap(TJString.JavaClass.init(DecryptedBytes, StringToJString('UTF-8'))));
end;

procedure TAndroidApiKeyStore.SaveApiKey(const Name, ApiKey: string);
var
  JsonData: TJSONObject;
  FileName, EncData: string;
begin
  FileName := GetStorageFile;
  if TFile.Exists(FileName) then
    JsonData := TJSONObject.ParseJSONValue(TFile.ReadAllText(FileName)) as TJSONObject
  else
    JsonData := TJSONObject.Create;

  try
    JsonData.RemovePair(Name);
    if not ApiKey.IsEmpty then
    begin
      EncData := EncryptData(ApiKey);
      JsonData.AddPair(Name, EncData);
    end;
    TFile.WriteAllText(FileName, JsonData.ToJSON);
  finally
    JsonData.Free;
  end;
end;

function TAndroidApiKeyStore.LoadApiKey(const Name: string): string;
var
  JsonData: TJSONObject;
  FileName, EncData: string;
begin
  Result := '';
  FileName := GetStorageFile;
  if not TFile.Exists(FileName) then Exit;

  JsonData := TJSONObject.ParseJSONValue(TFile.ReadAllText(FileName)) as TJSONObject;
  try
    if JsonData.TryGetValue(Name, EncData) then
      Result := DecryptData(EncData);
  finally
    JsonData.Free;
  end;
end;

procedure TAndroidApiKeyStore.SaveSetting(const Name, Value: string);
var
  JsonData: TJSONObject;
  FileName: string;
begin
  FileName := GetStorageFile;
  if TFile.Exists(FileName) then
    JsonData := TJSONObject.ParseJSONValue(TFile.ReadAllText(FileName)) as TJSONObject
  else
    JsonData := TJSONObject.Create;

  try
    JsonData.RemovePair(Name);
    if not Value.IsEmpty then
      JsonData.AddPair(Name, Value);
    TFile.WriteAllText(FileName, JsonData.ToJSON);
  finally
    JsonData.Free;
  end;
end;

function TAndroidApiKeyStore.LoadSetting(const Name: string): string;
var
  JsonData: TJSONObject;
  FileName, Value: string;
begin
  Result := '';
  FileName := GetStorageFile;
  if not TFile.Exists(FileName) then Exit;

  JsonData := TJSONObject.ParseJSONValue(TFile.ReadAllText(FileName)) as TJSONObject;
  try
    if JsonData.TryGetValue(Name, Value) then
      Result := Value;
  finally
    JsonData.Free;
  end;
end;

{$ENDIF}

end.

