unit uAmazon.Translate;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Hash,
  REST.Types,
  REST.Client,
  REST.Response.Adapter,
  uBaseTranslate
  ;

type
  TAmazonTranslate = class(TBaseTranslate)
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FAccessKey: string;
    FSecretKey: string;
    FEndpoint: string;
    FSourceLang: string;
    FTargetLang: string;
  public
    constructor Create(const AccessKey, SecretKey, Endpoint: string);
    function Translate(const SourceText: string; const toLang: string; const fromLang: string): string; override;
  end;

implementation

constructor TAmazonTranslate.Create(const AccessKey, SecretKey, Endpoint: string);
begin
  inherited Create;
  FAccessKey := AccessKey;
  FSecretKey := SecretKey;
  FEndpoint := Endpoint;
  // Create a new REST client and set the base URL to the Amazon Translate API endpoint
  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.BaseURL := FEndpoint;

  // Create a new REST request and set its parameters
  FRESTRequest := TRESTRequest.Create(nil);
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.AddParameter('X-Amz-Target', 'AWSShineFrontendService_20170701.TranslateText');
  FRESTRequest.AddParameter('Content-Type', 'application/x-amz-json-1.1');

  // Set the authorization headers using the access key and secret key
  FRESTRequest.Params.AddItem('X-Amz-Content-Sha256', '', TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);
  FRESTRequest.Params.AddItem('X-Amz-Date', '', TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);
  FRESTRequest.Params.AddItem('Authorization', '', TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);

  // Create a new REST response adapter
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTResponse.ContentType := 'application/json';
end;

function SHA256(const Input: string): string;
var
  HashBytes: TBytes;
  HashValue: TBytes;
  I: Integer;
  sha2 : THashSHA2;
begin
  HashBytes := TEncoding.UTF8.GetBytes(Input);
  sha2 := THashSHA2.Create;
  Result := sha2.GetHashString(Input);
end;

function SignRequest(const Method, URI, Body, AccessKey, SecretKey, Region, Service, Token, Timestamp: string): string;
var
  CanonicalRequest, StringToSign, Signature, DateStamp, AmzDate: string;
  KDate, KRegion, KService, KSigning: TBytes;
  Hasher: THashSHA2;
  I: Integer;
begin
  // Calculate the date and time values
  DateStamp := FormatDateTime('yyyymmdd', Now, TFormatSettings.Create('en-US'));
  AmzDate := Timestamp.Substring(0, 8) + 'T' + Timestamp.Substring(9, 6) + 'Z';

  // Calculate the hashed canonical request
  CanonicalRequest := Format('%s'#10'%s'#10'%s'#10'host:%s'#10'x-amz-content-sha256:%s'#10'x-amz-date:%s'#10'%s'#10'%s', [Method, URI, '', 'translate.us-west-2.amazonaws.com', SHA256(Body), AmzDate, 'content-type;host;x-amz-content-sha256;x-amz-date', SHA256(Body)]);
  Hasher := THashSHA2.Create;
  Hasher.Update(TEncoding.UTF8.GetBytes(CanonicalRequest));
  CanonicalRequest := Hasher.HashAsString;


  // Calculate the hashed string to sign
  StringToSign := Format('AWS4-HMAC-SHA256'#10'%s'#10'%s/%s/translate/aws4_request'#10'%s', [AmzDate, DateStamp, Region, SHA256(CanonicalRequest)]);

{
  // Calculate the signing keys
  KDate := THashSHA2.GetHMAC(TEncoding.UTF8.GetBytes('AWS4' + SecretKey), TEncoding.UTF8.GetBytes(DateStamp));
  KRegion := THashSHA2.GetHMAC(KDate, TEncoding.UTF8.GetBytes(Region));
  KService := THashSHA2.GetHMAC(KRegion, TEncoding.UTF8.GetBytes(Service));
  KSigning := THashSHA2.GetHMAC(KService, TEncoding.UTF8.GetBytes('aws4_request'));

  // Calculate the signature
  Signature := THashSHA2.GetHMAC(KSigning, TEncoding.UTF8.GetBytes(StringToSign)).BytesToHex;
}
  // Return the authorization header value
  Result := Signature;
end;

function TAmazonTranslate.Translate(const SourceText: string; const toLang: string; const fromLang: string): string;
var
  RequestBody: string;
  Timestamp: string;
  Signature: string;
begin
  // Build the request body JSON
  RequestBody := Format('{"SourceLanguageCode": "%s", "TargetLanguageCode": "%s", "Text": "%s"}', [FSourceLang, FTargetLang, SourceText]);

  // Generate the request timestamp and signature
  Timestamp := FormatDateTime('yyyymmdd"T"hhnnss"Z"', Now, TFormatSettings.Create('en-US'));
  Signature := SignRequest(''{FRESTRequest.Method}, '/translate', RequestBody, FAccessKey, FSecretKey, '', '', '', Timestamp);

  // Set the authorization headers using the timestamp and signature
  FRESTRequest.Params[0].Value := SHA256(RequestBody);
  FRESTRequest.Params[1].Value := Timestamp;
  FRESTRequest.Params[2].Value := Format('AWS4-HMAC-SHA256 Credential=%s/%s/us-west-2/translate/aws4_request, SignedHeaders=content-type;host;x-amz-content-sha256;x-amz-date, Signature=%s', [FAccessKey, FormatDateTime('yyyymmdd', Now, TFormatSettings.Create('en-US')), Signature]);

    // Set the request body
  FRESTRequest.AddBody(RequestBody);

  // Execute the request and get the response
  FRESTRequest.Execute;
  Result := FRESTResponse.Content;
end;

end.
