unit uAmazon.Translate;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Hash,
  REST.Types,
  REST.Client,
  REST.Response.Adapter,
  AWS.Translate,
  AWS.Core,
  uBaseTranslate
  ;

type
  TAmazonTranslate = class(TBaseTranslate)
  strict private
    FAccessKey: string;
    FSecretKey: string;
    FEndpoint: string;
    FSourceLang: string;
    FTargetLang: string;
  public
    constructor Create(const AccessKey, SecretKey, Endpoint: string);
    function Translate(const SourceText: string; const toLang: string; const fromLang: string): string; override;
    function FromLanguages: TArray<string>; override;
    function ToLanguages: TArray<string>; override;
  end;

implementation

constructor TAmazonTranslate.Create(const AccessKey, SecretKey, Endpoint: string);
begin
  inherited Create;
  FAccessKey := AccessKey;
  FSecretKey := SecretKey;
  FEndpoint := Endpoint;
end;

function TAmazonTranslate.FromLanguages: TArray<string>;
begin
  SetLength(Result, 1);
  Result[0] := 'French';
end;

function TAmazonTranslate.ToLanguages: TArray<string>;
begin
  SetLength(Result, 1);
  Result[0] := 'English';
end;

function TAmazonTranslate.Translate(const SourceText: string; const toLang: string; const fromLang: string): string;
var
  AwsTranslate : TTranslateClient;
  request : TTranslateRequest;
  response : ITranslateTranslateTextResponse;
  options : IAWSOptions;
begin
  options := TAWSOptions.Create;
  options.AccessKeyId := FAccessKey;
  options.SecretAccessKey := FSecretKey;
  options.Region := 'ap-southeast-2';
  AwsTranslate := TTranslateClient.Create(options);

  response := AwsTranslate.TranslateText(fromlang, toLang, SourceText);
  Result := response.TranslatedText;
end;

end.
