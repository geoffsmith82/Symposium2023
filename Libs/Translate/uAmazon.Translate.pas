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
var
  AwsTranslate : TTranslateClient;
  response : ITranslateTranslateTextResponse;
  options : IAWSOptions;
  language : ITranslateLanguage;
  i : Integer;
begin
  options := TAWSOptions.Create;
  options.AccessKeyId := FAccessKey;
  options.SecretAccessKey := FSecretKey;
  options.Region := 'ap-southeast-2';
  AwsTranslate := TTranslateClient.Create(options);
  SetLength(Result, AwsTranslate.ListLanguages.Languages.Count + 1);
  for i := 0 to AwsTranslate.ListLanguages.Languages.Count - 1 do
  begin
    language := AwsTranslate.ListLanguages.Languages[i];
    Result[i + 1] := language.LanguageName;
  end;
  Result[0] := 'auto';
end;

function TAmazonTranslate.ToLanguages: TArray<string>;
var
  AwsTranslate : TTranslateClient;
  response : ITranslateTranslateTextResponse;
  options : IAWSOptions;
  language : ITranslateLanguage;
  i : Integer;
begin
  options := TAWSOptions.Create;
  options.AccessKeyId := FAccessKey;
  options.SecretAccessKey := FSecretKey;
  options.Region := 'ap-southeast-2';
  AwsTranslate := TTranslateClient.Create(options);
  SetLength(Result, AwsTranslate.ListLanguages.Languages.Count + 1);
  for i := 0 to AwsTranslate.ListLanguages.Languages.Count - 1 do
  begin
    language := AwsTranslate.ListLanguages.Languages[i];
    Result[i] := language.LanguageName;
  end;
end;

function TAmazonTranslate.Translate(const SourceText: string; const toLang: string; const fromLang: string): string;
var
  AwsTranslate : TTranslateClient;
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
