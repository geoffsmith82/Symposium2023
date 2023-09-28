unit uAmazon.Translate;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Hash,
  System.Generics.Collections,
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
    function FromLanguages: TArray<TLanguageInfo>; override;
    function ToLanguages: TArray<TLanguageInfo>; override;
  end;

implementation

constructor TAmazonTranslate.Create(const AccessKey, SecretKey, Endpoint: string);
begin
  inherited Create;
  FAccessKey := AccessKey;
  FSecretKey := SecretKey;
  FEndpoint := Endpoint;
end;

function TAmazonTranslate.FromLanguages: TArray<TLanguageInfo>;
var
  AwsTranslate : TTranslateClient;
  options : IAWSOptions;
  language : ITranslateLanguage;
  langlist : TList<ITranslateLanguage>;
  i : Integer;
begin
  options := TAWSOptions.Create;
  options.AccessKeyId := FAccessKey;
  options.SecretAccessKey := FSecretKey;
  options.Region := 'ap-southeast-2';
  AwsTranslate := TTranslateClient.Create(options);
  try
    langlist := AwsTranslate.ListLanguages.Languages;
    SetLength(Result, langlist.Count + 1);
    for i := 0 to langlist.Count - 1 do
    begin
      Result[i + 1] := TLanguageInfo.Create;
      language := langlist[i];
      Result[i + 1].LanguageName := language.LanguageName;
      Result[i + 1].LanguageCode := language.LanguageCode;
    end;
  finally
    FreeAndNil(AwsTranslate);
  end;
  Result[0] := TLanguageInfo.Create;
  Result[0].LanguageName := 'auto';
  Result[0].LanguageCode := 'auto';
end;

function TAmazonTranslate.ToLanguages: TArray<TLanguageInfo>;
var
  AwsTranslate : TTranslateClient;
  options : IAWSOptions;
  language : ITranslateLanguage;
  langlist : TList<ITranslateLanguage>;
  i : Integer;
begin
  options := TAWSOptions.Create;
  options.AccessKeyId := FAccessKey;
  options.SecretAccessKey := FSecretKey;
  options.Region := 'ap-southeast-2';
  AwsTranslate := TTranslateClient.Create(options);
  try
    langlist := AwsTranslate.ListLanguages.Languages;
    SetLength(Result, langlist.Count);
    for i := 0 to langlist.Count - 1 do
    begin
      language := langlist[i];
      Result[i] := TLanguageInfo.Create;
      Result[i].LanguageName := language.LanguageName;
      Result[i].LanguageCode := language.LanguageCode;
    end;
  finally
    FreeAndNil(AwsTranslate);
  end;
end;

function TAmazonTranslate.Translate(const SourceText: string; const toLang: string; const fromLang: string): string;
var
  AwsTranslate : ITranslateClient;
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
