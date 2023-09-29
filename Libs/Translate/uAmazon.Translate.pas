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
    FRegion: string;
  public
    constructor Create(const AccessKey, SecretKey, Region: string);
    function Translate(const SourceText: string; const toLang: string; const fromLang: string): string; override;
    function FromLanguages: TObjectList<TLanguageInfo>; override;
    function ToLanguages: TObjectList<TLanguageInfo>; override;
  end;

implementation

constructor TAmazonTranslate.Create(const AccessKey, SecretKey, Region: string);
begin
  inherited Create('');
  FAccessKey := AccessKey;
  FSecretKey := SecretKey;
  FRegion := Region;
end;

function TAmazonTranslate.FromLanguages: TObjectList<TLanguageInfo>;
var
  AwsTranslate : TTranslateClient;
  options : IAWSOptions;
  language : ITranslateLanguage;
  langlist : TList<ITranslateLanguage>;
  i : Integer;
  langInfo : TLanguageInfo;
begin
  options := TAWSOptions.Create;
  options.AccessKeyId := FAccessKey;
  options.SecretAccessKey := FSecretKey;
  options.Region := FRegion;
  AwsTranslate := TTranslateClient.Create(options);
  try
    FFromLanguages.Clear;
    langInfo := TLanguageInfo.Create;
    langInfo.LanguageName := 'auto';
    langInfo.LanguageCode := 'auto';
    FFromLanguages.Add(langInfo);

    langlist := AwsTranslate.ListLanguages.Languages;
    for i := 0 to langlist.Count - 1 do
    begin
      language := langlist[i];
      langInfo := TLanguageInfo.Create;
      langInfo.LanguageName := language.LanguageName;
      langInfo.LanguageCode := language.LanguageCode;
      FFromLanguages.Add(langInfo);
    end;
  finally
    FreeAndNil(AwsTranslate);
  end;
  Result := FFromLanguages;
end;

function TAmazonTranslate.ToLanguages: TObjectList<TLanguageInfo>;
var
  AwsTranslate : TTranslateClient;
  options : IAWSOptions;
  language : ITranslateLanguage;
  langlist : TList<ITranslateLanguage>;
  i : Integer;
  langInfo : TLanguageInfo;
begin
  options := TAWSOptions.Create;
  options.AccessKeyId := FAccessKey;
  options.SecretAccessKey := FSecretKey;
  options.Region := FRegion;
  AwsTranslate := TTranslateClient.Create(options);
  try
    FToLanguages.Clear;
    langlist := AwsTranslate.ListLanguages.Languages;
    for i := 0 to langlist.Count - 1 do
    begin
      language := langlist[i];
      langInfo := TLanguageInfo.Create;
      langInfo.LanguageName := language.LanguageName;
      langInfo.LanguageCode := language.LanguageCode;
      FToLanguages.Add(langInfo);
    end;
  finally
    FreeAndNil(AwsTranslate);
  end;
  Result := FToLanguages;
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
  options.Region := FRegion;
  AwsTranslate := TTranslateClient.Create(options);
  response := AwsTranslate.TranslateText(fromlang, toLang, SourceText);
  Result := response.TranslatedText;
end;

end.
