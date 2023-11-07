unit uTranslate.Amazon;

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
  uTranslate
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
  LAwsTranslate : TTranslateClient;
  LOptions : IAWSOptions;
  LLanguage : ITranslateLanguage;
  LLanglist : TList<ITranslateLanguage>;
  i : Integer;
  LLangInfo : TLanguageInfo;
begin
  LOptions := TAWSOptions.Create;
  LOptions.AccessKeyId := FAccessKey;
  LOptions.SecretAccessKey := FSecretKey;
  LOptions.Region := FRegion;
  LAwsTranslate := TTranslateClient.Create(LOptions);
  try
    FFromLanguages.Clear;
    LLangInfo := TLanguageInfo.Create;
    LLangInfo.LanguageName := 'auto';
    LLangInfo.LanguageCode := 'auto';
    FFromLanguages.Add(LLangInfo);

    LLanglist := LAwsTranslate.ListLanguages.Languages;
    for i := 0 to LLanglist.Count - 1 do
    begin
      LLanguage := LLanglist[i];
      LLangInfo := TLanguageInfo.Create;
      LLangInfo.LanguageName := LLanguage.LanguageName;
      LLangInfo.LanguageCode := LLanguage.LanguageCode;
      FFromLanguages.Add(LLangInfo);
    end;
  finally
    FreeAndNil(LAwsTranslate);
  end;
  Result := FFromLanguages;
end;

function TAmazonTranslate.ToLanguages: TObjectList<TLanguageInfo>;
var
  LAwsTranslate : TTranslateClient;
  LOptions : IAWSOptions;
  LLanguage : ITranslateLanguage;
  LLanglist : TList<ITranslateLanguage>;
  i : Integer;
  LLangInfo : TLanguageInfo;
begin
  LOptions := TAWSOptions.Create;
  LOptions.AccessKeyId := FAccessKey;
  LOptions.SecretAccessKey := FSecretKey;
  LOptions.Region := FRegion;
  LAwsTranslate := TTranslateClient.Create(LOptions);
  try
    FToLanguages.Clear;
    LLanglist := LAwsTranslate.ListLanguages.Languages;
    for i := 0 to LLanglist.Count - 1 do
    begin
      LLanguage := LLanglist[i];
      LLangInfo := TLanguageInfo.Create;
      LLangInfo.LanguageName := LLanguage.LanguageName;
      LLangInfo.LanguageCode := LLanguage.LanguageCode;
      FToLanguages.Add(LLangInfo);
    end;
  finally
    FreeAndNil(LAwsTranslate);
  end;
  Result := FToLanguages;
end;

function TAmazonTranslate.Translate(const SourceText: string; const toLang: string; const fromLang: string): string;
var
  LAwsTranslate : ITranslateClient;
  LResponse : ITranslateTranslateTextResponse;
  LOptions : IAWSOptions;
begin
  LOptions := TAWSOptions.Create;
  LOptions.AccessKeyId := FAccessKey;
  LOptions.SecretAccessKey := FSecretKey;
  LOptions.Region := FRegion;
  LAwsTranslate := TTranslateClient.Create(LOptions);
  LResponse := LAwsTranslate.TranslateText(fromlang, toLang, SourceText);
  Result := LResponse.TranslatedText;
end;

end.
