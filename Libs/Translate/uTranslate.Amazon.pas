unit uTranslate.Amazon;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Hash,
  System.Generics.Collections,
  AWS.Translate,
  AWS.Runtime.ClientConfig,
  AWS.Runtime.Credentials,
  AWS.RegionEndpoints,
  AWS.RegionEndpoint,
  AWS.Internal.IRegionEndpoint,
  uTranslate
  ;

type
  TAmazonTranslate = class(TBaseTranslate)
  strict private
    FAccessKey: string;
    FSecretKey: string;
    FRegion: IRegionEndpointEx;
  public
    constructor Create(const AccessKey, SecretKey: string; Region: string);
    function Translate(const SourceText: string; const toLang: string; const fromLang: string): string; override;
    function FromLanguages: TObjectList<TLanguageInfo>; override;
    function ToLanguages: TObjectList<TLanguageInfo>; override;
  end;

implementation

constructor TAmazonTranslate.Create(const AccessKey, SecretKey: string; Region: string);
begin
  inherited Create('');
  FAccessKey := AccessKey;
  FSecretKey := SecretKey;
  FRegion := TRegionEndpoint.GetEndpoint(Region, '');
end;

function TAmazonTranslate.FromLanguages: TObjectList<TLanguageInfo>;
var
  lang : TLanguageInfo;
begin
  lang := TLanguageInfo.Create;
  lang.LanguageName := 'German';
  lang.LanguageCode := 'de';
  FFromLanguages.Add(lang);

  lang := TLanguageInfo.Create;
  lang.LanguageName := 'English';
  lang.LanguageCode := 'en';
  FFromLanguages.Add(lang);

  lang := TLanguageInfo.Create;
  lang.LanguageName := 'es';
  lang.LanguageCode := 'es';
  FFromLanguages.Add(lang);

  lang := TLanguageInfo.Create;
  lang.LanguageName := 'French';
  lang.LanguageCode := 'fr';
  FFromLanguages.Add(lang);

  lang := TLanguageInfo.Create;
  lang.LanguageName := 'Italian';
  lang.LanguageCode := 'it';
  FFromLanguages.Add(lang);

  lang := TLanguageInfo.Create;
  lang.LanguageName := 'Japanese';
  lang.LanguageCode := 'ja';
  FFromLanguages.Add(lang);

  lang := TLanguageInfo.Create;
  lang.LanguageName := 'Korean';
  lang.LanguageCode := 'ko';
  FFromLanguages.Add(lang);

  lang := TLanguageInfo.Create;
  lang.LanguageName := 'Portuguese';
  lang.LanguageCode := 'pt';
  FFromLanguages.Add(lang);

  lang := TLanguageInfo.Create;
  lang.LanguageName := 'Chinese';
  lang.LanguageCode := 'zh';
  FFromLanguages.Add(lang);

  lang := TLanguageInfo.Create;
  lang.LanguageName := 'Taiwanese';
  lang.LanguageCode := 'zh-TW';
  FFromLanguages.Add(lang);


  Result := FFromLanguages;
end;

function TAmazonTranslate.ToLanguages: TObjectList<TLanguageInfo>;
begin
  Result := FromLanguages;
end;

function TAmazonTranslate.Translate(const SourceText: string; const toLang: string; const fromLang: string): string;
var
  LAwsTranslate : TAmazonTranslateClient;
  LResponse : ITranslateTextResponse;
  Credentials : IAWSCredentials;
  Request : ITranslateTextRequest;
begin
  Credentials  := TBasicAWSCredentials.Create(FAccessKey, FSecretKey);
  LAwsTranslate := TAmazonTranslateClient.Create(Credentials, FRegion);
  Request := TTranslateTextRequest.Create;
  Request.SourceLanguageCode := fromLang;
  Request.TargetLanguageCode := toLang;
  Request.Text := SourceText;
  LResponse := LAwsTranslate.TranslateText(Request);
  Result := LResponse.TranslatedText;
end;

end.
