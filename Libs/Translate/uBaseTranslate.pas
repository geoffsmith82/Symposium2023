unit uBaseTranslate;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  Classes;

type
  TLanguageInfo = class
    LanguageName: string;
    LanguageCode: string;
  end;

  TBaseTranslate = class abstract
  protected
    FAPIKey: string;
    FFromLanguages: TObjectList<TLanguageInfo>;
    FToLanguages: TObjectList<TLanguageInfo>;
  public
    function FromLanguages: TObjectList<TLanguageInfo>; virtual; abstract;
    function ToLanguages: TObjectList<TLanguageInfo>; virtual; abstract;
    function Translate(const SourceText: string; const toLang: string; const fromLang: string): string; virtual; abstract;
    constructor Create(APIKey: string);
    destructor Destroy; override;
  end;

implementation

{ TBaseTranslate }

{ TBaseTranslate }

constructor TBaseTranslate.Create(APIKey: string);
begin
  FAPIKey := APIKey;
  FFromLanguages := TObjectList<TLanguageInfo>.Create;
  FToLanguages := TObjectList<TLanguageInfo>.Create;
end;

destructor TBaseTranslate.Destroy;
begin
  FreeAndNil(FFromLanguages);
  FreeAndNil(FToLanguages);
  inherited;
end;

end.
