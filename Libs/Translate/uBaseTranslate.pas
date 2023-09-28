unit uBaseTranslate;

interface

uses
  Classes;

type
  TLanguageInfo = class
    LanguageName: string;
    LanguageCode: string;
  end;

  TBaseTranslate = class abstract
  public
    function FromLanguages: TArray<TLanguageInfo>; virtual; abstract;
    function ToLanguages: TArray<TLanguageInfo>; virtual; abstract;
    function Translate(const SourceText: string; const toLang: string; const fromLang: string): string; virtual; abstract;
  end;

implementation

{ TBaseTranslate }

end.
