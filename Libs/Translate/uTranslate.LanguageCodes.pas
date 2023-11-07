unit uTranslate.LanguageCodes;

interface

function GetLanguageNameFromCode(const LanguageCode: string): string;

implementation

uses
  SysUtils;

type
  TLanguageInfo = record
    Code: string;
    Name: string;
  end;

const
  LanguageList: array[0..111] of TLanguageInfo = (
    (Code: 'af'; Name: 'Afrikaans'),
    (Code: 'ak'; Name: 'Akan'),
    (Code: 'as'; Name: 'Assamese'),
    (Code: 'sq'; Name: 'Albanian'),
    (Code: 'am'; Name: 'Amharic'),
    (Code: 'ar'; Name: 'Arabic'),
    (Code: 'ay'; Name: 'Aymara'),
    (Code: 'hy'; Name: 'Armenian'),
    (Code: 'az'; Name: 'Azerbaijani'),
    (Code: 'eu'; Name: 'Basque'),
    (Code: 'be'; Name: 'Belarusian'),
    (Code: 'bn'; Name: 'Bengali'),
    (Code: 'bs'; Name: 'Bosnian'),
    (Code: 'bg'; Name: 'Bulgarian'),
    (Code: 'bm'; Name: 'Bambara'),
    (Code: 'ca'; Name: 'Catalan'),
    (Code: 'ceb'; Name: 'Cebuano'),
    (Code: 'ny'; Name: 'Chichewa'),
    (Code: 'zh'; Name: 'Chinese'),
    (Code: 'co'; Name: 'Corsican'),
    (Code: 'hr'; Name: 'Croatian'),
    (Code: 'cs'; Name: 'Czech'),
    (Code: 'da'; Name: 'Danish'),
    (Code: 'dv'; Name: 'Divehi; Dhivehi; Maldivian'),
    (Code: 'nl'; Name: 'Dutch'),
    (Code: 'en'; Name: 'English'),
    (Code: 'ee'; Name: 'Ewe'),
    (Code: 'eo'; Name: 'Esperanto'),
    (Code: 'et'; Name: 'Estonian'),
    (Code: 'tl'; Name: 'Filipino'),
    (Code: 'fi'; Name: 'Finnish'),
    (Code: 'fr'; Name: 'French'),
    (Code: 'fy'; Name: 'Frisian'),
    (Code: 'gl'; Name: 'Galician'),
    (Code: 'ka'; Name: 'Georgian'),
    (Code: 'de'; Name: 'German'),
    (Code: 'el'; Name: 'Greek'),
    (Code: 'gu'; Name: 'Gujarati'),
    (Code: 'gn'; Name: 'Guarani'),
    (Code: 'ht'; Name: 'Haitian Creole'),
    (Code: 'ha'; Name: 'Hausa'),
    (Code: 'haw'; Name: 'Hawaiian'),
    (Code: 'iw'; Name: 'Hebrew'),
    (Code: 'he'; Name: 'Hebrew'),
    (Code: 'hi'; Name: 'Hindi'),
    (Code: 'hmn'; Name: 'Hmong'),
    (Code: 'hu'; Name: 'Hungarian'),
    (Code: 'is'; Name: 'Icelandic'),
    (Code: 'ig'; Name: 'Igbo'),
    (Code: 'id'; Name: 'Indonesian'),
    (Code: 'ga'; Name: 'Irish'),
    (Code: 'it'; Name: 'Italian'),
    (Code: 'ja'; Name: 'Japanese'),
    (Code: 'jw'; Name: 'Javanese'),
    (Code: 'jv'; Name: 'Javanese'),
    (Code: 'kn'; Name: 'Kannada'),
    (Code: 'kk'; Name: 'Kazakh'),
    (Code: 'km'; Name: 'Khmer'),
    (Code: 'ko'; Name: 'Korean'),
    (Code: 'ku'; Name: 'Kurdish (Kurmanji)'),
    (Code: 'ky'; Name: 'Kyrgyz'),
    (Code: 'lo'; Name: 'Lao'),
    (Code: 'la'; Name: 'Latin'),
    (Code: 'lv'; Name: 'Latvian'),
    (Code: 'lt'; Name: 'Lithuanian'),
    (Code: 'lb'; Name: 'Luxembourgish'),
    (Code: 'mk'; Name: 'Macedonian'),
    (Code: 'mg'; Name: 'Malagasy'),
    (Code: 'ms'; Name: 'Malay'),
    (Code: 'ml'; Name: 'Malayalam'),
    (Code: 'mt'; Name: 'Maltese'),
    (Code: 'mi'; Name: 'Maori'),
    (Code: 'mr'; Name: 'Marathi'),
    (Code: 'mn'; Name: 'Mongolian'),
    (Code: 'my'; Name: 'Myanmar (Burmese)'),
    (Code: 'ne'; Name: 'Nepali'),
    (Code: 'no'; Name: 'Norwegian'),
    (Code: 'ps'; Name: 'Pashto'),
    (Code: 'fa'; Name: 'Persian'),
    (Code: 'pl'; Name: 'Polish'),
    (Code: 'pt'; Name: 'Portuguese'),
    (Code: 'pa'; Name: 'Punjabi'),
    (Code: 'ro'; Name: 'Romanian'),
    (Code: 'ru'; Name: 'Russian'),
    (Code: 'sm'; Name: 'Samoan'),
    (Code: 'gd'; Name: 'Scots Gaelic'),
    (Code: 'sr'; Name: 'Serbian'),
    (Code: 'st'; Name: 'Sesotho'),
    (Code: 'sn'; Name: 'Shona'),
    (Code: 'sd'; Name: 'Sindhi'),
    (Code: 'si'; Name: 'Sinhala'),
    (Code: 'sk'; Name: 'Slovak'),
    (Code: 'sl'; Name: 'Slovenian'),
    (Code: 'so'; Name: 'Somali'),
    (Code: 'es'; Name: 'Spanish'),
    (Code: 'su'; Name: 'Sundanese'),
    (Code: 'sw'; Name: 'Swahili'),
    (Code: 'sv'; Name: 'Swedish'),
    (Code: 'tg'; Name: 'Tajik'),
    (Code: 'ta'; Name: 'Tamil'),
    (Code: 'te'; Name: 'Telugu'),
    (Code: 'th'; Name: 'Thai'),
    (Code: 'tr'; Name: 'Turkish'),
    (Code: 'uk'; Name: 'Ukrainian'),
    (Code: 'ur'; Name: 'Urdu'),
    (Code: 'uz'; Name: 'Uzbek'),
    (Code: 'vi'; Name: 'Vietnamese'),
    (Code: 'cy'; Name: 'Welsh'),
    (Code: 'xh'; Name: 'Xhosa'),
    (Code: 'yi'; Name: 'Yiddish'),
    (Code: 'yo'; Name: 'Yoruba'),
    (Code: 'zu'; Name: 'Zulu')
  );

function GetLanguageNameFromCode(const LanguageCode: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(LanguageList) to High(LanguageList) do
  begin
    if SameText(LanguageCode, LanguageList[I].Code) then
    begin
      Result := LanguageList[I].Name;
      Break;
    end;
  end;
  if Result.IsEmpty then
  begin
    Result := '[' + LanguageCode + ']';
  end;
end;

end.

