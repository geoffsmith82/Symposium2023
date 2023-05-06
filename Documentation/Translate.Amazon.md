# TAmazonTranslate Class Documentation

The `TAmazonTranslate` class is a wrapper around the Amazon Translate API, allowing users to translate text between supported languages. This class inherits from the `TBaseTranslate` class and provides methods for translating text, as well as getting supported source and target languages.

## TAmazonTranslate Methods

### Constructor

```pascal
constructor Create(const AccessKey, SecretKey, Endpoint: string);
```

Create a new `TAmazonTranslate` object with the given Amazon Translate API `AccessKey`, `SecretKey`, and `Endpoint`.

### Translate

```pascal
function Translate(const SourceText: string; const toLang: string; const fromLang: string): string; override;
```

Translate the `SourceText` from the source language `fromLang` to the target language `toLang`. Returns the translated text as a string.

### FromLanguages

```pascal
function FromLanguages: TArray<string>; override;
```

Get an array of supported source languages for translation. Each language is represented by its language code (e.g., 'en' for English, 'es' for Spanish).

### ToLanguages

```pascal
function ToLanguages: TArray<string>; override;
```

Get an array of supported target languages for translation. Each language is represented by its language code (e.g., 'en' for English, 'es' for Spanish).

## Example Usage

Here's an example of how to use the `TAmazonTranslate` class to translate text:

```pascal
var
  AmazonTranslate: TAmazonTranslate;
  SourceText: string;
  TranslatedText: string;

begin
  // Initialize TAmazonTranslate
  AmazonTranslate := TAmazonTranslate.Create('your-access-key', 'your-secret-key', 'https://translate.us-east-1.amazonaws.com');
  try
    // Define the source text and translate it
    SourceText := 'Hello, how are you?';
    TranslatedText := AmazonTranslate.Translate(SourceText, 'es', 'en');

    // Print the translated text
    WriteLn('Original text: ' + SourceText);
    WriteLn('Translated text: ' + TranslatedText);
  finally
    AmazonTranslate.Free;
  end;
end;
```