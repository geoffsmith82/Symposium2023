unit CodeExtractor;

interface

uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions,
  System.Generics.Collections;

type
  TCodeBlock = record
    Language: string;
    Code: string;
    FileName: string;
  end;

  TCodeExtractor = class
  private
    FOutput: string;
    FCodeBlocks: TArray<TCodeBlock>;
    function ExtractFileName(const ACode: string): string;
    procedure ExtractCodeBlocks;
  public
    constructor Create(const AOutput: string);
    function CodeBlockCount: Integer;
    property CodeBlocks: TArray<TCodeBlock> read FCodeBlocks;
  end;

implementation

{ TCodeExtractor }

constructor TCodeExtractor.Create(const AOutput: string);
begin
  FOutput := AOutput;
  ExtractCodeBlocks;
end;

function TCodeExtractor.ExtractFileName(const ACode: string): string;
var
  FirstLine: string;
begin
  Result := '';
  FirstLine := Trim(ACode);
  if FirstLine.StartsWith('program', True) then
    Result := Copy(FirstLine, Pos(' ', FirstLine) + 1, Length(FirstLine)) + '.dpr'
  else if FirstLine.StartsWith('unit', True) then
    Result := Copy(FirstLine, Pos(' ', FirstLine) + 1, Length(FirstLine)) + '.pas';
end;

procedure TCodeExtractor.ExtractCodeBlocks;
var
  Regex: TRegEx;
  Matches: TMatchCollection;
  Match: TMatch;
  CodeBlock: TCodeBlock;
  CodeList: TList<TCodeBlock>;
  CodeLines: TStringList;
begin
  CodeList := TList<TCodeBlock>.Create;
  try
    Regex := TRegEx.Create('```(\w+)?\s*([\s\S]*?)```', [roMultiLine]);
    Matches := Regex.Matches(FOutput);
    for Match in Matches do
    begin
      CodeBlock.Language := Match.Groups[1].Value;
      CodeBlock.Code := Match.Groups[2].Value;

      if SameText(CodeBlock.Language, 'delphi') then
      begin
        CodeLines := TStringList.Create;
        try
          CodeLines.Text := CodeBlock.Code;
          if CodeLines.Count > 0 then
            CodeBlock.FileName := ExtractFileName(CodeLines[0]);
        finally
          CodeLines.Free;
        end;
      end;

      CodeList.Add(CodeBlock);
    end;
    FCodeBlocks := CodeList.ToArray;
  finally
    CodeList.Free;
  end;
end;

function TCodeExtractor.CodeBlockCount: Integer;
begin
  Result := Length(FCodeBlocks);
end;

end.

