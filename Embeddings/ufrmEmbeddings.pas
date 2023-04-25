unit ufrmEmbeddings;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  uGoogleCustomSearch,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.PG,
  FireDAC.Phys.PGDef,
  FireDAC.VCLUI.Wait,
  Data.DB,
  FireDAC.Comp.Client,
  OpenAI
  ;

type
  TEmbeddingMatch = record
    Index: Integer;
    Distance: Double;
  end;


  TfrmEmbeddings = class(TForm)
    btnGoogleSearch: TButton;
    Memo1: TMemo;
    FDConnection1: TFDConnection;
    FDPhysPgDriverLink1: TFDPhysPgDriverLink;
    btnEmbeddings: TButton;
    procedure btnGoogleSearchClick(Sender: TObject);
    procedure btnEmbeddingsClick(Sender: TObject);
  private
    function GetQuestionsArray: TArray<string>;
    function GetCompareQuestionsArray: TArray<string>;
    procedure DisplayMatches(const closestMatches: TArray<TEmbeddingMatch>; const questions: TArray<string>);
    function FindClosestMatches(const compareEmbedding: TArray<Double>;
      const questions: TArray<string>;
      const embeddings: TArray<TArray<Double>>): TArray<TEmbeddingMatch>;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEmbeddings: TfrmEmbeddings;

implementation

{$R *.dfm}

{$I ..\Libs\apikey.inc}

procedure TfrmEmbeddings.btnGoogleSearchClick(Sender: TObject);
const
  API_KEY = google_custom_search_key;
  CX = google_custom_search_cx;
var
  GoogleCustomSearch: TGoogleCustomSearch;
  SearchResult: TGoogleSearchResult;
  Item: TGoogleSearchItem;
begin
  GoogleCustomSearch := TGoogleCustomSearch.Create(API_KEY, CX);
  try
    SearchResult := GoogleCustomSearch.Search('OpenAI');
    for Item in SearchResult.Items do
    begin
      // Process the search result items as needed, e.g., display them in a ListView or Memo
      Memo1.Lines.Add(Item.Title);
    end;
  finally
    FreeAndNil(GoogleCustomSearch);
  end;
end;

function TfrmEmbeddings.GetQuestionsArray: TArray<string>;
begin
  Result := TArray<string>.Create(
    'What is the time',
    'What is the date',
    'How much tax will I pay for 2022 if I earn 100,000 dollars',
    'What is the weather like tomorrow',
    'Do you have a cat?',
    'What is the capital of France?',
    'What is the largest continent?',
    'What is the population of China?',
    'Who was the first president of the United States?',
    'What is the meaning of life?',
    'How do I cook a perfect steak?',
    'What is the difference between a crocodile and an alligator?',
    'What is the best way to lose weight?',
    'What is the distance between the Earth and the Moon?',
    'What is the boiling point of water?',
    'What is the Pythagorean theorem?',
    'What is the value of pi?',
    'What is the atomic number of carbon?',
    'What is the chemical formula for water?',
    'What is the meaning of the word "epiphany"?'
    // add more questions here ...
  );
end;
function TfrmEmbeddings.GetCompareQuestionsArray: TArray<string>;
begin
  Result := TArray<string>.Create(
    'What is the temperature outside?',
    'What is the current time in Tokyo?',
    'What is the population of Brazil?',
    'What is the distance between the Earth and the Sun?',
    'What is the capital of India?',
    'How much tax will I have to pay if I earn 120000 dollars'
    // add more compare questions here ...
  );
end;
procedure TfrmEmbeddings.btnEmbeddingsClick(Sender: TObject);
var
  questions : TArray<string>;
  compareQuestions : TArray<string>;
  embeddingsFromDB : TArray<TArray<Double>>;
  compareQuestionEmbeddings : TArray<TArray<Double>>;
  i: Integer;
  closestMatches : TArray<TArray<TEmbeddingMatch>>;
begin
  questions := GetQuestionsArray;
  embeddingsFromDB := TOpenAI.Embeddings(questions);
  compareQuestions := GetCompareQuestionsArray;
  compareQuestionEmbeddings := TOpenAI.Embeddings(compareQuestions);
  Memo1.Lines.Clear;
  SetLength(closestMatches, Length(compareQuestions));
  // Loop through each compare question
  for i := 0 to Length(compareQuestions) - 1 do
  begin
    Memo1.Lines.Add('Compare Question: ' + compareQuestions[i]);
    // Find top 3 closest matches for current compare question
    closestMatches[i] := FindClosestMatches(compareQuestionEmbeddings[i], questions, embeddingsFromDB);
    // Display closest matches for current compare question
    DisplayMatches(closestMatches[i], questions);
  end;
end;

function TfrmEmbeddings.FindClosestMatches(const compareEmbedding: TArray<Double>; const questions: TArray<string>; const embeddings: TArray<TArray<Double>>): TArray<TEmbeddingMatch>;
var
  i: Integer;
  distances : TArray<Double>;
  closestMatches : TArray<TEmbeddingMatch>;
begin
  SetLength(distances, Length(questions));
  for i := 0 to Length(questions) - 1 do
  begin
    distances[i] := CosineDistance(compareEmbedding, embeddings[i]);
  end;
  SetLength(closestMatches, 3);
  for i := 0 to 2 do
  begin
    closestMatches[i].Index := -1;
    closestMatches[i].Distance := MaxDouble;
  end;
  for i := 0 to Length(questions) - 1 do
  begin
    for var j := 0 to 2 do
    begin
      if (distances[i] < closestMatches[j].Distance) then
      begin
        closestMatches[j].Index := i;
        closestMatches[j].Distance := distances[i];
        break;
      end;
    end;
  end;
  Result := closestMatches;
end;

procedure TfrmEmbeddings.DisplayMatches(const closestMatches: TArray<TEmbeddingMatch>; const questions: TArray<string>);
var
  i: Integer;
begin
  Memo1.Lines.Add('Closest Matches:');
  for i := 0 to 2 do
  begin
    if (closestMatches[i].Index <> -1) then
    begin
      Memo1.Lines.Add(closestMatches[i].Distance.ToString + ' | ' + questions[closestMatches[i].Index]);
    end;
  end;
  Memo1.Lines.Add('---');
end;

end.
