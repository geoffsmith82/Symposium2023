unit ufrmEmbeddings;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  System.JSON,
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
  OpenAI,
  uGPT
  ;

type
  TEmbeddingMatch = record
    Index: Integer;
    Distance: Double;
  end;


  TfrmEmbeddings = class(TForm)
    btnGoogleSearch: TButton;
    Memo1: TMemo;
    btnEmbeddings: TButton;
    Button1: TButton;
    Button2: TButton;
    Memo2: TMemo;
    btnQuery: TButton;
    procedure btnGoogleSearchClick(Sender: TObject);
    procedure btnEmbeddingsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnQueryClick(Sender: TObject);
  private
    FOpenAI : TOpenAI;
    function GetQuestionsArray: TArray<string>;
    function GetCompareQuestionsArray: TArray<string>;
    procedure DisplayMatches(const closestMatches: TArray<TEmbeddingMatch>; const questions: TArray<string>);
    function FindClosestMatches(const compareEmbedding: TEmbedding;
      const questions: TArray<string>;
      const embeddings: TEmbeddings): TArray<TEmbeddingMatch>;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEmbeddings: TfrmEmbeddings;

implementation

{$R *.dfm}

uses udmEmbeddings;

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

procedure TfrmEmbeddings.btnQueryClick(Sender: TObject);
var
  questionEmbedding : TEmbeddings;
  prompt : TPrompt;
  chatSettings : TChatSettings;
  chatMessages : TObjectList<TChatMessage>;
  chatMessage : TChatMessage;
  chatResponse : TChatResponse;
  sections : TArray<string>;
begin
  questionEmbedding := FOpenAI.Embeddings([Memo2.Lines.Text]);
  prompt := TPrompt.Create(
                           '{question}' + sLineBreak + sLineBreak +
                           'CONTEXT' + sLineBreak +
                           '{embedding1}' + sLineBreak + sLineBreak +
                           '{embedding2}' + sLineBreak);
  chatSettings.model := 'gpt-4';
  chatMessages := TObjectList<TChatMessage>.Create;
  try
    chatMessage := TChatMessage.Create;
    chatMessage.Role := 'System';
    chatMessage.Content := 'Answer the following question using the context following the question';
    chatMessages.Add(chatMessage);
    chatMessage := TChatMessage.Create;
    chatMessage.Role := 'User';
    sections := dmEmbeddings.LookupSections(questionEmbedding[0], 2);

    prompt.Parameters['question'] := Memo2.Lines.Text;
    prompt.Parameters['embedding1'] := sections[0];
    prompt.Parameters['embedding2'] := sections[1];


    chatMessage.Content := prompt.AsString;
    chatMessages.Add(chatMessage);
    chatSettings.max_tokens := 2500;
    chatSettings.n := 1;

    chatResponse := FOpenAI.SendChatMessagesToOpenAI(chatSettings, chatMessages);

    Memo1.Lines.Add(chatResponse.Content);
  finally
    FreeAndNil(prompt);
    FreeAndNil(chatMessages);
  end;
end;

procedure TfrmEmbeddings.Button2Click(Sender: TObject);
var
  sl : TStringList;
  sections : TArray<string>;
  i: Integer;
  j : Integer;
  embeddings : TEmbeddings;
  json :TJSONObject;
begin
  sl := nil;
  try
    j := 0;
    sl := TStringList.Create;
    sl.LoadFromFile('D:\Programming\ADUG\Symposium2023\Embeddings\inputText.txt');
    SetLength(sections, sl.Count);
    for i := 0 to sl.Count - 1 do
    begin
      sections[j] := sections[j] + sl[i] + System.sLineBreak;
      if Length(sections[j]) > 2048 then
      begin
        Inc(j);
      end;
    end;
    SetLength(sections, j);
    embeddings := FOpenAI.Embeddings(sections);
    json := TJSONObject.Create;
    json.AddPair('filename', 'inputText.txt');
    dmEmbeddings.AddDocument(embeddings, sections, 'inputText.txt', json);
  finally
    FreeAndNil(sl);
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
  embeddingsFromDB : TEmbeddings;
  compareQuestionEmbeddings : TEmbeddings;
  i: Integer;
  closestMatches : TArray<TArray<TEmbeddingMatch>>;
begin
  questions := GetQuestionsArray;
  embeddingsFromDB := FOpenAI.Embeddings(questions);
  compareQuestions := GetCompareQuestionsArray;
  compareQuestionEmbeddings := FOpenAI.Embeddings(compareQuestions);
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

function TfrmEmbeddings.FindClosestMatches(const compareEmbedding: TEmbedding;
      const questions: TArray<string>;
      const embeddings: TEmbeddings): TArray<TEmbeddingMatch>;
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

procedure TfrmEmbeddings.FormCreate(Sender: TObject);
begin
  FOpenAI := TOpenAI.Create(chatgpt_apikey);
end;

procedure TfrmEmbeddings.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FOpenAI);
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
