unit uGoogleCustomSearch;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.JSON,
  REST.Client,
  REST.Types
  ;

type
  TGoogleSearchItem = record
    Title: string;
    Link: string;
    Snippet: string;
  end;

  TGoogleSearchInformation = record
    searchTime : Double;
    totalResults : Int64;
  end;

  TGoogleSearchResult = record
    Items: TArray<TGoogleSearchItem>;
    SearchInformation: TGoogleSearchInformation;
  end;

  TGoogleCustomSearch = class
  private
    FApiKey: string;
    FCx: string;
    FRestClient: TRESTClient;
    FRestRequest: TRESTRequest;
    FRestResponse: TRESTResponse;
  public
    constructor Create(const AApiKey, ACx: string);
    destructor Destroy; override;
    function Search(const AQuery: string; AStart: Integer = 1): TGoogleSearchResult;
  end;

implementation


constructor TGoogleCustomSearch.Create(const AApiKey, ACx: string);
begin
  inherited Create;
  FApiKey := AApiKey;
  FCx := ACx;

  FRestClient := TRESTClient.Create('https://www.googleapis.com/customsearch/v1');
  FRestRequest := TRESTRequest.Create(nil);
  FRestResponse := TRESTResponse.Create(nil);

  FRestRequest.Client := FRestClient;
  FRestRequest.Response := FRestResponse;
  FRestRequest.Method := TRESTRequestMethod.rmGET;
end;

destructor TGoogleCustomSearch.Destroy;
begin
  FreeAndNil(FRestRequest);
  FreeAndNil(FRestResponse);
  FreeAndNil(FRestClient);

  inherited Destroy;
end;

function TGoogleCustomSearch.Search(const AQuery: string; AStart: Integer = 1): TGoogleSearchResult;
var
  LJsonResult, LJsonItem: TJSONObject;
  LJsonSearchInformation: TJSONObject;
  LJsonItems: TJSONArray;
  LSearchItem: TGoogleSearchItem;
  i : Integer;
begin
  FRestRequest.Params.Clear;

  FRestRequest.AddParameter('key', FApiKey, TRESTRequestParameterKind.pkGETorPOST);
  FRestRequest.AddParameter('cx', FCx, TRESTRequestParameterKind.pkGETorPOST);
  FRestRequest.AddParameter('q', AQuery, TRESTRequestParameterKind.pkGETorPOST);
  FRestRequest.AddParameter('start', AStart.ToString, TRESTRequestParameterKind.pkGETorPOST);

  FRestRequest.Execute;

  if FRestResponse.StatusCode = 200 then
  begin
    LJsonResult := TJSONObject.ParseJSONValue(FRestResponse.JSONText) as TJSONObject;
    LJsonSearchInformation := LJsonResult.GetValue<TJSONObject>('searchInformation');
    try
      LJsonItems := LJsonResult.GetValue<TJSONArray>('items');
      SetLength(Result.Items, LJsonItems.Count);

      for i := 0 to LJsonItems.Count - 1 do
      begin
          LJsonItem := LJsonItems[i] as TJSONObject;
          LSearchItem.Title := LJsonItem.GetValue<string>('title');
          LSearchItem.Link := LJsonItem.GetValue<string>('link');
          LSearchItem.Snippet := LJsonItem.GetValue<string>('snippet');
          Result.Items[i] := LSearchItem;
      end;

      LJsonSearchInformation.GetValue('searchTime', Result.SearchInformation.searchTime);
      LJsonSearchInformation.GetValue('totalResults', Result.SearchInformation.totalResults);
    finally
      FreeAndNil(LJsonResult);
    end;
  end
  else
    raise Exception.CreateFmt('Error: %d %s', [FRestResponse.StatusCode, FRestResponse.StatusText]);
end;

end.
