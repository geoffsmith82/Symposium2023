unit uReplicate.LLM;

interface

uses
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  System.JSON,
  System.SysUtils,
  uLLM
  ;

type
  TReplicateLLM = class(TBaseLLM)
  protected
    function GetModelInfo: TObjectList<TBaseModelInfo>; override;
  public
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion: string; const AModel: string): string; override;
  end;

implementation

{ TAnthropic }

function TReplicateLLM.ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse;
begin
  raise Exception.Create('Not Implemented Yet!');
end;

function TReplicateLLM.Completion(const AQuestion, AModel: string): string;
begin
  raise Exception.Create('Not Implemented Yet!');
end;

function TReplicateLLM.GetModelInfo: TObjectList<TBaseModelInfo>;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  JSONValue: TJSONObject;
  JSONVersion: TJSONObject;
  JSONArray: TJSONArray;
  I: Integer;
  ModelInfo: TBaseModelInfo;
begin
  FModelInfo.Clear;

  RestClient := TRESTClient.Create('https://api.replicate.com');
  try
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    try
      RestRequest.Client := RestClient;
      RestRequest.Response := RestResponse;
      RestRequest.Resource := '/v1/collections/streaming-language-models';

      RestRequest.Method := rmGET;

      RestRequest.AddParameter('Authorization', 'Token ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);

      RestRequest.Execute;

      if RestResponse.StatusCode = 200 then
      begin
        JSONValue := TJSONObject.ParseJSONValue(RestResponse.Content) as TJSONObject;

        if Assigned(JSONValue) and Assigned(JSONValue.GetValue('models')) then
        begin
          JSONArray := JSONValue.GetValue('models') as TJSONArray;
          for I := 0 to JSONArray.Count - 1 do
          begin
            ModelInfo := TBaseModelInfo.Create;
            ModelInfo.modelName := JSONArray.Items[I].GetValue<string>('name');
            JSONVersion := JSONArray.Items[I].GetValue<TJSONObject>('latest_version');
            if Assigned(JSONVersion.GetValue('id')) then
              ModelInfo.version := JSONVersion.GetValue<string>('id');
            FModelInfo.Add(ModelInfo);
          end;
        end;

        FreeAndNil(JSONValue);
      end
      else
      begin
        // Handle errors or exceptions if needed
      end;

    finally
      RestRequest.Free;
      RestResponse.Free;
    end;

  finally
    RestClient.Free;
  end;
  Result := FModelInfo;
end;

end.
