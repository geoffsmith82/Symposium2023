unit uHuggingFace.LLM;

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
  THuggingFaceLLM = class(TBaseLLM)
  protected
    function GetModelInfo: TObjectList<TBaseModelInfo>; override;
  public
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion: string; const AModel: string): string; override;
  end;

implementation

{ THuggingFaceLLM }

function THuggingFaceLLM.ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse;
begin

end;

function THuggingFaceLLM.Completion(const AQuestion, AModel: string): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONPayload: TJSONObject;
begin
  // Create REST components dynamically
  RESTClient := TRESTClient.Create('https://api-inference.huggingface.co');
  RESTResponse := TRESTResponse.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);

  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;

    RESTRequest.Resource := 'models/{models}';
    RESTRequest.AddParameter('models', AModel, TRESTRequestParameterKind.pkURLSEGMENT);
    RESTRequest.Method := rmPOST;
    RESTRequest.Accept := 'application/json';

    // Authorization header
    RESTRequest.AddParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);

    // Construct the JSON payload
    JSONPayload := TJSONObject.Create;
    try
      JSONPayload.AddPair('inputs', AQuestion);
      RESTRequest.AddBody(JSONPayload.ToString, ctAPPLICATION_JSON);

      // Execute request
      RESTRequest.Execute;

      if RESTResponse.StatusCode = 200 then
      begin
        // Here, I'm assuming the response is just a plain text. If it's a JSON, you'll need to parse it accordingly.
        Result := RESTResponse.Content;
      end
      else
      begin
        Result := 'Error: ' + RESTResponse.StatusCode.ToString + ' ' + RESTResponse.StatusText;
      end;

    finally
      JSONPayload.Free;
    end;

  finally
    RESTRequest.Free;
    RESTResponse.Free;
    RESTClient.Free;
  end;
end;

function THuggingFaceLLM.GetModelInfo: TObjectList<TBaseModelInfo>;
var
  model: TBaseModelInfo;
begin
  {TODO: Convert to a DB call or REST API}

  FModelInfo.Clear;
  model := TBaseModelInfo.Create;
  model.ModelName := 'gpt2';
  FModelInfo.Add(model);

  model := TBaseModelInfo.Create;
  model.ModelName := 'EleutherAI/gpt-j-6b';
  FModelInfo.Add(model);

  model := TBaseModelInfo.Create;
  model.ModelName := 'TheBloke/Wizard-Vicuna-30B-Uncensored-GPTQ';
  FModelInfo.Add(model);


  Result := FModelInfo;

end;

end.
