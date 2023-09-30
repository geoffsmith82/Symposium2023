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
  raise Exception.Create('Not Implemented yet');
end;

function THuggingFaceLLM.Completion(const AQuestion, AModel: string): string;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LJSONPayload: TJSONObject;
begin
  LRESTClient := nil;
  LRESTRequest := nil;
  LRESTResponse := nil;
  LJSONPayload := nil;
  try
    // Create REST components dynamically
    LRESTClient := TRESTClient.Create('https://api-inference.huggingface.co');
    LRESTResponse := TRESTResponse.Create(nil);
    LRESTRequest := TRESTRequest.Create(nil);

    LRESTRequest.Client := LRESTClient;
    LRESTRequest.Response := LRESTResponse;

    LRESTRequest.Resource := 'models/{models}';
    LRESTRequest.AddParameter('models', AModel, TRESTRequestParameterKind.pkURLSEGMENT);
    LRESTRequest.Method := rmPOST;
    LRESTRequest.Accept := 'application/json';
    // Authorization header
    LRESTRequest.AddParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);

    // Construct the JSON payload
    LJSONPayload := TJSONObject.Create;
    try
      LJSONPayload.AddPair('inputs', AQuestion);
      LRESTRequest.AddBody(LJSONPayload.ToString, ctAPPLICATION_JSON);

      // Execute request
      LRESTRequest.Execute;

      if LRESTResponse.StatusCode = 200 then
      begin
        // Here, I'm assuming the response is just a plain text. If it's a JSON, you'll need to parse it accordingly.
        Result := LRESTResponse.Content;
      end
      else
      begin
        Result := 'Error: ' + LRESTResponse.StatusCode.ToString + ' ' + LRESTResponse.StatusText;
      end;

    finally
      LJSONPayload.Free;
    end;

  finally
    LRESTRequest.Free;
    LRESTResponse.Free;
    LRESTClient.Free;
  end;
end;

function THuggingFaceLLM.GetModelInfo: TObjectList<TBaseModelInfo>;
var
  LModel: TBaseModelInfo;
begin
  {TODO: Convert to a DB call or REST API}

  FModelInfo.Clear;
  LModel := TBaseModelInfo.Create;
  LModel.ModelName := 'gpt2';
  FModelInfo.Add(LModel);

  LModel := TBaseModelInfo.Create;
  LModel.ModelName := 'EleutherAI/gpt-j-6b';
  FModelInfo.Add(LModel);

  LModel := TBaseModelInfo.Create;
  LModel.ModelName := 'TheBloke/Wizard-Vicuna-30B-Uncensored-GPTQ';
  FModelInfo.Add(LModel);


  Result := FModelInfo;

end;

end.
