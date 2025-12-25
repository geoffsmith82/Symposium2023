unit uLLM.Streaming.OpenAI;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.JSON,
  System.Threading,
  uLLM;

type
  TStreamTokenEvent = reference to procedure(Sender: TObject; const AText: string);

  TOpenAIStreamer = class(TThread)
  private
    FApiKey: string;
    FModel: string;
    FMessages: TObjectList<TChatMessage>;
    FOnToken: TStreamTokenEvent;
    FOnCurrentResponse: TStreamTokenEvent;
    procedure DoCurrentResponse(const AText: string);
  protected
    procedure Execute; override;
    procedure DoToken(const AText: string);
  public
    constructor Create(const AApiKey, AModel: string;  AMessages: TObjectList<TChatMessage>);
    property OnCurrentResponse: TStreamTokenEvent read FOnCurrentResponse write FOnCurrentResponse;
    property OnToken: TStreamTokenEvent read FOnToken write FOnToken;
  end;

implementation

{ TOpenAIStreamer }

constructor TOpenAIStreamer.Create(const AApiKey, AModel: string; AMessages: TObjectList<TChatMessage>);
begin
  inherited Create(True); // start suspended
  FreeOnTerminate := True;
  FApiKey := AApiKey;
  FModel := AModel;
  FMessages := AMessages;
end;

procedure TOpenAIStreamer.DoToken(const AText: string);
begin
  if Assigned(FOnToken) then
    TThread.Synchronize(nil,
      procedure
      begin
        FOnToken(Self, AText);
      end);
end;

procedure TOpenAIStreamer.DoCurrentResponse(const AText: string);
begin
  if Assigned(FOnToken) then
    TThread.Synchronize(nil,
      procedure
      begin
        FOnCurrentResponse(Self, AText);
      end);
end;


procedure TOpenAIStreamer.Execute;
var
  HttpClient: THTTPClient;
  HttpRequest: IHTTPRequest;
  HttpResponse: IHTTPResponse;
  ReqStream: TStringStream;
  RespStream: TStream;
  Buffer: TBytes;
  ReadCount: Integer;
  Line, Partial: string;
  LJSONMessages : TJSONArray;
  LMessage: TChatMessage;
begin
  HttpClient := THTTPClient.Create;
  try
    HttpRequest := HttpClient.GetRequest('POST', 'https://api.openai.com/v1/chat/completions');
    HttpRequest.AddHeader('Authorization', 'Bearer ' + FApiKey);
    HttpRequest.AddHeader('Content-Type', 'application/json');

    var reqMsg: TJSONObject;
    reqMsg := TJSONObject.Create;
    try
      reqMsg.AddPair('model', FModel);
      LJSONMessages := TJSONArray.Create;
      for LMessage in FMessages do
      begin
        LJSONMessages.AddElement(LMessage.AsJSON);
      end;


      reqMsg.AddPair('messages', LJSONMessages);
      reqMsg.AddPair('stream', TJSONBool.Create(True));


      ReqStream := TStringStream.Create(reqMsg.ToJSON, TEncoding.UTF8);
    finally
      FreeAndNil(reqMsg);
    end;

    try
      HttpRequest.SourceStream := ReqStream;
      RespStream := HttpClient.Execute(HttpRequest).ContentStream;
      try
        SetLength(Buffer, 1024);
        Partial := '';

        repeat
          ReadCount := RespStream.Read(Buffer, Length(Buffer));
          if ReadCount > 0 then
          begin
            Line := TEncoding.UTF8.GetString(Buffer, 0, ReadCount);
            Partial := Partial + Line;
        //    DoCurrentResponse(Line);
            // Process line by line
            while Partial.Contains(#10) do
            begin
              var P := Pos(#10, Partial);
              var Chunk := Trim(Copy(Partial, 1, P - 1));
              Delete(Partial, 1, P);

              if Chunk.StartsWith('data: ') then
              begin
                var JsonChunk := Chunk.Substring(6).Trim;
                if JsonChunk = '[DONE]' then
                  Exit;

                try
                  var JsonObj := TJSONObject.ParseJSONValue(JsonChunk) as TJSONObject;
                  try
                    var Choices := JsonObj.GetValue<TJSONArray>('choices');
                    if (Choices <> nil) and (Choices.Count > 0) then
                    begin
                      var Delta := Choices.Items[0].GetValue<TJSONObject>('delta');
                      if (Delta <> nil) and Delta.TryGetValue<string>('content', Line) then
                      begin
                        DoToken(Line); // fire event with token text
                      end;
                    end;
                  finally
                    FreeAndNil(JsonObj);
                  end;
                except
                  // ignore parse errors
                end;
              end;
            end;
          end;
        until ReadCount = 0;

      finally
//        RespStream.Free;
      end;
    finally
//      ReqStream.Free;
    end;
  finally
    FreeAndNil(HttpClient);
  end;
end;

end.
