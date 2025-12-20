unit TestBase.LLM;

interface

uses
  DUnitX.TestFramework,
  System.Generics.Collections,
  System.SysUtils,
  ApiKeyStore,
  uLLM,
  TestBase.AI;

type
  TBaseLLMTests = class abstract(TBaseAITest)
  protected
    FKeys: TApiKeyStore;
    function CreateLLM: TBaseLLM; virtual; abstract;
    function DefaultModel: string; virtual; abstract;
    function SupportsSystemRole: Boolean; virtual;

  public
    [Test] procedure ListModels_And_Chat_User;
    [Test] procedure Chat_System_And_User;
  end;

implementation

function TBaseLLMTests.SupportsSystemRole: Boolean;
begin
  Result := True;
end;

procedure TBaseLLMTests.ListModels_And_Chat_User;
var
  LLM: TBaseLLM;
  Model: TBaseModelInfo;
  Settings: TChatSettings;
  Messages: TObjectList<TChatMessage>;
  Msg: TChatMessage;
  Resp: TChatResponse;
begin
  RequireLiveTests;

  LLM := CreateLLM;
  try
    // Model list
    Assert.IsTrue(LLM.ModelInfo.Count > 0, 'No models returned');
    for Model in LLM.ModelInfo do
    begin
      Assert.IsNotEmpty(Model.modelName);
      Break;
    end;

    // Chat
    Settings := Default(TChatSettings);
    Settings.model := DefaultModel;

    Messages := TObjectList<TChatMessage>.Create(True);
    try
      Msg := LLM.CreateChatMessage; // TChatMessage.Create doesn't exist
      Msg.Role := 'user';
      Msg.Content := 'How long is a piece of string';
      Messages.Add(Msg);

      Resp := LLM.ChatCompletion(Settings, Messages);
      Assert.IsNotEmpty(Resp.Content);
    finally
      Messages.Free;
    end;
  finally
    FreeAndNil(LLM);
  end;
end;

procedure TBaseLLMTests.Chat_System_And_User;
var
  LLM: TBaseLLM;
  Settings: TChatSettings;
  Messages: TObjectList<TChatMessage>;
  Msg: TChatMessage;
  Resp: TChatResponse;
begin
  RequireLiveTests;

//  if not SupportsSystemRole then
//    Skip('System role not supported by this provider');

  LLM := CreateLLM;
  try
    Settings := Default(TChatSettings);
    Settings.model := DefaultModel;

    Messages := TObjectList<TChatMessage>.Create(True);
    try
      Msg := LLM.CreateChatMessage;
      Msg.Role := 'system';
      Msg.Content := 'You are a helpful assistant';
      Messages.Add(Msg);

      Msg := LLM.CreateChatMessage;
      Msg.Role := 'user';
      Msg.Content := 'Call the time and weather functions';
      Messages.Add(Msg);

      Resp := LLM.ChatCompletion(Settings, Messages);
      Assert.IsNotEmpty(Resp.Content);
    finally
      Messages.Free;
    end;
  finally
    LLM.Free;
  end;
end;

end.

