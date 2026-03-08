unit Test.StructuredOutput;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.JSON,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  uLLM,
  uLLM.OpenAI,
  uLLM.Anthropic,
  uLLM.DeepSeek,
  uLLM.Groq,
  ApiKeyStore,
  TestBase.AI,
  uSchemaModels;

type
  [TestFixture]
  TSchemaGenerationTests = class
  public
    [Test] procedure Recipe_Schema_Has_Required_Fields;
    [Test] procedure MovieReview_Schema_Has_Required_Fields;
    [Test] procedure ContactInfo_Schema_Has_Optional_Fields;
    [Test] procedure Recipe_Schema_Has_Steps_Array;
  end;

  [TestFixture]
  TOpenAIStructuredOutputTests = class(TBaseAITest)
  public
    [Test] procedure Chat_StructuredOutput_Recipe;
    [Test] procedure Chat_StructuredOutput_ContactInfo;
    [Test] procedure Chat_StructuredOutput_MovieReview;
  end;

  [TestFixture]
  TAnthropicStructuredOutputTests = class(TBaseAITest)
  public
    [Test] procedure Chat_StructuredOutput_Fallback_Recipe;
  end;

  [TestFixture]
  TGroqStructuredOutputTests = class(TBaseAITest)
  public
    [Test] procedure Chat_StructuredOutput_Recipe;
  end;

  [TestFixture]
  TDeepSeekStructuredOutputTests = class(TBaseAITest)
  public
    [Test] procedure Chat_StructuredOutput_Recipe;
  end;

implementation

uses
  PasDantic.SchemaGenerator,
  PasDantic.Validator;

{ RTTI-based JSON to model population.
  Handles string, integer, float fields and TObjectList<T> arrays.
  Uses case-insensitive field name matching against JSON keys. }

function FindJSONPair(AObj: TJSONObject; const AFieldName: string): TJSONPair;
var
  I: Integer;
begin
  for I := 0 to AObj.Count - 1 do
    if SameText(AObj.Pairs[I].JsonString.Value, AFieldName) then
      Exit(AObj.Pairs[I]);
  Result := nil;
end;

procedure PopulateObjectFromJSON(AInstance: TObject; AJSON: TJSONObject); forward;

function CreateObjectFromJSON(AClass: TClass; AJSON: TJSONObject): TObject;
begin
  Result := AClass.Create;
  PopulateObjectFromJSON(Result, AJSON);
end;

procedure PopulateObjectFromJSON(AInstance: TObject; AJSON: TJSONObject);
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  Pair: TJSONPair;
  GenericName, InnerClassName: string;
  InnerRttiType: TRttiType;
  ArrValue: TJSONArray;
  ListObj: TObject;
  AddMethod: TRttiMethod;
  ItemObj: TObject;
  I: Integer;
begin
  if (AInstance = nil) or (AJSON = nil) then
    Exit;

  Ctx := TRttiContext.Create;
  try
    RttiType := Ctx.GetType(AInstance.ClassType);

    for Field in RttiType.GetFields do
    begin
      if not Field.IsReadable then
        Continue;

      Pair := FindJSONPair(AJSON, Field.Name);
      if Pair = nil then
        Continue;

      case Field.FieldType.TypeKind of
        tkString, tkUString:
          if not (Pair.JsonValue is TJSONNull) then
            Field.SetValue(AInstance, Pair.JsonValue.Value);

        tkInteger:
          if not (Pair.JsonValue is TJSONNull) then
            Field.SetValue(AInstance, TValue.From<Integer>(Pair.JsonValue.AsType<Integer>));

        tkInt64:
          if not (Pair.JsonValue is TJSONNull) then
            Field.SetValue(AInstance, TValue.From<Int64>(Pair.JsonValue.AsType<Int64>));

        tkFloat:
          if not (Pair.JsonValue is TJSONNull) then
            Field.SetValue(AInstance, TValue.From<Double>(Pair.JsonValue.AsType<Double>));

        tkClass:
          begin
            if Field.FieldType.Name.StartsWith('TObjectList<') then
            begin
              // Handle TObjectList<T> - create the list and populate items
              GenericName := Field.FieldType.ToString;
              InnerClassName := Copy(GenericName, Pos('<', GenericName) + 1,
                Length(GenericName) - Pos('<', GenericName) - 1);
              InnerRttiType := Ctx.FindType(InnerClassName);

              if Assigned(InnerRttiType) and (Pair.JsonValue is TJSONArray) then
              begin
                ArrValue := TJSONArray(Pair.JsonValue);
                ListObj := Field.GetValue(AInstance).AsObject;

                // If the list wasn't created yet, create it via RTTI constructor
                if ListObj = nil then
                begin
                  var ListRttiType := Ctx.GetType(Field.FieldType.AsInstance.MetaclassType);
                  var Ctor: TRttiMethod := nil;
                  // Find constructor with single Boolean param (AOwnsObjects)
                  for var M in ListRttiType.GetMethods('Create') do
                  begin
                    var Params := M.GetParameters;
                    if M.IsConstructor and (Length(Params) = 1) and
                       (Params[0].ParamType.TypeKind = tkEnumeration) then
                    begin
                      Ctor := M;
                      Break;
                    end;
                  end;
                  if Assigned(Ctor) then
                    ListObj := Ctor.Invoke(Field.FieldType.AsInstance.MetaclassType,
                      [TValue.From<Boolean>(True)]).AsObject
                  else
                    ListObj := Field.FieldType.AsInstance.MetaclassType.Create;
                  Field.SetValue(AInstance, ListObj);
                end;

                AddMethod := Ctx.GetType(ListObj.ClassType).GetMethod('Add');
                if Assigned(AddMethod) then
                begin
                  for I := 0 to ArrValue.Count - 1 do
                  begin
                    if ArrValue.Items[I] is TJSONObject then
                    begin
                      ItemObj := CreateObjectFromJSON(
                        InnerRttiType.AsInstance.MetaclassType,
                        TJSONObject(ArrValue.Items[I]));
                      AddMethod.Invoke(ListObj, [ItemObj]);
                    end;
                  end;
                end;
              end;
            end
            else if Pair.JsonValue is TJSONObject then
            begin
              // Nested object
              var NestedObj := Field.GetValue(AInstance).AsObject;
              if NestedObj = nil then
              begin
                NestedObj := Field.FieldType.AsInstance.MetaclassType.Create;
                Field.SetValue(AInstance, NestedObj);
              end;
              PopulateObjectFromJSON(NestedObj, TJSONObject(Pair.JsonValue));
            end;
          end;
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

{ Validate a deserialized model, handling TObjectList fields safely.
  ValidateModel recurses into all class fields via RTTI, but TObjectList<T>
  has internal TList fields that cause stack overflow. This procedure
  validates scalar fields on the top-level object and validates each
  TObjectList item individually. }
procedure ValidateDeserializedModel(AInstance: TObject; AJSON: TJSONObject);
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  ListObj: TObject;
  ValidationResult: TValidationResult;
  ErrorMsg: string;
  I: Integer;
  HasListFields: Boolean;
  ItemCount: TRttiProperty;
  CountVal: Integer;
  ItemsProp: TRttiIndexedProperty;
  ItemVal: TValue;
begin
  HasListFields := False;
  Ctx := TRttiContext.Create;
  try
    RttiType := Ctx.GetType(AInstance.ClassType);
    for Field in RttiType.GetFields do
    begin
      if Field.FieldType.Name.StartsWith('TObjectList<') then
      begin
        HasListFields := True;
        // Validate each item in the list individually
        ListObj := Field.GetValue(AInstance).AsObject;
        if Assigned(ListObj) then
        begin
          ItemCount := Ctx.GetType(ListObj.ClassType).GetProperty('Count');
          if Assigned(ItemCount) then
          begin
            CountVal := ItemCount.GetValue(ListObj).AsInteger;
            Assert.IsTrue(CountVal > 0, Field.Name + ' list should not be empty');
            // Validate first item as representative
            ItemsProp := Ctx.GetType(ListObj.ClassType).GetIndexedProperty('Items');
            if Assigned(ItemsProp) then
            begin
              for I := 0 to CountVal - 1 do
              begin
                ItemVal := ItemsProp.GetValue(ListObj, [I]);
                if (ItemVal.Kind = tkClass) and (ItemVal.AsObject <> nil) then
                begin
                  ValidationResult := ValidateModel(ItemVal.AsObject);
                  if not ValidationResult.IsValid then
                  begin
                    ErrorMsg := Field.Name + '[' + IntToStr(I) + '] validation failed:';
                    for var J := 0 to High(ValidationResult.Errors) do
                      ErrorMsg := ErrorMsg + sLineBreak + '  ' +
                        ValidationResult.Errors[J].PropertyPath + ': ' +
                        ValidationResult.Errors[J].Message;
                    Assert.Fail(ErrorMsg);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    Ctx.Free;
  end;

  if not HasListFields then
  begin
    // Safe to validate directly - no TObjectList fields
    ValidationResult := ValidateModel(AInstance);
    if not ValidationResult.IsValid then
    begin
      ErrorMsg := 'PasDantic validation failed with ' +
        IntToStr(ValidationResult.ErrorCount) + ' error(s):';
      for I := 0 to High(ValidationResult.Errors) do
        ErrorMsg := ErrorMsg + sLineBreak + '  ' +
          ValidationResult.Errors[I].PropertyPath + ': ' +
          ValidationResult.Errors[I].Message;
      Assert.Fail(ErrorMsg);
    end;
  end
  else
  begin
    // For objects with list fields, validate scalar fields via JSON structure
    // (the list items were already validated above)
    // Check that required scalar fields are present and non-empty in JSON
    Ctx := TRttiContext.Create;
    try
      RttiType := Ctx.GetType(AInstance.ClassType);
      for Field in RttiType.GetFields do
      begin
        if not Field.FieldType.Name.StartsWith('TObjectList<') then
        begin
          // Validate individual scalar fields
          var FieldVal := Field.GetValue(AInstance);
          if Field.FieldType.TypeKind in [tkString, tkUString] then
            Assert.IsNotEmpty(FieldVal.AsString, Field.Name + ' should not be empty');
        end;
      end;
    finally
      Ctx.Free;
    end;
  end;
end;

{ Extract JSON from response that may be wrapped in markdown code fences }
function ExtractJSON(const AContent: string): string;
var
  S: string;
  StartPos, EndPos: Integer;
begin
  S := Trim(AContent);
  // Strip ```json ... ``` or ``` ... ```
  if S.StartsWith('```') then
  begin
    StartPos := Pos(#10, S);
    if StartPos > 0 then
      S := Copy(S, StartPos + 1, Length(S));
    if S.EndsWith('```') then
      S := Copy(S, 1, Length(S) - 3);
    Result := Trim(S);
    Exit;
  end;
  // Try to find JSON object boundaries
  StartPos := Pos('{', S);
  EndPos := S.LastIndexOf('}');
  if (StartPos > 0) and (EndPos >= StartPos) then
    Result := Copy(S, StartPos, EndPos - StartPos + 2)
  else
    Result := S;
end;

{ Helper: call LLM with structured output, deserialize, validate with PasDantic }

procedure RunStructuredOutputTest(LLM: TBaseLLM; const AModel: string;
  ASchemaClass: TClass; const ASchemaName, APrompt, ASystemPrompt: string);
var
  Messages: TObjectList<TChatMessage>;
  Config: TChatSettings;
  Response: TChatResponse;
  Msg: TChatMessage;
  JSONValue: TJSONValue;
  JSONContent: string;
  JSONObj: TJSONObject;
  ModelInstance: TObject;
begin
  try
    Messages := TObjectList<TChatMessage>.Create(True);
    try
      // System message
      Msg := TChatMessage.Create;
      Msg.Role := 'system';
      if not LLM.SupportsStructuredOutput then
        Msg.Content := ASystemPrompt + sLineBreak + sLineBreak +
          TBaseLLM.SchemaFallbackPrompt(ASchemaClass)
      else
        Msg.Content := ASystemPrompt;
      Messages.Add(Msg);

      // User message
      Msg := TChatMessage.Create;
      Msg.Role := 'user';
      Msg.Content := APrompt;
      Messages.Add(Msg);

      // Configure
      Config := Default(TChatSettings);
      Config.model := AModel;
      Config.max_tokens := 2000;

      if LLM.SupportsStructuredOutput then
      begin
        Config.ResponseSchema := ASchemaClass;
        Config.ResponseSchemaName := ASchemaName;
      end;

      // Execute
      Response := LLM.ChatCompletion(Config, Messages);

      // 1. Response is not empty
      Assert.IsNotEmpty(Response.Content, 'Response content should not be empty');

      // 2. Response is valid JSON object (extract from markdown if needed)
      JSONContent := ExtractJSON(Response.Content);
      JSONValue := TJSONObject.ParseJSONValue(JSONContent);
      Assert.IsNotNull(JSONValue, 'Response should be valid JSON');
      try
        Assert.IsTrue(JSONValue is TJSONObject, 'Response should be a JSON object');
        JSONObj := TJSONObject(JSONValue);

        // 3. Deserialize into model instance via RTTI
        ModelInstance := CreateObjectFromJSON(ASchemaClass, JSONObj);
        try
          // 4. Validate with PasDantic ValidateModel
          //    ValidateModel recurses into all class-typed fields via RTTI.
          //    TObjectList<T> internal fields cause stack overflow, so for
          //    models with list fields, validate list items individually.
          ValidateDeserializedModel(ModelInstance, JSONObj);
        finally
          ModelInstance.Free;
        end;
      finally
        JSONValue.Free;
      end;

    finally
      Messages.Free;
    end;
  finally
    LLM.Free;
  end;
end;

const
  SYSTEM_PROMPT = 'You are a helpful assistant that provides structured data responses.';

{ TSchemaGenerationTests }

procedure TSchemaGenerationTests.Recipe_Schema_Has_Required_Fields;
var
  Schema: TJSONObject;
  Props: TJSONObject;
begin
  Schema := GenerateJSONSchema(TRecipe);
  try
    Assert.IsNotNull(Schema, 'Schema should not be nil');
    Assert.AreEqual('object', Schema.GetValue<string>('type'));

    Props := Schema.GetValue<TJSONObject>('properties');
    Assert.IsNotNull(Props, 'properties should exist');
    Assert.IsNotNull(Props.GetValue('Name'), 'Name property should exist');
    Assert.IsNotNull(Props.GetValue('Description'), 'Description property should exist');
    Assert.IsNotNull(Props.GetValue('Servings'), 'Servings property should exist');
    Assert.IsNotNull(Props.GetValue('PrepTimeMinutes'), 'PrepTimeMinutes property should exist');
    Assert.IsNotNull(Props.GetValue('CookTimeMinutes'), 'CookTimeMinutes property should exist');
    Assert.IsNotNull(Props.GetValue('Difficulty'), 'Difficulty property should exist');
    Assert.IsNotNull(Props.GetValue('Steps'), 'Steps property should exist');
  finally
    Schema.Free;
  end;
end;

procedure TSchemaGenerationTests.MovieReview_Schema_Has_Required_Fields;
var
  Schema: TJSONObject;
  Props: TJSONObject;
begin
  Schema := GenerateJSONSchema(TMovieReview);
  try
    Props := Schema.GetValue<TJSONObject>('properties');
    Assert.IsNotNull(Props.GetValue('Title'), 'Title should exist');
    Assert.IsNotNull(Props.GetValue('Year'), 'Year should exist');
    Assert.IsNotNull(Props.GetValue('Genre'), 'Genre should exist');
    Assert.IsNotNull(Props.GetValue('Rating'), 'Rating should exist');
    Assert.IsNotNull(Props.GetValue('Summary'), 'Summary should exist');
    Assert.IsNotNull(Props.GetValue('Sentiment'), 'Sentiment should exist');
  finally
    Schema.Free;
  end;
end;

procedure TSchemaGenerationTests.ContactInfo_Schema_Has_Optional_Fields;
var
  Schema: TJSONObject;
  Props: TJSONObject;
  RequiredArr: TJSONArray;
  HasFullName: Boolean;
  HasEmail: Boolean;
  I: Integer;
begin
  Schema := GenerateJSONSchema(TContactInfo);
  try
    Props := Schema.GetValue<TJSONObject>('properties');
    Assert.IsNotNull(Props.GetValue('FullName'), 'FullName should exist');
    Assert.IsNotNull(Props.GetValue('EmailAddress'), 'EmailAddress should exist');
    Assert.IsNotNull(Props.GetValue('PhoneNumber'), 'PhoneNumber should exist');

    RequiredArr := Schema.GetValue<TJSONArray>('required');
    Assert.IsNotNull(RequiredArr, 'required array should exist');

    HasFullName := False;
    HasEmail := False;
    for I := 0 to RequiredArr.Count - 1 do
    begin
      if RequiredArr.Items[I].Value = 'FullName' then
        HasFullName := True;
      if RequiredArr.Items[I].Value = 'EmailAddress' then
        HasEmail := True;
    end;

    Assert.IsTrue(HasFullName, 'FullName should be in required array');
    Assert.IsFalse(HasEmail, 'EmailAddress should NOT be in required array (it is Optional)');
  finally
    Schema.Free;
  end;
end;

procedure TSchemaGenerationTests.Recipe_Schema_Has_Steps_Array;
var
  Schema: TJSONObject;
  Props: TJSONObject;
  StepsProp: TJSONObject;
begin
  Schema := GenerateJSONSchema(TRecipe);
  try
    Props := Schema.GetValue<TJSONObject>('properties');
    StepsProp := Props.GetValue<TJSONObject>('Steps');
    Assert.IsNotNull(StepsProp, 'Steps property should exist');
    Assert.AreEqual('array', StepsProp.GetValue<string>('type'), 'Steps should be an array type');
    Assert.IsNotNull(StepsProp.GetValue('items'), 'Steps should have items definition');
  finally
    Schema.Free;
  end;
end;

{ TOpenAIStructuredOutputTests }

procedure TOpenAIStructuredOutputTests.Chat_StructuredOutput_Recipe;
var
  Keys: TApiKeyStore;
  LLM: TBaseLLM;
begin
  RequireLiveTests;
  Keys := TApiKeyStore.GetInstance;
  LLM := TOpenAI.Create(Keys.LoadApiKey('chatgpt_apikey'));
  Assert.IsTrue(LLM.SupportsStructuredOutput, 'OpenAI should support native structured output');
  RunStructuredOutputTest(LLM, 'gpt-4o-mini', TRecipe, 'recipe',
    'Give me a recipe for a classic Italian carbonara pasta.', SYSTEM_PROMPT);
end;

procedure TOpenAIStructuredOutputTests.Chat_StructuredOutput_ContactInfo;
var
  Keys: TApiKeyStore;
  LLM: TBaseLLM;
begin
  RequireLiveTests;
  Keys := TApiKeyStore.GetInstance;
  LLM := TOpenAI.Create(Keys.LoadApiKey('chatgpt_apikey'));
  RunStructuredOutputTest(LLM, 'gpt-4o-mini', TContactInfo, 'contact_info',
    'Extract the contact information from this text:' + sLineBreak + sLineBreak +
    'Hi, my name is Sarah Johnson. I work as a Senior Developer at TechCorp Industries. ' +
    'You can reach me at sarah.johnson@techcorp.com or call me at +61 412 345 678.',
    'You are a helpful assistant that extracts structured data from text.');
end;

procedure TOpenAIStructuredOutputTests.Chat_StructuredOutput_MovieReview;
var
  Keys: TApiKeyStore;
  LLM: TBaseLLM;
begin
  RequireLiveTests;
  Keys := TApiKeyStore.GetInstance;
  LLM := TOpenAI.Create(Keys.LoadApiKey('chatgpt_apikey'));
  RunStructuredOutputTest(LLM, 'gpt-4o-mini', TMovieReview, 'movie_review',
    'Write a review of the movie "The Shawshank Redemption" (1994).', SYSTEM_PROMPT);
end;

{ TAnthropicStructuredOutputTests }

procedure TAnthropicStructuredOutputTests.Chat_StructuredOutput_Fallback_Recipe;
var
  Keys: TApiKeyStore;
  LLM: TBaseLLM;
begin
  RequireLiveTests;
  Keys := TApiKeyStore.GetInstance;
  LLM := TAnthropic.Create(Keys.LoadApiKey('Claude_APIKey'));
  Assert.IsFalse(LLM.SupportsStructuredOutput,
    'Anthropic should use fallback (no native structured output)');
  RunStructuredOutputTest(LLM, 'claude-sonnet-4-20250514', TRecipe, 'recipe',
    'Give me a recipe for a classic Italian carbonara pasta.', SYSTEM_PROMPT);
end;

{ TGroqStructuredOutputTests }

procedure TGroqStructuredOutputTests.Chat_StructuredOutput_Recipe;
var
  Keys: TApiKeyStore;
  LLM: TBaseLLM;
begin
  RequireLiveTests;
  Keys := TApiKeyStore.GetInstance;
  LLM := TGroqLLM.Create(Keys.LoadApiKey('groq_apikey'));
  Assert.IsTrue(LLM.SupportsStructuredOutput, 'Groq should support native structured output');
  // Only openai/gpt-oss models support json_schema on Groq
  RunStructuredOutputTest(LLM, 'openai/gpt-oss-120b', TRecipe, 'recipe',
    'Give me a recipe for a classic Italian carbonara pasta.', SYSTEM_PROMPT);
end;

{ TDeepSeekStructuredOutputTests }

procedure TDeepSeekStructuredOutputTests.Chat_StructuredOutput_Recipe;
var
  Keys: TApiKeyStore;
  LLM: TBaseLLM;
begin
  RequireLiveTests;
  Keys := TApiKeyStore.GetInstance;
  LLM := TDeepSeek.Create(Keys.LoadApiKey('Deepseek_Key'));
  // DeepSeek json_schema format currently unavailable; test via fallback
  RunStructuredOutputTest(LLM, 'deepseek-chat', TRecipe, 'recipe',
    'Give me a recipe for a classic Italian carbonara pasta.', SYSTEM_PROMPT);
end;

initialization
  TDUnitX.RegisterTestFixture(TSchemaGenerationTests);
  TDUnitX.RegisterTestFixture(TOpenAIStructuredOutputTests);
  TDUnitX.RegisterTestFixture(TAnthropicStructuredOutputTests);
  TDUnitX.RegisterTestFixture(TGroqStructuredOutputTests);
  TDUnitX.RegisterTestFixture(TDeepSeekStructuredOutputTests);

end.
