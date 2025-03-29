unit MCP.Prompts;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections;

type
  /// <summary>
  /// Represents a variable in an MCP prompt template
  /// </summary>
  TMCPPromptVariable = class
  private
    FName: string;
    FDescription: string;
    FRequired: Boolean;
    FDefaultValue: string;
  public
    constructor Create(const AName, ADescription: string; ARequired: Boolean = True; 
      const ADefaultValue: string = '');
    function ToJSON: TJSONObject;
    
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Required: Boolean read FRequired write FRequired;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
  end;

  /// <summary>
  /// Represents a prompt template for MCP
  /// </summary>
  TMCPPrompt = class
  private
    FID: string;
    FName: string;
    FDescription: string;
    FPromptText: string;
    FVariables: TObjectList<TMCPPromptVariable>;
    FTags: TArray<string>;
    function GetVariable(Index: Integer): TMCPPromptVariable;
    function GetVariableCount: Integer;
  public
    constructor Create(const AID, AName, ADescription, APromptText: string);
    destructor Destroy; override;
    
    function ToJSON: TJSONObject;
    function AddVariable(const AName, ADescription: string; ARequired: Boolean = True; 
      const ADefaultValue: string = ''): TMCPPromptVariable;
    procedure AddTag(const ATag: string);
    function ValidatePromptText: Boolean;
    
    property ID: string read FID write FID;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property PromptText: string read FPromptText write FPromptText;
    property Variables[Index: Integer]: TMCPPromptVariable read GetVariable;
    property VariableCount: Integer read GetVariableCount;
    property Tags: TArray<string> read FTags write FTags;
  end;

  /// <summary>
  /// Manages a collection of MCP prompts
  /// </summary>
  TMCPPromptManager = class
  private
    FPrompts: TObjectList<TMCPPrompt>;
    function GetPrompt(Index: Integer): TMCPPrompt;
    function GetPromptCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    function AddPrompt(const AID, AName, ADescription, APromptText: string): TMCPPrompt;
    function FindPromptByID(const AID: string): TMCPPrompt;
    procedure DeletePrompt(const AID: string);
    function ToJSON: TJSONArray;
    
    property Prompts[Index: Integer]: TMCPPrompt read GetPrompt;
    property PromptCount: Integer read GetPromptCount;
  end;

implementation

{ TMCPPromptVariable }

constructor TMCPPromptVariable.Create(const AName, ADescription: string; ARequired: Boolean;
  const ADefaultValue: string);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
  FRequired := ARequired;
  FDefaultValue := ADefaultValue;
end;

function TMCPPromptVariable.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  
  // Add required properties
  if FName.Trim.IsEmpty then
    raise Exception.Create('Prompt variable name cannot be empty');
    
  Result.AddPair('name', FName);
  Result.AddPair('description', FDescription);
  
  // Add optional properties
  Result.AddPair('required', TJSONBool.Create(FRequired));
  
  if not FDefaultValue.IsEmpty then
    Result.AddPair('default_value', FDefaultValue);
end;

{ TMCPPrompt }

constructor TMCPPrompt.Create(const AID, AName, ADescription, APromptText: string);
begin
  inherited Create;
  
  if AID.Trim.IsEmpty then
    raise Exception.Create('Prompt ID cannot be empty');
    
  if AName.Trim.IsEmpty then
    raise Exception.Create('Prompt name cannot be empty');
    
  FID := AID;
  FName := AName;
  FDescription := ADescription;
  FPromptText := APromptText;
  FVariables := TObjectList<TMCPPromptVariable>.Create(True); // Owns objects
  SetLength(FTags, 0);
end;

destructor TMCPPrompt.Destroy;
begin
  FVariables.Free;
  inherited;
end;

function TMCPPrompt.GetVariable(Index: Integer): TMCPPromptVariable;
begin
  Result := FVariables[Index];
end;

function TMCPPrompt.GetVariableCount: Integer;
begin
  Result := FVariables.Count;
end;

function TMCPPrompt.AddVariable(const AName, ADescription: string; ARequired: Boolean;
  const ADefaultValue: string): TMCPPromptVariable;
begin
  // Check if variable with same name already exists
  for var Variable in FVariables do
  begin
    if SameText(Variable.Name, AName) then
      raise Exception.CreateFmt('Variable "%s" already exists in prompt', [AName]);
  end;
  
  Result := TMCPPromptVariable.Create(AName, ADescription, ARequired, ADefaultValue);
  FVariables.Add(Result);
end;

procedure TMCPPrompt.AddTag(const ATag: string);
begin
  if ATag.Trim.IsEmpty then
    Exit;
    
  // Check if tag already exists
  for var Tag in FTags do
  begin
    if SameText(Tag, ATag) then
      Exit;
  end;
  
  // Add the tag
  SetLength(FTags, Length(FTags) + 1);
  FTags[High(FTags)] := ATag;
end;

function TMCPPrompt.ValidatePromptText: Boolean;
var
  FoundVariables: TDictionary<string, Boolean>;
  VarName: string;
  i, StartPos, EndPos: Integer;
begin
  // Create a dictionary to track found variables
  FoundVariables := TDictionary<string, Boolean>.Create;
  try
    // Add all defined variables to the dictionary
    for var Variable in FVariables do
      FoundVariables.Add(Variable.Name, False);
    
    // Check prompt text for variable placeholders
    i := 1;
    while i <= Length(FPromptText) do
    begin
      if (i < Length(FPromptText)) and (FPromptText[i] = '{') then
      begin
        StartPos := i + 1;
        EndPos := StartPos;
        
        // Find the closing brace
        while (EndPos <= Length(FPromptText)) and (FPromptText[EndPos] <> '}') do
          Inc(EndPos);
          
        if (EndPos <= Length(FPromptText)) and (FPromptText[EndPos] = '}') then
        begin
          // Extract variable name
          VarName := Copy(FPromptText, StartPos, EndPos - StartPos);
          
          // Mark variable as found
          if FoundVariables.ContainsKey(VarName) then
            FoundVariables[VarName] := True;
            
          i := EndPos + 1;
          Continue;
        end;
      end;
      
      Inc(i);
    end;
    
    // Check if all required variables are used in the prompt
    for var Variable in FVariables do
    begin
      if Variable.Required and not FoundVariables[Variable.Name] then
        Exit(False);
    end;
    
    Result := True;
  finally
    FoundVariables.Free;
  end;
end;

function TMCPPrompt.ToJSON: TJSONObject;
var
  VariablesArray, TagsArray: TJSONArray;
begin
  Result := TJSONObject.Create;
  
  // Add required properties
  Result.AddPair('id', FID);
  Result.AddPair('name', FName);
  Result.AddPair('description', FDescription);
  Result.AddPair('prompt_text', FPromptText);
  
  // Add variables array
  VariablesArray := TJSONArray.Create;
  for var Variable in FVariables do
    VariablesArray.Add(Variable.ToJSON);
    
  Result.AddPair('variables', VariablesArray);
  
  // Add tags array if any
  if Length(FTags) > 0 then
  begin
    TagsArray := TJSONArray.Create;
    for var Tag in FTags do
      TagsArray.Add(Tag);
      
    Result.AddPair('tags', TagsArray);
  end;
end;

{ TMCPPromptManager }

constructor TMCPPromptManager.Create;
begin
  inherited;
  FPrompts := TObjectList<TMCPPrompt>.Create(True); // Owns objects
end;

destructor TMCPPromptManager.Destroy;
begin
  FPrompts.Free;
  inherited;
end;

function TMCPPromptManager.GetPrompt(Index: Integer): TMCPPrompt;
begin
  Result := FPrompts[Index];
end;

function TMCPPromptManager.GetPromptCount: Integer;
begin
  Result := FPrompts.Count;
end;

function TMCPPromptManager.AddPrompt(const AID, AName, ADescription, APromptText: string): TMCPPrompt;
begin
  // Check if prompt with same ID already exists
  for var Prompt in FPrompts do
  begin
    if SameText(Prompt.ID, AID) then
      raise Exception.CreateFmt('Prompt with ID "%s" already exists', [AID]);
  end;
  
  Result := TMCPPrompt.Create(AID, AName, ADescription, APromptText);
  FPrompts.Add(Result);
end;

function TMCPPromptManager.FindPromptByID(const AID: string): TMCPPrompt;
begin
  Result := nil;
  for var Prompt in FPrompts do
  begin
    if SameText(Prompt.ID, AID) then
    begin
      Result := Prompt;
      Break;
    end;
  end;
end;

procedure TMCPPromptManager.DeletePrompt(const AID: string);
var
  i: Integer;
begin
  for i := FPrompts.Count - 1 downto 0 do
  begin
    if SameText(FPrompts[i].ID, AID) then
    begin
      FPrompts.Delete(i);
      Break;
    end;
  end;
end;

function TMCPPromptManager.ToJSON: TJSONArray;
begin
  Result := TJSONArray.Create;
  
  // Add each prompt as a JSON object
  for var Prompt in FPrompts do
    Result.Add(Prompt.ToJSON);
end;

end.