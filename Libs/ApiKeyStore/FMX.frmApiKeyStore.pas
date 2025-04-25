unit fmx.frmApiKeyStore;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.TabControl,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  System.Rtti,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Grid,
  uAPIKeyNameList,
  ApiKeyStore;

type
  TfrmApiKeyStores = class(TForm)
    TabControl1: TTabControl;
    tiAPIKeys: TTabItem;
    tiSettings: TTabItem;
    btnCancel: TButton;
    btnClose: TButton;
    sgAPIKeys: TStringGrid;
    sgSettings: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    procedure btnCancelClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sgSettingsEditingDone(Sender: TObject; const ACol, ARow: Integer);
    procedure sgAPIKeysDrawColumnCell(Sender: TObject; const Canvas: TCanvas;
        const Column: TColumn; const Bounds: TRectF; const Row: Integer; const
        Value: TValue; const State: TGridDrawStates);
    procedure sgAPIKeysEditingDone(Sender: TObject; const ACol, ARow: Integer);
  private
    { Private declarations }
    FApiKeyStore : TApiKeyStore;
    FModifiedRows: array of Boolean;
    FSettingsModifiedRows: array of Boolean;
    function MaskAPIKey(const APIKey: string): string;
  public
    { Public declarations }
  end;


implementation

{$R *.fmx}

procedure TfrmApiKeyStores.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmApiKeyStores.btnCloseClick(Sender: TObject);
var
  i: Integer;
  ApiKeyName, ApiKeyValue: string;
begin
  for i := 0 to sgAPIKeys.RowCount - 1 do
  begin
    if FModifiedRows[i] then
    begin
      ApiKeyName := sgAPIKeys.Cells[0, i];
      ApiKeyValue := sgAPIKeys.Cells[1, i];
      FApiKeyStore.SaveApiKey(ApiKeyName, ApiKeyValue);
    end;
  end;

  for i := 0 to sgSettings.RowCount - 1 do
  begin
    if FSettingsModifiedRows[i] then
    begin
      ApiKeyName := sgSettings.Cells[0, i];
      ApiKeyValue := sgSettings.Cells[1, i];
      FApiKeyStore.SaveSetting(ApiKeyName, ApiKeyValue);
    end;
  end;

  Close;
end;

procedure TfrmApiKeyStores.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  StringColumn1.Width := 300;
  StringColumn2.Width := sgAPIKeys.Width - StringColumn1.Width - 20;

  StringColumn3.Width := 300;
  StringColumn4.Width := sgSettings.Width - StringColumn3.Width - 20;

  sgAPIKeys.RowCount := High(ApiKeyNames);
  sgSettings.RowCount := 2;

  for i := 1 to High(ApiKeyNames) do
  begin
    sgAPIKeys.Cells[0, i - 1] := ApiKeyNames[i];
  end;

  sgSettings.Cells[0, 0] := 'AWSRegion';
  sgSettings.Cells[0, 1] := 'AzureOpenAIEndpoint';

  FApiKeyStore := TApiKeyStore.GetInstance;

  for i := 0 to sgAPIKeys.RowCount - 1 do
  begin
    sgAPIKeys.Cells[1, i] :=  FApiKeyStore.LoadApiKey(sgAPIKeys.Cells[0, i]);
  end;

  for i := 0 to sgSettings.RowCount - 1 do
  begin
    sgSettings.Cells[1, i] :=  FApiKeyStore.LoadSetting(sgSettings.Cells[0, i]);
  end;

  // Initialize modified tracking array
  SetLength(FModifiedRows, sgAPIKeys.RowCount);
  for i := 0 to High(FModifiedRows) do
    FModifiedRows[i] := False;

    sgAPIKeys.EditorMode := False;

  // Initialize modified tracking array
  SetLength(FSettingsModifiedRows, sgSettings.RowCount);
  for i := 0 to High(FSettingsModifiedRows) do
    FSettingsModifiedRows[i] := False;

end;

function TfrmApiKeyStores.MaskAPIKey(const APIKey: string): string;
var
  MaskedText: string;
  SpecialCharPos, VisibleEndChars: Integer;
begin
  MaskedText := '';
  VisibleEndChars := 2; // Number of characters to show at the end

  // Check if the length of the API key is more than 15 characters
  if Length(APIKey) > 15 then
  begin
    // Look for '-' or '_' in the first 6 characters
    SpecialCharPos := Pos('-', Copy(APIKey, 1, 6));
    if SpecialCharPos = 0 then
      SpecialCharPos := Pos('_', Copy(APIKey, 1, 6));

    // If a special character is found within the first 6 characters
    if SpecialCharPos > 0 then
    begin
      // Make sure 15 characters remain masked in the middle
      if (Length(APIKey) - SpecialCharPos - VisibleEndChars) >= 15 then
        MaskedText := Copy(APIKey, 1, SpecialCharPos) +
                      StringOfChar('*', Length(APIKey) - SpecialCharPos - VisibleEndChars) +
                      Copy(APIKey, Length(APIKey) - VisibleEndChars + 1, VisibleEndChars)
      else
        // If not enough characters to mask, fall back to masking everything but start and end
        MaskedText := APIKey[1] + StringOfChar('*', Length(APIKey) - 2) + APIKey[Length(APIKey)];
    end
    else
    begin
      // Default masking without special character: show first and last characters only
      MaskedText := APIKey[1] +
                    StringOfChar('*', Length(APIKey) - 2 - VisibleEndChars) +
                    Copy(APIKey, Length(APIKey) - VisibleEndChars + 1, VisibleEndChars);
    end;
  end
  else
    // For shorter API keys, mask the entire key
    MaskedText := StringOfChar('*', Length(APIKey));

  Result := MaskedText;
end;

procedure TfrmApiKeyStores.sgSettingsEditingDone(Sender: TObject; const
    ACol, ARow: Integer);
begin
  if ACol = 1 then
  begin
    // Mark row as modified
    FSettingsModifiedRows[ARow] := True;
  end;
end;



procedure TfrmApiKeyStores.sgAPIKeysDrawColumnCell(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  DisplayText: string;
begin
  if (Column.Header = 'Name') then
    DisplayText := Value.ToString  // Show full API key when selected or row is selected
  else if (TGridDrawState.Selected in State) or (TGridDrawState.RowSelected in State) then
    DisplayText := Value.ToString  // Show full API key when selected or row is selected
  else
    DisplayText := MaskAPIKey(Value.ToString); // Show masked key otherwise

  // Set text alignment and draw the text within the cell bounds
  Canvas.Fill.Color := TAlphaColorRec.Black;
  Canvas.Font.Size := 12;
  Canvas.FillText(Bounds, DisplayText, False, 1, [], TTextAlign.Leading, TTextAlign.Center);
end;

procedure TfrmApiKeyStores.sgAPIKeysEditingDone(Sender: TObject; const ACol,
    ARow: Integer);
begin
  if ACol = 1 then
  begin
    // Mark row as modified
    FModifiedRows[ARow] := True;
  end;
end;

end.

