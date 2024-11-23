unit frmApiKeyStore;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  ApiKeyStore
  ;

type
  TfrmApiKeyStores = class(TForm)
    btnClose: TButton;
    btnCancel: TButton;
    PageControl1: TPageControl;
    tsAPIKeys: TTabSheet;
    tsSettings: TTabSheet;
    StringGrid: TStringGrid;
    SettingsStringGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure StringGridDrawCell(Sender: TObject; ACol, ARow: LongInt;
      Rect: TRect; State: TGridDrawState);
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: LongInt;
      var CanSelect: Boolean);
    procedure StringGridSetEditText(Sender: TObject; ACol, ARow: LongInt;
      const Value: string);
    procedure btnCancelClick(Sender: TObject);
    procedure SettingsStringGridSetEditText(Sender: TObject; ACol,
      ARow: LongInt; const Value: string);
  private
    { Private declarations }
    FApiKeyStore : TApiKeyStore;
    FModifiedRows: array of Boolean;
    FEditCell: TPoint;
    FSettingsModifiedRows: array of Boolean;
    function MaskAPIKey(const APIKey: string): string;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}



procedure TfrmApiKeyStores.btnCloseClick(Sender: TObject);
var
  i: Integer;
  ApiKeyName, ApiKeyValue: string;
begin
  for i := 1 to StringGrid.RowCount - 1 do
  begin
    if FModifiedRows[i] then
    begin
      ApiKeyName := StringGrid.Cells[0, i];
      ApiKeyValue := StringGrid.Cells[1, i];
      FApiKeyStore.SaveApiKey(ApiKeyName, ApiKeyValue);
    end;
  end;

  for i := 1 to SettingsStringGrid.RowCount - 1 do
  begin
    if FSettingsModifiedRows[i] then
    begin
      ApiKeyName := SettingsStringGrid.Cells[0, i];
      ApiKeyValue := SettingsStringGrid.Cells[1, i];
      FApiKeyStore.SaveSetting(ApiKeyName, ApiKeyValue);
    end;
  end;

  Close;
end;

procedure TfrmApiKeyStores.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmApiKeyStores.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  StringGrid.Cells[0, 0] := 'Name';
  StringGrid.Cells[1, 0] := 'Key';

  SettingsStringGrid.Cells[0, 0] := 'Name';
  SettingsStringGrid.Cells[1, 0] := 'Value';

  StringGrid.ColWidths[0] := 400;
  StringGrid.ColWidths[1] := StringGrid.Width - StringGrid.ColWidths[0];

  SettingsStringGrid.ColWidths[0] := 300;
  SettingsStringGrid.ColWidths[1] := SettingsStringGrid.Width - SettingsStringGrid.ColWidths[0];

  StringGrid.RowCount := 18;
  SettingsStringGrid.RowCount := 3;

  StringGrid.Cells[0, 1] := 'chatgpt_apikey';
  StringGrid.Cells[0, 2] := 'X_AI';
  StringGrid.Cells[0, 3] := 'groq_apikey';
  StringGrid.Cells[0, 4] := 'ElevenLabsAPIKey';
  StringGrid.Cells[0, 5] := 'revai_key';
  StringGrid.Cells[0, 6] := 'assemblyai_key';
  StringGrid.Cells[0, 7] := 'deepgram_key';
  StringGrid.Cells[0, 8] := 'HuggingFace_APIKey';
  StringGrid.Cells[0, 9] := 'ms_cognative_service_resource_key';
  StringGrid.Cells[0,10] := 'AWSAccessKey';
  StringGrid.Cells[0,11] := 'AWSSecretKey';
  StringGrid.Cells[0,12] := 'google_clientid';
  StringGrid.Cells[0,13] := 'google_clientsecret';
  StringGrid.Cells[0,14] := 'Replicate_APIKey';
  StringGrid.Cells[0,15] := 'AzureAPIKey';
  StringGrid.Cells[0,16] := 'Claude_APIKey';
  StringGrid.Cells[0,17] := 'picovoice';



  SettingsStringGrid.Cells[0, 1] := 'AWSRegion';
  SettingsStringGrid.Cells[0, 2] := 'AzureOpenAIEndpoint';




  FApiKeyStore := TApiKeyStore.GetInstance;

  for i := 1 to StringGrid.RowCount - 1 do
  begin
    StringGrid.Cells[1, i] :=  FApiKeyStore.LoadApiKey(StringGrid.Cells[0, i]);
  end;

  for i := 1 to SettingsStringGrid.RowCount - 1 do
  begin
    SettingsStringGrid.Cells[1, i] :=  FApiKeyStore.LoadSetting(SettingsStringGrid.Cells[0, i]);
  end;

  // Initialize modified tracking array
  SetLength(FModifiedRows, StringGrid.RowCount);
  for i := 0 to High(FModifiedRows) do
    FModifiedRows[i] := False;

    StringGrid.EditorMode := False;

  // Initialize modified tracking array
  SetLength(FSettingsModifiedRows, SettingsStringGrid.RowCount);
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



procedure TfrmApiKeyStores.SettingsStringGridSetEditText(Sender: TObject; ACol,
  ARow: LongInt; const Value: string);
begin
  if ACol = 1 then
  begin
    // Allow direct editing of the APIKey
    SettingsStringGrid.Cells[ACol, ARow] := Value;

    // Mark row as modified
    FSettingsModifiedRows[ARow] := True;
  end;
end;

procedure TfrmApiKeyStores.StringGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  CellText, DisplayText: string;
begin
  CellText := StringGrid.Cells[ACol, ARow];

  if (ACol = 0) or (ARow = 0) then
  begin
    StringGrid.Canvas.FillRect(Rect);
    StringGrid.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, CellText);
    Exit;
  end;

  // Check if we are in the APIKey column and not editing
  if not ((FEditCell.X = ACol) and (FEditCell.Y = ARow))  then
  begin
    // Use MaskAPIKey function to get the masked version of the API key
    DisplayText := MaskAPIKey(CellText);

    // Draw the masked text
    StringGrid.Canvas.FillRect(Rect);
    StringGrid.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, DisplayText);
  end
  else //if not StringGrid.EditorMode or ((ACol = 0) or (ARow = 0)) then
  begin
    StringGrid.Canvas.FillRect(Rect);
    StringGrid.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, CellText);
  end;
end;


procedure TfrmApiKeyStores.StringGridSelectCell(Sender: TObject; ACol,
  ARow: LongInt; var CanSelect: Boolean);
begin
  // Enable editing mode for the APIKey column when selected
  if (ACol = 1) and (ARow <> 0) then
  begin
    StringGrid.EditorMode := True;
    FEditCell.X := ACol;
    FEditCell.Y := ARow;
  end
  else
    CanSelect := False;
end;

procedure TfrmApiKeyStores.StringGridSetEditText(Sender: TObject; ACol,
  ARow: LongInt; const Value: string);
begin
  if (ACol = 1) and (ARow <> 0) then
  begin
    // Allow direct editing of the APIKey
    StringGrid.Cells[ACol, ARow] := Value;

    // Mark row as modified
    FModifiedRows[ARow] := True;
  end;
end;

end.
