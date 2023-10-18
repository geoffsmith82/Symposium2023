unit BubbleText;

interface

uses
  System.Classes,
  Vcl.ComCtrls,
  Vcl.Forms,
  System.SysUtils,
  Dialogs,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Graphics,
  System.Types,
  System.UITypes
  ;

type
  TBubbleType = (btUser, btOther, btSystem);

  TBubbleText = class;

  TNotifyBubbleEvent = procedure(Sender: TObject; Bubble: TBubbleText) of object;

  TBubbleText = class(TCustomControl)
  private
    FSelected: Boolean;
    FPrimaryKey: Int64;
    FEdit: TRichEdit;  // Using TRichEdit instead of TLabel
    FOnSelected: TNotifyBubbleEvent;
    FBubbleType: TBubbleType;
    FBackgroundColor: TColor;
    FPadding: TPadding;
    FRectForBubble: TRect;
    procedure SetText(const Value: string);
    function GetText: string;
    procedure SetBubbleType(const Value: TBubbleType);
    procedure ResizeBubble;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetPadding(const Value: TPadding);
    procedure AlignBubbleBasedOnUserType;
    procedure SetSelected(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoSelected; virtual;
  published
    property Selected: Boolean read FSelected write SetSelected;
    property OnSelected: TNotifyBubbleEvent read FOnSelected write FOnSelected;
    property PrimaryKey: Int64 read FPrimaryKey write FPrimaryKey;
    property Text: string read GetText write SetText;
    property BubbleType: TBubbleType read FBubbleType write SetBubbleType default btUser;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property Padding: TPadding read FPadding write SetPadding;
    property Color;
    property Font;
    property Align;
    property Anchors;
    property Constraints;
    property Visible;
  end;

procedure Register;

implementation

uses
  Winapi.Windows;

procedure Register;
begin
  try
    RegisterComponents('Samples', [TBubbleText]);
  except
    on E: Exception do
      ShowMessage('Error registering TBubbleText: ' + E.Message);
  end;
end;

{ TBubbleText }

constructor TBubbleText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 150;
  Height := 50;

  FPadding := TPadding.Create(Self);
  FEdit := TRichEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.ReadOnly := True; // Ensure text is read-only
  FEdit.ScrollBars := ssNone; // Hide scrollbars
  FEdit.BorderStyle := TBorderStyle(0); // Remove the border to make it look like a TLabel
  FEdit.WordWrap := True;
  FEdit.Transparent := True;

  FBubbleType := btUser;  // Default type
  FPrimaryKey := -1;

  // Default background color based on BubbleType
  if FBubbleType = btUser then
    FBackgroundColor := clSkyBlue
  else if FBubbleType = btSystem then
    FBackgroundColor := clRed
  else
    FBackgroundColor := clSilver;

  FPadding := TPadding.Create(Self);
  FPadding.Top := 10;
  FPadding.Left := 10;
  FPadding.Right := 10;
  FPadding.Bottom := 10;
end;

destructor TBubbleText.Destroy;
begin
  FreeAndNil(FEdit);
  FreeAndNil(FPadding);
  inherited;
end;

procedure TBubbleText.AlignBubbleBasedOnUserType;
begin
  case FBubbleType of
    btUser:
      begin
        FRectForBubble := Rect(FPadding.Left, FPadding.Top, Trunc(Width * 0.75) - FPadding.Right, Height - FPadding.Bottom);
        FEdit.Left := FPadding.Left + 15;
        FEdit.Top := FPadding.Top;
        FEdit.Width := Trunc(Width * 0.75) - FPadding.Right - 30;
        FEdit.Height := Height - FPadding.Bottom;
      end;
    btOther:
      begin
        FRectForBubble := Rect(Trunc(Width * 0.25) + FPadding.Left, FPadding.Top, Width - FPadding.Right, Height - FPadding.Bottom);
        FEdit.Left := Trunc(Width * 0.25) + FPadding.Left + 15;
        FEdit.Top := FPadding.Top;
        FEdit.Width := Trunc(Width * 0.75) - FPadding.Right - 30;
        FEdit.Height := Height - FPadding.Bottom;
      end;
    btSystem:
      begin
        FRectForBubble := Rect(Trunc(Width * 0.25) + FPadding.Left, FPadding.Top, Width - FPadding.Right, Height - FPadding.Bottom);
        FEdit.Left := Trunc(Width * 0.25) + FPadding.Left + 15;
        FEdit.Top := FPadding.Top;
        FEdit.Width := Trunc(Width * 0.75) - FPadding.Right - 30;
        FEdit.Height := Height - FPadding.Bottom;
      end;
  end;
end;

function TBubbleText.GetText: string;
begin
  Result := FEdit.Text;
end;

procedure TBubbleText.Paint;
const
  SelectionBorder = 2;  // Adjust the thickness as needed
begin
  inherited;

  if FSelected then
  begin
    // Highlight the control with a rectangle
    Canvas.Pen.Color := clHighlight;  // Change this to your desired highlight color
    Canvas.Pen.Width := SelectionBorder;
    Canvas.Brush.Style := bsBDiagonal;  // Ensure only the border is drawn
    Canvas.Rectangle(0, 0, Width, Height);
  end;

  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := FBackgroundColor;

  // Drawing the rounded rectangle as the bubble shape
  Canvas.RoundRect(FRectForBubble, 15, 15);
end;

procedure TBubbleText.SetPadding(const Value: TPadding);
begin
  FPadding.Assign(Value);
  ResizeBubble;
  Invalidate;
end;

procedure TBubbleText.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then
    Selected := not Selected;
end;

procedure TBubbleText.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    Invalidate;  // Trigger a repaint to reflect selection change

    if FSelected then
      DoSelected;  // Call the event when selected
  end;
end;

procedure TBubbleText.DoSelected;
begin
  if Assigned(FOnSelected) then
    FOnSelected(Self, Self);
end;

procedure TBubbleText.Resize;
begin
  inherited;
  AlignBubbleBasedOnUserType;
  Invalidate;  // Redraw on resize
end;

procedure TBubbleText.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    Invalidate;  // Redraw when the color changes
  end;
end;

procedure TBubbleText.SetBubbleType(const Value: TBubbleType);
begin
  if FBubbleType <> Value then
  begin
    FBubbleType := Value;
    AlignBubbleBasedOnUserType;

    // Set the default colors based on the BubbleType
    if FBubbleType = btUser then
      BackgroundColor := clSkyBlue
    else if FBubbleType = btSystem then
      BackgroundColor := clRed
    else
      BackgroundColor := clSilver;

    Invalidate;  // Redraw when BubbleType changes
  end;
end;

function CalculateTextHeight(const ACanvas: TCanvas; const Text: string; MaxWidth: Integer): Integer;
var
  Rect2: TRect;
begin
  Rect2 := Rect(0, 0, MaxWidth, 0);
  DrawText(ACanvas.Handle, PChar(Text), -1, Rect2, DT_WORDBREAK or DT_CALCRECT or DT_LEFT);
  Result := Rect2.Bottom;
end;


const
  MIN_BUBBLE_WIDTH = 1000;
  MAX_BUBBLE_WIDTH = 1000;
  PADDING2 = 10;

procedure TBubbleText.ResizeBubble;
var
  TextWidth, TextHeight: Integer;
  NewWidth, NewHeight: Integer;
begin
  Canvas.Font.Assign(Font);
  TextWidth := Canvas.TextWidth(FEdit.Text);
  TextHeight := CalculateTextHeight(Canvas, FEdit.Text, MAX_BUBBLE_WIDTH - 2 * PADDING2);

  NewWidth := TextWidth + 2 * PADDING2;
  NewHeight := TextHeight + 2 * PADDING2;

  if NewWidth < MIN_BUBBLE_WIDTH then
    NewWidth := MIN_BUBBLE_WIDTH
  else if NewWidth > MAX_BUBBLE_WIDTH then
    NewWidth := MAX_BUBBLE_WIDTH;

  Width := NewWidth;
  Height := NewHeight;
  OutputDebugString(PChar('H' + NewHeight.ToString + ' ' + 'W ' + NewWidth.ToString));
end;


procedure TBubbleText.SetText(const Value: string);
begin
  FEdit.Text := Value;
  ResizeBubble;
  AlignBubbleBasedOnUserType;
  Invalidate;
end;

end.
