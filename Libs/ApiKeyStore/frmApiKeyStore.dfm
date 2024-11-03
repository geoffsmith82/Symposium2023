object frmApiKeyStores: TfrmApiKeyStores
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  BorderStyle = bsDialog
  Caption = 'API Keys and Settings'
  ClientHeight = 1112
  ClientWidth = 1683
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -30
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 240
  TextHeight = 41
  object btnClose: TButton
    Left = 1440
    Top = 980
    Width = 188
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Close'
    TabOrder = 0
    OnClick = btnCloseClick
  end
  object btnCancel: TButton
    Left = 1200
    Top = 980
    Width = 188
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 13
    Width = 1670
    Height = 928
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    ActivePage = tsAPIKeys
    TabOrder = 2
    object tsAPIKeys: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'API Keys'
      object StringGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 1650
        Height = 854
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        ColCount = 2
        DefaultColWidth = 160
        DefaultRowHeight = 60
        DefaultDrawing = False
        DoubleBuffered = True
        RowCount = 9
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goFixedRowDefAlign]
        ParentDoubleBuffered = False
        TabOrder = 0
        OnDrawCell = StringGridDrawCell
        OnSelectCell = StringGridSelectCell
        OnSetEditText = StringGridSetEditText
      end
    end
    object tsSettings: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Settings'
      ImageIndex = 1
      object SettingsStringGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 1650
        Height = 854
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        ColCount = 2
        DefaultColWidth = 160
        DefaultRowHeight = 60
        RowCount = 9
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goFixedRowDefAlign]
        TabOrder = 0
        OnDrawCell = StringGridDrawCell
        OnSetEditText = SettingsStringGridSetEditText
      end
    end
  end
end
