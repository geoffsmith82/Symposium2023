object frmEmbeddings: TfrmEmbeddings
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Caption = 'Embedding Demo'
  ClientHeight = 1105
  ClientWidth = 1570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -30
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 240
  TextHeight = 41
  object btnGoogleSearch: TButton
    Left = 1258
    Top = 960
    Width = 292
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Google Search'
    TabOrder = 0
    OnClick = btnGoogleSearchClick
  end
  object Memo1: TMemo
    Left = 20
    Top = 220
    Width = 1223
    Height = 865
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object btnEmbeddings: TButton
    Left = 1258
    Top = 860
    Width = 292
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Embeddings Demo'
    TabOrder = 2
    OnClick = btnEmbeddingsClick
  end
  object Button1: TButton
    Left = 1320
    Top = 300
    Width = 188
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Button1'
    TabOrder = 3
  end
  object Button2: TButton
    Left = 1320
    Top = 420
    Width = 188
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Import File'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Memo2: TMemo
    Left = 20
    Top = 20
    Width = 1223
    Height = 143
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Lines.Strings = (
      'Memo2')
    TabOrder = 5
  end
  object btnQuery: TButton
    Left = 1258
    Top = 20
    Width = 250
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Ask Question'
    TabOrder = 6
    OnClick = btnQueryClick
  end
  object MainMenu: TMainMenu
    Left = 1340
    Top = 120
    object miFile: TMenuItem
      Caption = '&File'
      object miExit: TMenuItem
        Caption = 'E&xit'
        OnClick = miExitClick
      end
    end
    object miSettings: TMenuItem
      Caption = 'Settings'
      object miAPIKeys: TMenuItem
        Caption = 'API Keys...'
        OnClick = miAPIKeysClick
      end
    end
  end
end
