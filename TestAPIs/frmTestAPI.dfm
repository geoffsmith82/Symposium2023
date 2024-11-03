object frmTestApiWindow: TfrmTestApiWindow
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Caption = 'frmTestApiWindow'
  ClientHeight = 1105
  ClientWidth = 1863
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
  object Memo1: TMemo
    Left = 0
    Top = 65
    Width = 1863
    Height = 1040
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button2: TButton
    Left = 0
    Top = 0
    Width = 1863
    Height = 65
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    Caption = 'Authenticate'
    TabOrder = 1
    OnClick = Button2Click
  end
  object MainMenu: TMainMenu
    Left = 456
    Top = 264
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object TestMenuItem: TMenuItem
      Caption = 'Tests'
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
