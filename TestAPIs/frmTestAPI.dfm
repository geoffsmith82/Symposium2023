object frmTestApiWindow: TfrmTestApiWindow
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Caption = 'AI API Tester'
  ClientHeight = 990
  ClientWidth = 1843
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
    Top = 0
    Width = 1843
    Height = 990
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
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
      object miGoogleAuthenticate: TMenuItem
        Caption = 'Google Authenticate...'
        OnClick = miGoogleAuthenticateClick
      end
    end
  end
end
