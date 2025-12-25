object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Caption = 'Streaming Prototype'
  ClientHeight = 1113
  ClientWidth = 1608
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -30
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  OnCreate = FormCreate
  PixelsPerInch = 240
  TextHeight = 41
  object mmoQuestion: TMemo
    Left = 0
    Top = 63
    Width = 1608
    Height = 415
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    DoubleBuffered = False
    Lines.Strings = (
      'Write a detailed blog post about Delphi openai streaming')
    ParentDoubleBuffered = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 0
    Top = 0
    Width = 1608
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    Caption = 'Send Request'
    TabOrder = 1
    OnClick = Button1Click
  end
  object mmoResponse: TMemo
    Left = 0
    Top = 495
    Width = 1608
    Height = 618
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alBottom
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object MainMenu: TMainMenu
    Left = 620
    Top = 580
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Settings1: TMenuItem
      Caption = 'Settings'
      object APIKeys1: TMenuItem
        Caption = 'API Keys...'
        OnClick = APIKeys1Click
      end
    end
  end
end
