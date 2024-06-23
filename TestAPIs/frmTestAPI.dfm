object frmTestApiWindow: TfrmTestApiWindow
  Left = 0
  Top = 0
  Caption = 'frmTestApiWindow'
  ClientHeight = 442
  ClientWidth = 745
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 745
    Height = 442
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
    ExplicitTop = 150
    ExplicitWidth = 628
    ExplicitHeight = 292
  end
  object Button2: TButton
    Left = 135
    Top = 0
    Width = 114
    Height = 25
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
  end
end
