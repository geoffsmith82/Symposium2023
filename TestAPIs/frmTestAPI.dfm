object frmTestApiWindow: TfrmTestApiWindow
  Left = 0
  Top = 0
  Caption = 'frmTestApiWindow'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 75
    Width = 628
    Height = 367
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
    ExplicitWidth = 624
    ExplicitHeight = 366
  end
  object Button1: TButton
    Left = 0
    Top = 0
    Width = 628
    Height = 25
    Align = alTop
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
    ExplicitWidth = 624
  end
  object Button2: TButton
    Left = 0
    Top = 25
    Width = 628
    Height = 25
    Align = alTop
    Caption = 'Authenticate'
    TabOrder = 2
    OnClick = Button2Click
    ExplicitWidth = 624
  end
  object Button3: TButton
    Left = 0
    Top = 50
    Width = 628
    Height = 25
    Align = alTop
    Caption = 'Button3'
    TabOrder = 3
    OnClick = Button3Click
    ExplicitWidth = 624
  end
end
