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
    Top = 125
    Width = 628
    Height = 317
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
    ExplicitTop = 100
    ExplicitHeight = 342
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
  end
  object Button4: TButton
    Left = 0
    Top = 75
    Width = 628
    Height = 25
    Align = alTop
    Caption = 'Button4'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 0
    Top = 100
    Width = 628
    Height = 25
    Align = alTop
    Caption = 'Test GPT Vision'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 0
    Top = 125
    Width = 628
    Height = 25
    Align = alTop
    Caption = 'Button6'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 0
    Top = 150
    Width = 628
    Height = 25
    Align = alTop
    Caption = 'Button7'
    TabOrder = 7
    OnClick = Button7Click
    ExplicitLeft = 240
    ExplicitTop = 160
    ExplicitWidth = 75
  end
end
