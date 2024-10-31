object Form2: TForm2
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Caption = 'OpenAI Assistant API Invoice decoder Demo'
  ClientHeight = 1105
  ClientWidth = 2072
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -30
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 240
  TextHeight = 41
  object Memo1: TMemo
    Left = 360
    Top = 0
    Width = 1712
    Height = 1105
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button7: TButton
    Left = 13
    Top = 63
    Width = 308
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Decode PDF Invoice'
    TabOrder = 1
    OnClick = Button7Click
  end
end
