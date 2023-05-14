object frmEmbeddings: TfrmEmbeddings
  Left = 0
  Top = 0
  Caption = 'Embedding Demo'
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
  object btnGoogleSearch: TButton
    Left = 503
    Top = 384
    Width = 117
    Height = 25
    Caption = 'Google Search'
    TabOrder = 0
    OnClick = btnGoogleSearchClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 88
    Width = 489
    Height = 346
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object btnEmbeddings: TButton
    Left = 503
    Top = 344
    Width = 117
    Height = 25
    Caption = 'Embeddings Demo'
    TabOrder = 2
    OnClick = btnEmbeddingsClick
  end
  object Button1: TButton
    Left = 528
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
  end
  object Button2: TButton
    Left = 528
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Import File'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Memo2: TMemo
    Left = 8
    Top = 8
    Width = 489
    Height = 57
    Lines.Strings = (
      'Memo2')
    TabOrder = 5
  end
  object btnQuery: TButton
    Left = 503
    Top = 8
    Width = 100
    Height = 25
    Caption = 'Ask Question'
    TabOrder = 6
    OnClick = btnQueryClick
  end
end
