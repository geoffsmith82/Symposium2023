object frmEmbeddings: TfrmEmbeddings
  Left = 0
  Top = 0
  Caption = 'frmEmbeddings'
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
    Top = 16
    Width = 117
    Height = 25
    Caption = 'Google Search'
    TabOrder = 0
    OnClick = btnGoogleSearchClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 24
    Width = 489
    Height = 410
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object btnEmbeddings: TButton
    Left = 503
    Top = 72
    Width = 117
    Height = 25
    Caption = 'Embeddings Demo'
    TabOrder = 2
    OnClick = btnEmbeddingsClick
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Server=172.29.34.21'
      'Database=embeddings'
      'User_Name=myuser'
      'Password=password'
      'DriverID=PG')
    LoginPrompt = False
    Left = 536
    Top = 224
  end
  object FDPhysPgDriverLink1: TFDPhysPgDriverLink
    VendorLib = 
      'D:\Programming\ADUG\Symposium2023\Embeddings\Win32\Debug\libpq.d' +
      'll'
    Left = 552
    Top = 312
  end
end
