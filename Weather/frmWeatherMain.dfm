object frmWeatherWindow: TfrmWeatherWindow
  Left = 0
  Top = 0
  Caption = 'Bendigo Weather'
  ClientHeight = 552
  ClientWidth = 636
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 20
  object btnLatestForcast: TButton
    Left = 20
    Top = 10
    Width = 132
    Height = 32
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Latest Forcast'
    TabOrder = 0
    OnClick = btnLatestForcastClick
  end
  object Memo1: TMemo
    Left = 20
    Top = 50
    Width = 612
    Height = 480
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object MediaPlayer1: TMediaPlayer
    Left = 50
    Top = 420
    Width = 316
    Height = 38
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Visible = False
    ParentDoubleBuffered = False
    TabOrder = 2
  end
end
