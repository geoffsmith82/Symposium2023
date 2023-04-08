object frmWeatherWindow: TfrmWeatherWindow
  Left = 0
  Top = 0
  Caption = 'Bendigo Weather'
  ClientHeight = 441
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object btnLatestForcast: TButton
    Left = 16
    Top = 8
    Width = 106
    Height = 26
    Caption = 'Latest Forcast'
    TabOrder = 0
    OnClick = btnLatestForcastClick
  end
  object Memo1: TMemo
    Left = 16
    Top = 40
    Width = 490
    Height = 384
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object MediaPlayer1: TMediaPlayer
    Left = 40
    Top = 336
    Width = 253
    Height = 30
    DoubleBuffered = True
    Visible = False
    ParentDoubleBuffered = False
    TabOrder = 2
  end
end
