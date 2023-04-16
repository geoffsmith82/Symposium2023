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
  object MediaPlayer1: TMediaPlayer
    Left = 40
    Top = 336
    Width = 253
    Height = 30
    DoubleBuffered = True
    Visible = False
    ParentDoubleBuffered = False
    TabOrder = 0
  end
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 521
    Height = 441
    Align = alClient
    Caption = 'GridPanel1'
    ColumnCollection = <
      item
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = btnLatestForcast
        Row = 0
      end
      item
        Column = 0
        Control = mmoWeatherQuestion
        Row = 1
      end
      item
        Column = 0
        Control = mmWeatherAnswer
        Row = 2
      end>
    RowCollection = <
      item
        Value = 13.461538461538460000
      end
      item
        Value = 48.076923076923070000
      end
      item
        Value = 38.461538461538460000
      end>
    TabOrder = 1
    ExplicitLeft = 235
    ExplicitTop = 40
    ExplicitWidth = 185
    ExplicitHeight = 377
    DesignSize = (
      521
      441)
    object btnLatestForcast: TButton
      Left = 136
      Top = 9
      Width = 249
      Height = 43
      Anchors = []
      Caption = 'Latest Forcast'
      TabOrder = 0
      OnClick = btnLatestForcastClick
      ExplicitTop = 0
    end
    object mmoWeatherQuestion: TMemo
      Left = 1
      Top = 60
      Width = 519
      Height = 211
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object mmWeatherAnswer: TMemo
      Left = 1
      Top = 271
      Width = 519
      Height = 169
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 2
    end
  end
end
