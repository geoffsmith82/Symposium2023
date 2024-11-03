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
        Control = mmoWeatherQuestion
        Row = 1
      end
      item
        Column = 0
        Control = mmWeatherAnswer
        Row = 2
      end
      item
        Column = 0
        Control = Panel1
        Row = 0
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
      end
      item
        SizeStyle = ssAuto
      end
      item
        SizeStyle = ssAuto
      end>
    TabOrder = 0
    ExplicitWidth = 517
    ExplicitHeight = 440
    object mmoWeatherQuestion: TMemo
      Left = 1
      Top = 60
      Width = 519
      Height = 211
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object mmWeatherAnswer: TMemo
      Left = 1
      Top = 271
      Width = 519
      Height = 169
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 519
      Height = 59
      Align = alClient
      Caption = 'Panel1'
      TabOrder = 2
      ExplicitTop = -5
      DesignSize = (
        519
        59)
      object btnLatestForcast: TButton
        Left = 116
        Top = 6
        Width = 249
        Height = 43
        Anchors = []
        Caption = 'Latest Forcast'
        TabOrder = 0
        OnClick = btnLatestForcastClick
      end
      object chkUseGPT4: TCheckBox
        Left = 380
        Top = 19
        Width = 97
        Height = 17
        Anchors = []
        Caption = 'Use GPT4'
        TabOrder = 1
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 680
    Top = 400
    object miFile: TMenuItem
      Caption = '&File'
      object miExit: TMenuItem
        Caption = 'Exit'
      end
    end
    object miSettings: TMenuItem
      Caption = 'Settings'
      object miAPIKeys: TMenuItem
        Caption = 'API Keys...'
        OnClick = miAPIKeysClick
      end
    end
  end
end
