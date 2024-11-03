object frmWeatherWindow: TfrmWeatherWindow
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Caption = 'Bendigo Weather'
  ClientHeight = 1103
  ClientWidth = 1303
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -30
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 240
  TextHeight = 41
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 1303
    Height = 1103
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
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
    object mmoWeatherQuestion: TMemo
      Left = 1
      Top = 149
      Width = 1301
      Height = 530
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object mmWeatherAnswer: TMemo
      Left = 1
      Top = 679
      Width = 1301
      Height = 423
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 1301
      Height = 148
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      Caption = 'Panel1'
      TabOrder = 2
      DesignSize = (
        1301
        148)
      object btnLatestForcast: TButton
        Left = 291
        Top = 15
        Width = 623
        Height = 108
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Anchors = []
        Caption = 'Latest Forcast'
        TabOrder = 0
        OnClick = btnLatestForcastClick
      end
      object chkUseGPT4: TCheckBox
        Left = 952
        Top = 48
        Width = 243
        Height = 42
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
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
        OnClick = miExitClick
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
