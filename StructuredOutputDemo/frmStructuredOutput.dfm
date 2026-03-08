object frmStructuredOutputDemo: TfrmStructuredOutputDemo
  Left = 0
  Top = 0
  Caption = 'Structured Output Demo (PasDantic)'
  ClientHeight = 700
  ClientWidth = 1100
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  Position = poScreenCenter
  DesignSize = (
    1100
    700)
  TextHeight = 15
  object splMain: TSplitter
    Left = 320
    Top = 0
    Width = 5
    Height = 670
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 670
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 0
    object grpProvider: TGroupBox
      Left = 8
      Top = 8
      Width = 304
      Height = 70
      Align = alTop
      Caption = ' Provider '
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      Padding.Bottom = 4
      TabOrder = 0
      object lblProvider: TLabel
        Left = 10
        Top = 22
        Width = 45
        Height = 15
        Caption = 'Provider:'
      end
      object cboProvider: TComboBox
        Left = 10
        Top = 40
        Width = 280
        Height = 23
        Style = csDropDownList
        TabOrder = 0
      end
    end
    object grpSchema: TGroupBox
      Left = 8
      Top = 78
      Width = 304
      Height = 100
      Align = alTop
      Caption = ' Schema '
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      Padding.Bottom = 4
      TabOrder = 1
      object lblSchema: TLabel
        Left = 10
        Top = 22
        Width = 78
        Height = 15
        Caption = 'Output Schema:'
      end
      object cboSchema: TComboBox
        Left = 10
        Top = 40
        Width = 280
        Height = 23
        Style = csDropDownList
        TabOrder = 0
        OnChange = cboSchemaChange
      end
      object btnViewSchema: TButton
        Left = 10
        Top = 68
        Width = 280
        Height = 25
        Caption = 'View JSON Schema'
        TabOrder = 1
        OnClick = btnViewSchemaClick
      end
    end
    object grpPrompt: TGroupBox
      Left = 8
      Top = 178
      Width = 304
      Height = 452
      Align = alClient
      Caption = ' Prompt '
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      Padding.Bottom = 4
      TabOrder = 2
      object mmoPrompt: TMemo
        Left = 6
        Top = 19
        Width = 292
        Height = 392
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object btnExecute: TButton
        Left = 6
        Top = 411
        Width = 292
        Height = 35
        Align = alBottom
        Caption = 'Execute Structured Query'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = btnExecuteClick
      end
    end
  end
  object pnlRight: TPanel
    Left = 325
    Top = 0
    Width = 775
    Height = 670
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 1
    object grpResponse: TGroupBox
      Left = 8
      Top = 8
      Width = 759
      Height = 430
      Align = alClient
      Caption = ' Structured Response '
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      Padding.Bottom = 4
      TabOrder = 0
      object mmoResponse: TMemo
        Left = 6
        Top = 19
        Width = 747
        Height = 405
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object grpLog: TGroupBox
      Left = 8
      Top = 438
      Width = 759
      Height = 224
      Align = alBottom
      Caption = ' Log '
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      Padding.Bottom = 4
      TabOrder = 1
      object mmoLog: TMemo
        Left = 6
        Top = 19
        Width = 747
        Height = 199
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object lblStatus: TLabel
    Left = 8
    Top = 678
    Width = 35
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Ready'
  end
  object lblTokens: TLabel
    Left = 600
    Top = 678
    Width = 3
    Height = 15
    Anchors = [akRight, akBottom]
  end
  object MainMenu: TMainMenu
    Left = 544
    Top = 16
    object miFile: TMenuItem
      Caption = '&File'
      object miExit: TMenuItem
        Caption = 'E&xit'
        OnClick = miExitClick
      end
    end
    object miSettings: TMenuItem
      Caption = '&Settings'
      object miAPIKeys: TMenuItem
        Caption = 'API &Keys...'
        OnClick = miAPIKeysClick
      end
    end
  end
end
