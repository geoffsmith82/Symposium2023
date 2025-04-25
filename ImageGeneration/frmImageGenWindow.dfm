object frmImageGenerator: TfrmImageGenerator
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Caption = 'Image Generator'
  ClientHeight = 1105
  ClientWidth = 1570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -30
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = mmMainMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 240
  DesignSize = (
    1570
    1105)
  TextHeight = 41
  object Label1: TLabel
    Left = 60
    Top = 8
    Width = 190
    Height = 41
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Image Prompt'
  end
  object Label2: TLabel
    Left = 1069
    Top = 13
    Width = 187
    Height = 41
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Balance: $2.10'
  end
  object mmoImagePrompt: TMemo
    Left = 53
    Top = 135
    Width = 1203
    Height = 223
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    TabOrder = 0
  end
  object btnExecute: TButton
    Left = 1323
    Top = 129
    Width = 188
    Height = 62
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Anchors = [akTop, akRight]
    Caption = 'Execute'
    TabOrder = 1
    OnClick = btnExecuteClick
  end
  object seImageCount: TSpinEdit
    Left = 1363
    Top = 241
    Width = 148
    Height = 52
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Anchors = [akTop, akRight]
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 1
  end
  object cboSize: TComboBox
    Left = 1288
    Top = 309
    Width = 230
    Height = 49
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Style = csDropDownList
    Anchors = [akTop, akRight]
    ItemIndex = 2
    TabOrder = 3
    Text = '1024x1024'
    Items.Strings = (
      '256x256'
      '512x512'
      '1024x1024')
  end
  object ScrollBox: TScrollBox
    Left = 40
    Top = 400
    Width = 1471
    Height = 643
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    VertScrollBar.Range = 5000
    VertScrollBar.Smooth = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoScroll = False
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 4
    UseWheelForScrolling = True
  end
  object Button1: TButton
    Left = 1297
    Top = 13
    Width = 198
    Height = 41
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Buy Credits...'
    TabOrder = 5
  end
  object mmMainMenu: TMainMenu
    Left = 184
    Top = 120
    object miFile: TMenuItem
      Caption = '&File'
      object miExit: TMenuItem
        Caption = 'E&xit'
        OnClick = miExitClick
      end
    end
    object miGenerator: TMenuItem
      Caption = 'Generator'
    end
    object miSetup: TMenuItem
      Caption = 'Setup'
      object miAPIKeys: TMenuItem
        Caption = 'API Keys...'
        OnClick = miAPIKeysClick
      end
    end
  end
  object pmPopupMenu: TPopupMenu
    Left = 368
    Top = 32
    object miSaveImage: TMenuItem
      Caption = 'Save Image...'
      OnClick = miSaveImageClick
    end
  end
  object SaveDialog: TSaveDialog
    Left = 264
    Top = 40
  end
end
