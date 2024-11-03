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
  object mmoImagePrompt: TMemo
    Left = 40
    Top = 60
    Width = 1203
    Height = 223
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    TabOrder = 0
  end
  object btnExecute: TButton
    Left = 1272
    Top = 58
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
    Left = 1312
    Top = 135
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
    Left = 1230
    Top = 210
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
  object ScrollBox1: TScrollBox
    Left = 40
    Top = 320
    Width = 1455
    Height = 723
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    VertScrollBar.Range = 5000
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoScroll = False
    TabOrder = 4
    object ImagesFlowPanel: TFlowPanel
      Left = 0
      Top = 0
      Width = 1408
      Height = 5000
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      Caption = 'ImagesFlowPanel'
      TabOrder = 0
    end
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
      object miDALLE2: TMenuItem
        AutoCheck = True
        Caption = 'OpenAI DALL-E 2'
        GroupIndex = 100
        RadioItem = True
      end
      object miDALLE3: TMenuItem
        AutoCheck = True
        Caption = 'OpenAI DALL-E 3'
        Checked = True
        GroupIndex = 100
        RadioItem = True
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
