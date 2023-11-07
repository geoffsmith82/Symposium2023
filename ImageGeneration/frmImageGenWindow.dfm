object frmImageGenerator: TfrmImageGenerator
  Left = 0
  Top = 0
  Caption = 'Image Generator'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = mmMainMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    628
    442)
  TextHeight = 15
  object Label1: TLabel
    Left = 24
    Top = 3
    Width = 76
    Height = 15
    Caption = 'Image Prompt'
  end
  object mmoImagePrompt: TMemo
    Left = 16
    Top = 24
    Width = 481
    Height = 89
    TabOrder = 0
  end
  object btnExecute: TButton
    Left = 496
    Top = 23
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Execute'
    TabOrder = 1
    OnClick = btnExecuteClick
    ExplicitLeft = 492
  end
  object seImageCount: TSpinEdit
    Left = 512
    Top = 54
    Width = 59
    Height = 24
    Anchors = [akTop, akRight]
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 1
    ExplicitLeft = 508
  end
  object cboSize: TComboBox
    Left = 479
    Top = 84
    Width = 92
    Height = 23
    Style = csDropDownList
    Anchors = [akTop, akRight]
    ItemIndex = 2
    TabOrder = 3
    Text = '1024x1024'
    Items.Strings = (
      '256x256'
      '512x512'
      '1024x1024')
    ExplicitLeft = 475
  end
  object ScrollBox1: TScrollBox
    Left = 16
    Top = 128
    Width = 569
    Height = 289
    VertScrollBar.Range = 2000
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoScroll = False
    TabOrder = 4
    ExplicitWidth = 565
    ExplicitHeight = 288
    object ImagesFlowPanel: TFlowPanel
      Left = 0
      Top = 0
      Width = 548
      Height = 2000
      Align = alTop
      Caption = 'ImagesFlowPanel'
      TabOrder = 0
      ExplicitLeft = 48
      ExplicitTop = 48
      ExplicitWidth = 185
      ExplicitHeight = 41
    end
  end
  object mmMainMenu: TMainMenu
    Left = 184
    Top = 120
    object miFile: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New'
      end
      object Open1: TMenuItem
        Caption = '&Open...'
      end
      object Save1: TMenuItem
        Caption = '&Save'
      end
      object SaveAs1: TMenuItem
        Caption = 'Save &As...'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Caption = '&Print...'
      end
      object PrintSetup1: TMenuItem
        Caption = 'P&rint Setup...'
      end
      object N1: TMenuItem
        Caption = '-'
      end
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
