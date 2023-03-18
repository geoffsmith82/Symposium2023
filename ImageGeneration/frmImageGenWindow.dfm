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
  Menu = MainMenu1
  OldCreateOrder = True
  PixelsPerInch = 96
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
    Left = 520
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Execute'
    TabOrder = 1
    OnClick = btnExecuteClick
  end
  object JvImagesViewer1: TJvImagesViewer
    Left = 16
    Top = 119
    Width = 481
    Height = 306
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    FileMask = 
      '*.gif;*.cur;*.pcx;*.ani;*.svg;*.gif;*.png_old;*.svg;*.svg;*.gif;' +
      '*.jpg;*.jpeg;*.png;*.bmp;*.ico;*.emf;*.wmf;*.tif;*.tiff'
    Options.AutoCenter = False
    Options.HotTrack = False
    Options.ImagePadding = 20
    Options.MultiSelect = False
    SelectedIndex = -1
    TabOrder = 2
    TabStop = True
  end
  object seImageCount: TSpinEdit
    Left = 536
    Top = 63
    Width = 59
    Height = 24
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 1
  end
  object mmoOutput: TMemo
    Left = 232
    Top = 152
    Width = 345
    Height = 249
    Lines.Strings = (
      'mmoOutput')
    TabOrder = 4
  end
  object cboSize: TComboBox
    Left = 503
    Top = 104
    Width = 92
    Height = 23
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 5
    Text = '1024x1024'
    Items.Strings = (
      '256x256'
      '512x512'
      '1024x1024')
  end
  object MainMenu1: TMainMenu
    Left = 184
    Top = 120
    object File1: TMenuItem
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
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Generator1: TMenuItem
      Caption = 'Generator'
      object Generator2: TMenuItem
        Caption = 'OpenAI DALL-E 2'
        Checked = True
      end
    end
  end
end
