object VoiceRecognitionForm: TVoiceRecognitionForm
  Left = 0
  Top = 0
  Caption = 'Voice Recognition'
  ClientHeight = 475
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
    Left = 40
    Top = 14
    Width = 53
    Height = 15
    Caption = 'Audio File'
  end
  object edtFilename: TEdit
    Left = 24
    Top = 35
    Width = 353
    Height = 23
    TabOrder = 0
  end
  object btnBrowse: TButton
    Left = 383
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object ComboBox1: TComboBox
    Left = 232
    Top = 64
    Width = 145
    Height = 23
    TabOrder = 2
    Text = 'ComboBox1'
  end
  object btnRecognizeSpeech: TButton
    Left = 472
    Top = 33
    Width = 113
    Height = 25
    Caption = 'Recognize Speech'
    TabOrder = 3
  end
  object Memo1: TMemo
    Left = 24
    Top = 96
    Width = 553
    Height = 305
    Lines.Strings = (
      'Memo1')
    TabOrder = 4
  end
  object OpenDialog: TOpenDialog
    Left = 432
    Top = 88
  end
  object MainMenu1: TMainMenu
    Left = 352
    Top = 208
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
      object miExit: TMenuItem
        Caption = 'E&xit'
        OnClick = miExitClick
      end
    end
    object miEngine: TMenuItem
      Caption = 'Engine'
      object miMicrosoft: TMenuItem
        Caption = 'Microsoft'
      end
      object miGoogle: TMenuItem
        Caption = 'Google'
      end
      object miAmazon: TMenuItem
        Caption = 'Amazon'
      end
      object miOpenAIWhisper: TMenuItem
        Caption = 'Open AI Whisper'
      end
      object miOpenAIWhisperLocal: TMenuItem
        Caption = 'Open AI Whisper (Local)'
      end
    end
  end
end
