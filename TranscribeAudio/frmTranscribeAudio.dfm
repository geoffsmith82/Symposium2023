object VoiceRecognitionForm: TVoiceRecognitionForm
  Left = 0
  Top = 0
  Caption = 'Transcribe Audio'
  ClientHeight = 474
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = mmMainMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
    Visible = False
  end
  object btnRecognizeSpeech: TButton
    Left = 472
    Top = 33
    Width = 113
    Height = 25
    Caption = 'Recognize Speech'
    TabOrder = 3
    OnClick = btnRecognizeSpeechClick
  end
  object Memo1: TMemo
    Left = 24
    Top = 96
    Width = 553
    Height = 305
    TabOrder = 4
  end
  object OpenDialog: TOpenDialog
    Left = 432
    Top = 88
  end
  object mmMainMenu: TMainMenu
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
      Caption = 'Recognition Engine'
      object miMicrosoft: TMenuItem
        AutoCheck = True
        Caption = 'Microsoft'
        GroupIndex = 1
        RadioItem = True
        OnClick = miEngineSelectedClick
      end
      object miGoogle: TMenuItem
        AutoCheck = True
        Caption = 'Google'
        GroupIndex = 1
        RadioItem = True
        OnClick = miEngineSelectedClick
      end
      object miAmazon: TMenuItem
        AutoCheck = True
        Caption = 'Amazon'
        GroupIndex = 1
        RadioItem = True
        OnClick = miEngineSelectedClick
      end
      object miOpenAIWhisper: TMenuItem
        AutoCheck = True
        Caption = 'Open AI Whisper'
        GroupIndex = 1
        RadioItem = True
        OnClick = miEngineSelectedClick
      end
      object miOpenAIWhisperLocal: TMenuItem
        AutoCheck = True
        Caption = 'Open AI Whisper (Local)'
        GroupIndex = 1
        RadioItem = True
        OnClick = miEngineSelectedClick
      end
    end
    object miGoogleMenu: TMenuItem
      Caption = 'Google'
      object GoogleAuthenticate1: TMenuItem
        Caption = 'Google Authenticate'
        OnClick = GoogleAuthenticate1Click
      end
    end
  end
end
