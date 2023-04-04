object frmMainTranslationWindow: TfrmMainTranslationWindow
  Left = 0
  Top = 0
  Caption = 'Translate Language'
  ClientHeight = 471
  ClientWidth = 787
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu1
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object mmoSourceText: TMemo
    Left = 24
    Top = 8
    Width = 545
    Height = 177
    Lines.Strings = (
      'mmoSourceText')
    TabOrder = 0
  end
  object mmoTranslatedText: TMemo
    Left = 24
    Top = 208
    Width = 545
    Height = 177
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object btnTranslate: TButton
    Left = 600
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Translate'
    TabOrder = 2
    OnClick = btnTranslateClick
  end
  object Button1: TButton
    Left = 600
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 600
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 4
    OnClick = Button2Click
  end
  object MainMenu1: TMainMenu
    Left = 84
    Top = 56
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
    object TranslationEngine1: TMenuItem
      Caption = 'Translation Engine'
      object miMicrosoft: TMenuItem
        Caption = 'Microsoft Translate'
        RadioItem = True
        OnClick = miMicrosoftClick
      end
      object miGoogle: TMenuItem
        Caption = 'Google Translate'
        RadioItem = True
        OnClick = miMicrosoftClick
      end
      object miAmazonTranslate: TMenuItem
        Caption = 'Amazon Translate'
        RadioItem = True
        OnClick = miMicrosoftClick
      end
    end
    object miFromLanguage: TMenuItem
      Caption = 'From Language'
    end
    object miToLanguage: TMenuItem
      Caption = 'To Language'
    end
    object GoogleAuthenticate1: TMenuItem
      Caption = 'Google Authenticate'
      OnClick = GoogleAuthenticate1Click
    end
  end
end
