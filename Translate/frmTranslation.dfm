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
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
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
    object ranslationEngine1: TMenuItem
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
  end
end
