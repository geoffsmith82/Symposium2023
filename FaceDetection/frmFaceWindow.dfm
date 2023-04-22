object frmFaceDetection: TfrmFaceDetection
  Left = 0
  Top = 0
  Caption = 'Face Detection'
  ClientHeight = 343
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMainMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 16
    Width = 112
    Height = 13
    Caption = 'Photo URL with Face(s)'
  end
  object edtImageURL: TEdit
    Left = 32
    Top = 40
    Width = 513
    Height = 21
    TabOrder = 0
  end
  object btnDetectFaces: TButton
    Left = 568
    Top = 38
    Width = 83
    Height = 25
    Caption = 'Detect Faces'
    TabOrder = 1
    OnClick = btnDetectFacesClick
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 96
    Width = 690
    Height = 247
    ActivePage = tsResults
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object tsOriginalPhoto: TTabSheet
      Caption = 'Original Photo'
      object imgOriginal: TImage
        Left = 0
        Top = 0
        Width = 682
        Height = 219
        Align = alClient
        Proportional = True
        Stretch = True
        ExplicitLeft = 192
        ExplicitTop = 80
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object tsDetectedPhoto: TTabSheet
      Caption = 'Detected Photo'
      ImageIndex = 1
      object imgDetectedPhoto: TImage
        Left = 0
        Top = 0
        Width = 682
        Height = 219
        Align = alClient
        Proportional = True
        Stretch = True
        ExplicitLeft = 192
        ExplicitTop = 80
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object tsResults: TTabSheet
      Caption = 'Results'
      ImageIndex = 2
      object mmoResults: TMemo
        Left = 0
        Top = 0
        Width = 682
        Height = 219
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
        ExplicitLeft = -224
        ExplicitTop = -68
        ExplicitWidth = 505
        ExplicitHeight = 233
      end
    end
  end
  object mmMainMenu: TMainMenu
    Left = 600
    Top = 192
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
    object miFaceRecognitionEngine: TMenuItem
      Caption = 'Face Recognition Engine'
      object miGoogle: TMenuItem
        Caption = 'Google'
        GroupIndex = 10
        RadioItem = True
        OnClick = miSelectEngineClick
      end
      object miMicrosoft: TMenuItem
        Caption = 'Microsoft'
        GroupIndex = 10
        RadioItem = True
        OnClick = miSelectEngineClick
      end
    end
  end
end
