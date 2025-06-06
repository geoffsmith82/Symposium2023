object frmFaceDetection: TfrmFaceDetection
  Left = 0
  Top = 0
  Margins.Left = 8
  Margins.Top = 8
  Margins.Right = 8
  Margins.Bottom = 8
  Caption = 'Face Detection'
  ClientHeight = 855
  ClientWidth = 1715
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -28
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMainMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 240
  TextHeight = 34
  object Label1: TLabel
    Left = 100
    Top = 40
    Width = 291
    Height = 34
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Photo URL with Face(s)'
  end
  object edtImageURL: TEdit
    Left = 80
    Top = 100
    Width = 1283
    Height = 42
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    TabOrder = 0
  end
  object btnDetectFaces: TButton
    Left = 1420
    Top = 95
    Width = 208
    Height = 63
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Detect Faces'
    TabOrder = 1
    OnClick = btnDetectFacesClick
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 240
    Width = 1715
    Height = 615
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    ActivePage = tsResults
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object tsOriginalPhoto: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Original Photo'
      object imgOriginal: TImage
        Left = 0
        Top = 0
        Width = 1695
        Height = 548
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Proportional = True
        Stretch = True
      end
    end
    object tsDetectedPhoto: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Detected Photo'
      ImageIndex = 1
      object imgDetectedPhoto: TImage
        Left = 0
        Top = 0
        Width = 1695
        Height = 548
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Proportional = True
        Stretch = True
      end
    end
    object tsResults: TTabSheet
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Caption = 'Results'
      ImageIndex = 2
      object mmoResults: TMemo
        Left = 0
        Top = 0
        Width = 1695
        Height = 548
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
      end
    end
  end
  object JvFilenameEdit1: TJvFilenameEdit
    Left = 80
    Top = 168
    Width = 1283
    Height = 42
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    ButtonWidth = 53
    TabOrder = 3
    Text = 'JvFilenameEdit1'
  end
  object btnDetectFacesFromLocalFile: TButton
    Left = 1420
    Top = 163
    Width = 208
    Height = 62
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Caption = 'Detect Faces'
    TabOrder = 4
    OnClick = btnDetectFacesFromLocalFileClick
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
      object miCodeProject: TMenuItem
        Caption = 'Code Project'
        GroupIndex = 10
        RadioItem = True
        OnClick = miSelectEngineClick
      end
    end
    object miGoogleMenu: TMenuItem
      Caption = 'Google'
      object miGoogleLogin: TMenuItem
        Caption = 'Login'
        OnClick = miGoogleLoginClick
      end
    end
    object miSettings: TMenuItem
      Caption = 'Settings'
      object miAPIKeys: TMenuItem
        Caption = 'API Keys...'
        OnClick = miAPIKeysClick
      end
    end
  end
end
