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
  object Memo1: TMemo
    Left = 40
    Top = 88
    Width = 505
    Height = 233
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
end
