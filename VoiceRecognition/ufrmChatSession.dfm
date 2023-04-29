object frmNewChatSession: TfrmNewChatSession
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'New Chat Session'
  ClientHeight = 303
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object btnNewSession: TButton
    Left = 512
    Top = 40
    Width = 89
    Height = 25
    Caption = 'New Session'
    Default = True
    TabOrder = 0
    OnClick = btnNewSessionClick
  end
  object btnCancel: TButton
    Left = 512
    Top = 96
    Width = 89
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object DBLookupListBox1: TDBLookupListBox
    Left = 64
    Top = 40
    Width = 409
    Height = 109
    DataField = 'PromptID'
    DataSource = dsSelected
    KeyField = 'PromptID'
    ListField = 'Personality'
    ListSource = dsChatSessionList
    TabOrder = 2
  end
  object dsChatSessionList: TDataSource
    DataSet = tblChatSessionList
    Left = 552
    Top = 168
  end
  object tblChatSessionList: TFDTable
    Active = True
    IndexFieldNames = 'PromptID'
    Connection = frmVoiceRecognition.FDConnection
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'ChatBotRoles'
    Left = 488
    Top = 144
  end
  object tblSelectedInitialSessionPrompt: TFDMemTable
    Active = True
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 440
    Top = 200
    object tblSelectedInitialSessionPromptPromptID: TLargeintField
      FieldName = 'PromptID'
    end
  end
  object dsSelected: TDataSource
    DataSet = tblSelectedInitialSessionPrompt
    Left = 544
    Top = 232
  end
end
