object dmPersonalAlarms: TdmPersonalAlarms
  Height = 223
  Width = 590
  object Connection: TFDConnection
    Params.Strings = (
      'DriverID=MSAcc'
      'Database=D:\Programming\ChatGPTAction\alarms.mdb')
    LoginPrompt = False
    Left = 80
    Top = 32
  end
end
