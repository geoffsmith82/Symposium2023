object dmDiff: TdmDiff
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 234
  Width = 439
  object DosCommand: TDosCommand
    CommandLine = 'dcc32 -B  -NS"Vcl;System;Winapi" AITest3.dpr'
    CurrentDir = 'D:\Programming\ChatGPTAction\Projects\AITest3'
    InputToOutput = False
    MaxTimeAfterBeginning = 0
    MaxTimeAfterLastOutput = 0
    OnNewLine = DosCommandNewLine
    Left = 80
    Top = 48
  end
end
