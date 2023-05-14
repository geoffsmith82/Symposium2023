object dmEmbeddings: TdmEmbeddings
  OnCreate = DataModuleCreate
  Height = 480
  Width = 640
  object EmbeddingConnection: TFDConnection
    Params.Strings = (
      'Server=172.20.74.30'
      'Database=embeddings'
      'User_Name=geoff'
      'Password=HelloWorld!'
      'CharacterSet=UTF8'
      'DriverID=PG')
    LoginPrompt = False
    Left = 48
    Top = 16
  end
  object PhysPgDriverLink: TFDPhysPgDriverLink
    VendorLib = 'C:\Program Files\PostgreSQL\psqlODBC\bin\libpq.dll'
    Left = 48
    Top = 104
  end
end
