object dmWeather: TdmWeather
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 242
  Width = 593
  object IdFTP1: TIdFTP
    Host = 'ftp.bom.gov.au'
    Passive = True
    ConnectTimeout = 0
    Password = 'anonymous'
    Username = 'anonymous'
    NATKeepAlive.UseKeepAlive = False
    NATKeepAlive.IdleTimeMS = 0
    NATKeepAlive.IntervalMS = 0
    ProxySettings.ProxyType = fpcmNone
    ProxySettings.Port = 0
    Left = 120
    Top = 72
  end
  object XMLDocument1: TXMLDocument
    Left = 124
    Top = 168
  end
end
