object dmWeather: TdmWeather
  OldCreateOrder = True
  Height = 1500
  Width = 2000
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
