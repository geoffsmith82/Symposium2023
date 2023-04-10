object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Voice Recognition'
  ClientHeight = 424
  ClientWidth = 658
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Button1: TButton
    Left = 568
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 122
    Width = 537
    Height = 151
    TabOrder = 1
  end
  object Button2: TButton
    Left = 568
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ListBox1: TListBox
    Left = 16
    Top = 8
    Width = 537
    Height = 97
    ItemHeight = 13
    TabOrder = 3
    OnClick = ListBox1Click
  end
  object Memo2: TMemo
    Left = 8
    Top = 279
    Width = 537
    Height = 137
    TabOrder = 4
  end
  object MediaPlayer1: TMediaPlayer
    Left = 587
    Top = 196
    Width = 29
    Height = 30
    VisibleButtons = [btPlay]
    DoubleBuffered = True
    Visible = False
    ParentDoubleBuffered = False
    TabOrder = 5
  end
  object DXAudioIn1: TDXAudioIn
    Latency = 100
    SamplesToRead = -1
    DeviceNumber = 0
    InBitsPerSample = 16
    InChannels = 1
    InSampleRate = 16000
    RecTime = -1
    EchoRecording = False
    FramesInBuffer = 16000
    PollingInterval = 100
    Left = 96
    Top = 80
  end
  object AudioProcessor1: TAudioProcessor
    Input = DXAudioIn1
    OnGetBitsPerSample = AudioProcessor1GetBitsPerSample
    OnGetChannels = AudioProcessor1GetChannels
    OnGetData = AudioProcessor1GetData
    OnGetSampleRate = AudioProcessor1GetSampleRate
    Left = 272
    Top = 64
  end
  object StreamOut1: TStreamOut
    Input = AudioProcessor1
    Left = 280
    Top = 224
  end
  object sgcWebSocketClient1: TsgcWebSocketClient
    Host = 'api.deepgram.com'
    Port = 443
    ConnectTimeout = 0
    ReadTimeout = -1
    WriteTimeout = 0
    TLS = False
    Proxy.Enabled = True
    Proxy.Port = 8080
    Proxy.ProxyType = pxyHTTP
    HeartBeat.Enabled = False
    HeartBeat.Interval = 300
    HeartBeat.Timeout = 0
    IPVersion = Id_IPv4
    OnMessage = sgcWebSocketClient1Message
    OnHandshake = sgcWebSocketClient1Handshake
    Authentication.Enabled = False
    Authentication.URL.Enabled = True
    Authentication.Session.Enabled = False
    Authentication.Basic.Enabled = False
    Authentication.Token.Enabled = False
    Authentication.Token.AuthName = 'Bearer'
    Extensions.DeflateFrame.Enabled = True
    Extensions.DeflateFrame.WindowBits = 15
    Extensions.PerMessage_Deflate.Enabled = False
    Extensions.PerMessage_Deflate.ClientMaxWindowBits = 15
    Extensions.PerMessage_Deflate.ClientNoContextTakeOver = False
    Extensions.PerMessage_Deflate.MemLevel = 9
    Extensions.PerMessage_Deflate.ServerMaxWindowBits = 15
    Extensions.PerMessage_Deflate.ServerNoContextTakeOver = False
    Options.CleanDisconnect = False
    Options.FragmentedMessages = frgOnlyBuffer
    Options.Parameters = '/'
    Options.RaiseDisconnectExceptions = True
    Options.ValidateUTF8 = False
    Specifications.Drafts.Hixie76 = False
    Specifications.RFC6455 = True
    NotifyEvents = neNoSync
    LogFile.Enabled = False
    QueueOptions.Binary.Level = qmNone
    QueueOptions.Ping.Level = qmNone
    QueueOptions.Text.Level = qmNone
    WatchDog.Attempts = 0
    WatchDog.Enabled = False
    WatchDog.Interval = 10
    Throttle.BitsPerSec = 0
    Throttle.Enabled = False
    LoadBalancer.Enabled = False
    LoadBalancer.Port = 0
    TLSOptions.VerifyCertificate = False
    TLSOptions.VerifyDepth = 0
    TLSOptions.Version = tlsUndefined
    TLSOptions.IOHandler = iohOpenSSL
    TLSOptions.OpenSSL_Options.APIVersion = oslAPI_1_0
    TLSOptions.OpenSSL_Options.LibPath = oslpNone
    TLSOptions.OpenSSL_Options.UnixSymLinks = oslsSymLinksDefault
    TLSOptions.SChannel_Options.CertStoreName = scsnMY
    TLSOptions.SChannel_Options.CertStorePath = scspStoreCurrentUser
    Left = 157
    Top = 115
  end
  object MainMenu1: TMainMenu
    Left = 576
    Top = 128
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
    object SpeechEngine1: TMenuItem
      Caption = 'Speech Engine'
      object miMicrosoftSpeechEngine: TMenuItem
        AutoCheck = True
        Caption = 'Microsoft'
        RadioItem = True
        OnClick = miMicrosoftSpeechEngineClick
      end
      object miAmazonSpeechEngine: TMenuItem
        AutoCheck = True
        Caption = 'Amazon'
        RadioItem = True
        OnClick = miAmazonSpeechEngineClick
      end
      object miGoogleSpeechEngine: TMenuItem
        AutoCheck = True
        Caption = 'Google'
        RadioItem = True
        OnClick = miGoogleSpeechEngineClick
      end
      object miElevenLabsSpeechEngine: TMenuItem
        AutoCheck = True
        Caption = 'Eleven Labs'
        RadioItem = True
        OnClick = miElevenLabsSpeechEngineClick
      end
      object miWindowsSpeechEngine: TMenuItem
        AutoCheck = True
        Caption = 'Windows'
        RadioItem = True
        OnClick = miWindowsSpeechEngineClick
      end
    end
  end
  object Timer1: TTimer
    Interval = 50
    OnTimer = Timer1Timer
    Left = 568
    Top = 88
  end
end
