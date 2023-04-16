object frmVoiceRecognition: TfrmVoiceRecognition
  Left = 0
  Top = 0
  Caption = 'Voice Recognition'
  ClientHeight = 424
  ClientWidth = 802
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mmMainMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    802
    424)
  TextHeight = 13
  object VirtualImage1: TVirtualImage
    Left = 712
    Top = 47
    Width = 82
    Height = 73
    Anchors = [akTop, akRight]
    ImageCollection = ImageCollection1
    ImageWidth = 0
    ImageHeight = 0
    ImageIndex = -1
  end
  object mmoQuestions: TMemo
    Left = 248
    Top = 8
    Width = 433
    Height = 151
    TabOrder = 0
  end
  object mmoAnswers: TMemo
    Left = 248
    Top = 173
    Width = 433
    Height = 137
    TabOrder = 1
  end
  object DBCtrlGrid1: TDBCtrlGrid
    Left = 0
    Top = 41
    Width = 225
    Height = 383
    Align = alLeft
    DataSource = dsSessions
    PanelHeight = 54
    PanelWidth = 208
    TabOrder = 2
    RowCount = 7
    ExplicitTop = 48
    ExplicitHeight = 371
    object DBText1: TDBText
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 202
      Height = 25
      Align = alTop
      DataField = 'CreationTime'
      DataSource = dsSessions
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 208
    end
  end
  object PnlButtons: TPanel
    Left = 0
    Top = 0
    Width = 802
    Height = 41
    Align = alTop
    TabOrder = 3
    ExplicitLeft = 448
    ExplicitTop = 112
    ExplicitWidth = 185
    object btnNewChatSession: TButton
      Left = 1
      Top = 1
      Width = 225
      Height = 39
      Align = alLeft
      Caption = 'New Chat'
      TabOrder = 0
      OnClick = btnNewChatSessionClick
      ExplicitLeft = 96
      ExplicitTop = -8
      ExplicitHeight = 49
    end
    object btnStart: TButton
      Left = 696
      Top = 1
      Width = 105
      Height = 39
      Align = alRight
      Caption = 'Start'
      TabOrder = 1
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 600
      Top = 1
      Width = 96
      Height = 39
      Align = alRight
      Caption = 'Stop'
      TabOrder = 2
      OnClick = btnStopClick
    end
  end
  object sgConversationGrid: TStringGrid
    Left = 248
    Top = 56
    Width = 448
    Height = 254
    Anchors = [akLeft, akTop, akRight]
    ColCount = 2
    FixedCols = 0
    TabOrder = 4
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
    Left = 56
    Top = 168
  end
  object AudioProcessor1: TAudioProcessor
    Input = DXAudioIn1
    OnGetData = AudioProcessor1GetData
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
    Left = 133
    Top = 203
  end
  object mmMainMenu: TMainMenu
    Left = 488
    Top = 336
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
    object miTextToSpeechEngine: TMenuItem
      Caption = 'Text to Speech Engine'
      object miMicrosoftSpeechEngine: TMenuItem
        AutoCheck = True
        Caption = 'Microsoft'
        RadioItem = True
        OnClick = SelectSpeechEngine
      end
      object miAmazonSpeechEngine: TMenuItem
        AutoCheck = True
        Caption = 'Amazon'
        RadioItem = True
        OnClick = SelectSpeechEngine
      end
      object miGoogleSpeechEngine: TMenuItem
        AutoCheck = True
        Caption = 'Google'
        RadioItem = True
        OnClick = SelectSpeechEngine
      end
      object miElevenLabsSpeechEngine: TMenuItem
        AutoCheck = True
        Caption = 'Eleven Labs'
        RadioItem = True
        OnClick = SelectSpeechEngine
      end
      object miWindowsSpeechEngine: TMenuItem
        AutoCheck = True
        Caption = 'Windows'
        RadioItem = True
        OnClick = SelectSpeechEngine
      end
    end
    object miSpeechRecognitionEngine: TMenuItem
      Caption = 'Speech Recognition Engine'
      object miDeepGram: TMenuItem
        Caption = 'DeepGram'
        GroupIndex = 5
        RadioItem = True
        OnClick = SelectSpeechRecognitionClick
      end
      object miAssemblyAI: TMenuItem
        Caption = 'AssemblyAI'
        GroupIndex = 5
        RadioItem = True
        OnClick = SelectSpeechRecognitionClick
      end
    end
    object miAudioInput: TMenuItem
      Caption = 'Audio Input'
    end
  end
  object UserInterfaceUpdateTimer: TTimer
    Interval = 50
    OnTimer = UserInterfaceUpdateTimerTimer
    Left = 320
    Top = 344
  end
  object ImageCollection1: TImageCollection
    Images = <
      item
        Name = 'microphone'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000060000000600803000000D54687
              0A000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              0252504C5445E4E6EAD4D7DDD1D4DBDDDFE4B1B7C36D798D41506B293A592032
              521E305025365536466259667E959EADA0A8B53A4A6617294A162849192C4C1C
              2E4E1D2F4F1C2E4F1A2D4D172A4B142749253756737E92E3E6EAE5E7EA57657D
              1528491427482F405DB3B9C43C4C681527499FA7B53F4E6A16294A1B2E4E1B2D
              4EADB4BF606C831A2C4D283958D7DAE0AAB1BD5A687F42516C192B4CBFC4CDB9
              BFC9182A4B677389747F9330415E46556F1D2F50C7CBD3A6ADBADEE0E5929AAA
              D8DBE02133538992A3D8DBE1213352D5D9DFDCDFE4D3D6DDE7E9ECE2E4E8A8AF
              BCA1A8B6A5ACB9D6DADFB1B7C2A2A9B7C6CBD3B0B6C1A3ABB8A7AEBBA4ABB9A1
              A9B6E1E4E8A4ABB8DDE0E5ADB3BFCACED5D9DCE2A2A9B6C5CAD2DADCE2ABB1BD
              C6CAD3DADDE28892A3C6CAD2E8EAEDD9DCE12334548B95A5D5D8DEE3E5E99AA2
              B0CCD0D7384865B5BBC6ACB2BEC0C5CE5B687F1C2F4F233554DADDE3E1E3E8A2
              AAB7DBDEE3B4BAC5A3AAB7A3AAB8B1B8C3969EAD182B4C475670CFD3DAC1C6CF
              A4ACB99199A9BABFC9A9B0BC6E7A8E1B2D4D2E3E5CE6E8ECA8AFBBB7BDC7B4BA
              C42A3B59969FAED2D6DCCCD0D8B6BCC6E0E2E7A7AEBA5C6980AFB6C18891A2D2
              D5DC939CAB56647CBEC3CC2637568A93A44C5A7316284A3646637D879AD3D7DD
              828C9E6D798E69758A798396949DACC1C5CEBBC0CAB2B8C3D4D8DEE0E3E7ABB2
              BEA9AFBCC9CDD5D3D6DCB8BDC7CFD2D9CDD1D8AFB5C0D0D3DACACED6B7BCC7B9
              BEC8D6D9DFE2E5E9BCC1CBD1D5DBC7CCD4BAC0CADFE2E6A7ADBAD0D4DAD0D4DB
              BDC2CCB8BEC8B0B6C2DCDEE3A0A7B5AEB4C0B5BBC50000006FFA6582000000C6
              74524E53FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFF00C61AE7EA000000097048597300000EC300000EC301C7
              6FA86400000476494441546843ED98F97714451080B904820A249B84A0380B46
              3018131550912812E4C8B41948320EAB23108907A2C60B99F120C8A28057E4F2
              44B9C40888A27880CAA540FF5F7475D7E6ED4EFA0AB8EFC973BF5F52DD5D5BDF
              74F7F6CC6C86D02253121829098C940446FE77822143870DC7D092C108465C33
              72D4E8B231D75E77FD2024F682B1E3C69757A42A2BABAA27D44CBCE146EC3562
              2B9874935351D64F2A3D790A0E98B0140CBFB9166B2315A95B70C880A560EA34
              2CDCCFAD75D3714C8F9DE0B63496CDA3FEF6061CD56225681C70FD8073070E6B
              B112DC7917D62CA0B27E068EEBB011CC9C85251394DF8D093A6C04F738583141
              DDBD98A0C346305BBA426565F7D5CDC10C0D3682C97558314979136668B010DC
              3FB10A0B26711EC0140D1682B90F5662C124B5F330458385A0B9D882A2CFA0F8
              82F9258181AB7F934BE7C048D1BE45CD0F89BF66C182858B4420412958DCE2BA
              E461888C8256D7F596F0488252B0D423A4AD1D22D31ECC7009E9F01F81508252
              101086BF8C4526410652DD472194A0122C6AE382C758683A078D20081F875082
              522066B09C85A619ACE08295104AF84F080CDFA22B1274B25023780252856015
              841254822E217892859D4F29054F43AAD864D53B984A40A13EF19F61D1EA5129
              2C98C47916325B40E0AF81508252D0DED1FFB1E7EAB1609209CFC3B0B8946110
              4A500A5E80EBF2BA217C713C164CF0D2CBAFB0D157D782207C0D32252805EB22
              F6B1F87508DF48FC3ECBF1E65B30BADE0741D005B104A5A087DD8BC806FE76BB
              608C7C13D26FC3E84698419085508652D0C02FCC5F08F1A61A2C59C03BEF6E86
              C12DB096D1560865280562EFA2F7205CF67E3516CDA7E60318FB505CC84710CB
              500B7A611348F431C4DB660D3C0AE9ED3C6D470C69018F65A80543D96D9E09D6
              F1C6CEF2E436D4CEE603BBF804304B865A204E2809D7F3C6275505BF0453E94F
              79B7D801E2AB7F326B046BF8140222523E1BE9F4AF5375FAF32F7827EDE58720
              DE215A3234029AE10F9D780536C32F473BCEEE694E6DC5575FE3B1EA0E218384
              70E014E8049D7014D80267B04DF7447BF7ED3FF00D3FDEC04151DFFB16DB3274
              02DAC31789442DF2FBC056BEC1A4AD11DB52B402DA27E610443DD891C777591C
              0C0E618F14BD801EE68781ADF291EFB10769EE0BF9F7872DD051EC926310D02C
              1A02BFE9871F73C90DDD8743ECEFF08F61A702938066C442B01B5FE447ED9911
              537ECA12DFC3AB678BF733E6A9300A68AFBB01ABB1EB0DE238C81567785BE0A1
              ADC52CA0C7496E120902FF174CD16021A05DBF461E3C400B89DDCC6F98A0C346
              40E9EF279ADCB5794B43623F6AE58F0A237602C6C999D908972A0E49DF1FB61F
              B41630261DE3F5A33FFFC20E1B0623A0F4146C857B125B565C86C0377DF50B18
              9C601B179CC6961597338333D8B2C24AB079CE09E0EC39569E9DAFBFCFF2E6C1
              B938ACC546F04FEC7A9CDC8D4FB4C2F3FFD67F7E5BB1F000C2D598A1C146D09E
              7F86F371F58F028E8DE0027FB792E0F3B7323D36823D63BD584274BE171374D8
              0868E7C65609178FE3B0162BC19550121829098C940446AE7601A597000D5214
              58151628D30000000049454E44AE426082}
          end>
      end
      item
        Name = 'speak'
        SourceImages = <
          item
            Image.Data = {
              89504E470D0A1A0A0000000D4948445200000060000000600803000000D54687
              0A000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
              0258504C5445FFFFFFD3D6DD969EAE848EA0949DACC9CED6BAC0CA2D3E5C1729
              4A1A2C4D172A4B2B3C59A9B0BCB0B6C12B3D5B1D2F4F1E30502A3B58C0C5CEF0
              F1F32C3D5B838D9EE8EAED54627A1628491C2E4E3F4E69EDEFF1E0E2E6485670
              1C2E4F273857EAECEFD6DAE03E4D69192B4CCED1D9354562C4C9D130415EBABF
              C9EBECEFDDE0E5C8CCD4C6CAD3CCD0D7EAECEE949CABDDDFE4ABB1BDA4ABB9A5
              ACB9A3AAB7ADB4C0E5E7EB54637ACACED5A4ABB8ACB3BFE3E5E9475670C9CDD5
              E6E8EBD6DADFDDE0E4ACB2BDAEB4C0EBEDF0CDD1D8364662ACB3BEA3AAB81B2E
              4ED9DCE1D9DDE2EFF0F2E4E6EAE3E6E9D8DBE1B1B7C28B94A541506C21335220
              32512032521F31511D3050CED1D8A3ABB8CDD0D7C3C7D03B4B66142748ACB2BE
              2B3C5AE2E4E8CFD3DACBD0D7BCC2CBD9DCE2A2A9B7CDD0D8E8EAEE344460E0E3
              E7E7E9EC9EA6B416284AA1A8B6E9EBEDCBCED6C8CDD542516CEFF0F3ADB4BFCA
              CED6A2A9B61A2D4DDBDEE3E2E5E9ECEDF0A1A9B6DBDDE2A2AAB7D3D6DCCBCFD7
              16294AEEF0F2CFD2D9D6D9DFCACDD5D7DAE0DCDEE3D1D5DCC7CBD3D0D4DBE1E3
              E7D4D7DEDCDFE4D5D8DDEAEBEEE5E7EAC8CDD4D5D8DEA4ACB9D5D9DED7DBE03C
              4C679099A8182A4BCED2D9ACB4BFD4D8DDCFD3D9939BAB152849D2D5DCD4D7DD
              E9EAEE4A5872B7BDC7EDEEF1C7CCD4E6E8EC2B3D5AC5C9D1818C9D31415F1527
              49142749C7CCD3DEE1E5B9BEC8B9BFC853617AE4E5E9DFE1E6919AA91D2F50B8
              BDC7E5E8EBC2C7D02F405EDBDDE3344561D5D8DF45546EEDEFF2526078ECEEF0
              2D3D5B3D4D69ECEEF11B2D4E7B869952607941506B9FA7B43646639099A9CA7B
              E201000000097048597300000EC300000EC301C76FA8640000042F4944415468
              43ED98F97B134518C7A782428BA59160B4B6D6A30AD83658B22992D4B68A3545
              52CAA140C12E2D128F4245A95041AC0A8AA0728327B715904351F1C00B6FFDB7
              DC99F966763333FB641337CFE30FFBF921DDEFF4D9F7BBF3EEFBBEBB09090808
              080808F89F5276CDB8F1D7E2B8145C37616279C5A4EBA1FCA7727255C8E28629
              D03E131E3F95860F85CA6FC48ABF446EBA99C70F55DF5283353FA9BDB502F143
              75B7DD8E451FB9E3CE7A842F8DC15D779723BA45090CA64DAF46708AFF063326
              B0F2CCE2BB41E5D49CF896C13DF88FA0A1B1293A13C78512BE17E52F5077D03C
              2B66C45B66431546E4BE6CF90B54833909C33012C956C842A8BD7F22C2DAA806
              6DD4C08825DBA1BDD3E1287F816AF0C083D4C048CC7D080B5EE97CD851FE024D
              1575A49843D71C688F4C9B3E0F3173D0189047B8436A3EB43BE9EE56CE82EE64
              6EF90B7406A4070E11683DED0B172D5EF228E7B1A5F5CB105142D30716CBE3D4
              A07705A496958FF7D5575709104F41BB0312AE64A56476436B58B8CA3971DC71
              18F40F9489E35696A4C46A4895272621403E6C8335B3E299542D0479B28B3AA4
              9E82946978DA352712C2A02649239ACF704506D916326590326BD7E1FCBC0883
              D6211AD1483ECB2559CF26C6735012E1E7B535AF43186C186606BD2F7049E66F
              A432A9AB31AB8E37B914A58A7D0F464CE660BEC8E56696B22D235C4974AA43CD
              0D471FBCC4428AE267436FEBCB50B98CBE82D3F363EFC0BA2E5639C3AF72F51A
              6DB6DE4E2E248A3488B02D0CA154B7652C91D8CE854491063C2B99D7B978638B
              25628D5C48146B3040B312DFC1452D3530DEE442A258839DCC602717BB4A60B0
              9AA5E82D2EDEF63F45E9167ACDD9D2EFA19D9668E342A22003BB57DFD94A0D52
              BBB9DA41F395887221112D6A077BD8B048EC851CA5F9CADE10897D9AD707176C
              83C1E1FDD4207300BA89AA8D07A172D97DA80EE7E7C53668A4576C182D692EF9
              232735C895CCE177717E5E844104B3AE994BDE132E554AC87BF22BA82BC26026
              BBE2F73FE08AD4B08A8A7F08A9F0D11104C8879DA2F5D6EC491D8520C7E82412
              B35BE5F8098F85641BA4579C6CC21822E414DB40EC63480D638B3ED1BD292A38
              FB208CBF16CBF9335FFFB801A7CF9C3DF2E9398AF5D9E7E666EFC04933BB21FB
              CF43BA7161E545F0D9C5CF5D52A635B8C41E0C86F905B417D25FEA0B4B67307B
              3846E3778D427B64EEE4AF10D489C6A03F8996DB8C05AF5C96BEFF315483AF79
              C719E62E2C78E79B6FD5F755D520CA0AC830BF832E84E8954D882B500DF6B204
              993D9085F1FD2179CEAADF0F06680F9B6BA00AA5E107F13B0B47DDC18F6DE650
              2A3BF20A67ECA7DCF7624D15913D233FE3A818A486D019FC57721AA21406E4F2
              55BB214A62E06C88D218381AA2EE975FB1E62FFBB20DF1DBEF58F19B6C43F4AD
              C582EF8CFDD15715AA5AF76771BF3C7961C35F7F575CF9A71FAA24B41F737D69
              080808080808D041C8BFA91E4A17192A17AB0000000049454E44AE426082}
          end>
      end>
    Left = 592
    Top = 320
  end
  object FDConnection: TFDConnection
    Params.Strings = (
      
        'Database=D:\Programming\ADUG\Symposium2023\VoiceRecognition\chat' +
        's.mdb'
      'DriverID=MSAcc')
    Connected = True
    LoginPrompt = False
    Left = 720
    Top = 232
  end
  object tblSessions: TFDTable
    AfterScroll = tblSessionsAfterScroll
    IndexFieldNames = 'SessionID'
    Connection = FDConnection
    ResourceOptions.AssignedValues = [rvEscapeExpand]
    TableName = 'Sessions'
    Left = 248
    Top = 312
  end
  object tblConversion: TFDTable
    MasterSource = dsSessions
    MasterFields = 'SessionID'
    Connection = FDConnection
    TableName = 'Conversation'
    Left = 248
    Top = 368
  end
  object dsSessions: TDataSource
    DataSet = tblSessions
    Left = 152
    Top = 296
  end
  object dsConversation: TDataSource
    DataSet = tblConversion
    Left = 144
    Top = 368
  end
end
