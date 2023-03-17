unit SpeechLib_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 98336 $
// File generated on 19/01/2023 12:37:16 AM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Windows\System32\Speech\Common\sapi.dll (1)
// LIBID: {C866CA3A-32F7-11D2-9602-00C04F8EE628}
// LCID: 0
// Helpfile: 
// HelpString: Microsoft Speech Object Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// Errors:
//   Hint: Parameter 'Object' of ISpeechObjectToken.CreateInstance changed to 'Object_'
//   Hint: Parameter 'Object' of ISpeechObjectToken.IsUISupported changed to 'Object_'
//   Hint: Parameter 'Object' of ISpeechObjectToken.DisplayUI changed to 'Object_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Parameter 'Type' of ISpeechVoice.Skip changed to 'Type_'
//   Hint: Parameter 'Type' of ISpeechRecognizer.GetFormat changed to 'Type_'
//   Hint: Parameter 'Type' of ISpeechGrammarRuleState.AddWordTransition changed to 'Type_'
//   Hint: Parameter 'Type' of ISpeechGrammarRuleState.AddSpecialTransition changed to 'Type_'
//   Hint: Parameter 'Type' of ISpeechGrammarRuleStateTransition.Type changed to 'Type_'
//   Hint: Parameter 'Property' of ISpeechPhraseProperties.Item changed to 'Property_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleServer, Winapi.ActiveX;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  SpeechLibMajorVersion = 5;
  SpeechLibMinorVersion = 4;

  LIBID_SpeechLib: TGUID = '{C866CA3A-32F7-11D2-9602-00C04F8EE628}';

  IID_ISpeechDataKey: TGUID = '{CE17C09B-4EFA-44D5-A4C9-59D9585AB0CD}';
  IID_ISpeechObjectToken: TGUID = '{C74A3ADC-B727-4500-A84A-B526721C8B8C}';
  IID_ISpeechObjectTokenCategory: TGUID = '{CA7EAC50-2D01-4145-86D4-5AE7D70F4469}';
  IID_ISpeechObjectTokens: TGUID = '{9285B776-2E7B-4BC0-B53E-580EB6FA967F}';
  IID_ISpeechAudioBufferInfo: TGUID = '{11B103D8-1142-4EDF-A093-82FB3915F8CC}';
  IID_ISpeechAudioStatus: TGUID = '{C62D9C91-7458-47F6-862D-1EF86FB0B278}';
  IID_ISpeechAudioFormat: TGUID = '{E6E9C590-3E18-40E3-8299-061F98BDE7C7}';
  IID_ISpeechWaveFormatEx: TGUID = '{7A1EF0D5-1581-4741-88E4-209A49F11A10}';
  IID_ISpeechBaseStream: TGUID = '{6450336F-7D49-4CED-8097-49D6DEE37294}';
  IID_ISpeechFileStream: TGUID = '{AF67F125-AB39-4E93-B4A2-CC2E66E182A7}';
  IID_ISpeechMemoryStream: TGUID = '{EEB14B68-808B-4ABE-A5EA-B51DA7588008}';
  IID_ISpeechCustomStream: TGUID = '{1A9E9F4F-104F-4DB8-A115-EFD7FD0C97AE}';
  IID_ISpeechAudio: TGUID = '{CFF8E175-019E-11D3-A08E-00C04F8EF9B5}';
  IID_ISpeechMMSysAudio: TGUID = '{3C76AF6D-1FD7-4831-81D1-3B71D5A13C44}';
  IID_ISpeechVoice: TGUID = '{269316D8-57BD-11D2-9EEE-00C04F797396}';
  IID_ISpeechVoiceStatus: TGUID = '{8BE47B07-57F6-11D2-9EEE-00C04F797396}';
  DIID__ISpeechVoiceEvents: TGUID = '{A372ACD1-3BEF-4BBD-8FFB-CB3E2B416AF8}';
  IID_ISpeechRecognizer: TGUID = '{2D5F1C0C-BD75-4B08-9478-3B11FEA2586C}';
  IID_ISpeechRecognizerStatus: TGUID = '{BFF9E781-53EC-484E-BB8A-0E1B5551E35C}';
  IID_ISpeechRecoContext: TGUID = '{580AA49D-7E1E-4809-B8E2-57DA806104B8}';
  IID_ISpeechRecoGrammar: TGUID = '{B6D6F79F-2158-4E50-B5BC-9A9CCD852A09}';
  IID_ISpeechGrammarRules: TGUID = '{6FFA3B44-FC2D-40D1-8AFC-32911C7F1AD1}';
  IID_ISpeechGrammarRule: TGUID = '{AFE719CF-5DD1-44F2-999C-7A399F1CFCCC}';
  IID_ISpeechGrammarRuleState: TGUID = '{D4286F2C-EE67-45AE-B928-28D695362EDA}';
  IID_ISpeechGrammarRuleStateTransitions: TGUID = '{EABCE657-75BC-44A2-AA7F-C56476742963}';
  IID_ISpeechGrammarRuleStateTransition: TGUID = '{CAFD1DB1-41D1-4A06-9863-E2E81DA17A9A}';
  IID_ISpeechTextSelectionInformation: TGUID = '{3B9C7E7A-6EEE-4DED-9092-11657279ADBE}';
  IID_ISpeechRecoResult: TGUID = '{ED2879CF-CED9-4EE6-A534-DE0191D5468D}';
  IID_ISpeechRecoResultTimes: TGUID = '{62B3B8FB-F6E7-41BE-BDCB-056B1C29EFC0}';
  IID_ISpeechPhraseInfo: TGUID = '{961559CF-4E67-4662-8BF0-D93F1FCD61B3}';
  IID_ISpeechPhraseRule: TGUID = '{A7BFE112-A4A0-48D9-B602-C313843F6964}';
  IID_ISpeechPhraseRules: TGUID = '{9047D593-01DD-4B72-81A3-E4A0CA69F407}';
  IID_ISpeechPhraseProperties: TGUID = '{08166B47-102E-4B23-A599-BDB98DBFD1F4}';
  IID_ISpeechPhraseProperty: TGUID = '{CE563D48-961E-4732-A2E1-378A42B430BE}';
  IID_ISpeechPhraseElements: TGUID = '{0626B328-3478-467D-A0B3-D0853B93DDA3}';
  IID_ISpeechPhraseElement: TGUID = '{E6176F96-E373-4801-B223-3B62C068C0B4}';
  IID_ISpeechPhraseReplacements: TGUID = '{38BC662F-2257-4525-959E-2069D2596C05}';
  IID_ISpeechPhraseReplacement: TGUID = '{2890A410-53A7-4FB5-94EC-06D4998E3D02}';
  IID_ISpeechPhraseAlternates: TGUID = '{B238B6D5-F276-4C3D-A6C1-2974801C3CC2}';
  IID_ISpeechPhraseAlternate: TGUID = '{27864A2A-2B9F-4CB8-92D3-0D2722FD1E73}';
  DIID__ISpeechRecoContextEvents: TGUID = '{7B8FCB42-0E9D-4F00-A048-7B04D6179D3D}';
  IID_ISpeechRecoResult2: TGUID = '{8E0A246D-D3C8-45DE-8657-04290C458C3C}';
  IID_ISpeechLexicon: TGUID = '{3DA7627A-C7AE-4B23-8708-638C50362C25}';
  IID_ISpeechLexiconWords: TGUID = '{8D199862-415E-47D5-AC4F-FAA608B424E6}';
  IID_ISpeechLexiconWord: TGUID = '{4E5B933C-C9BE-48ED-8842-1EE51BB1D4FF}';
  IID_ISpeechLexiconPronunciations: TGUID = '{72829128-5682-4704-A0D4-3E2BB6F2EAD3}';
  IID_ISpeechLexiconPronunciation: TGUID = '{95252C5D-9E43-4F4A-9899-48EE73352F9F}';
  IID_ISpeechXMLRecoResult: TGUID = '{AAEC54AF-8F85-4924-944D-B79D39D72E19}';
  IID_ISpeechRecoResultDispatch: TGUID = '{6D60EB64-ACED-40A6-BBF3-4E557F71DEE2}';
  IID_ISpeechPhraseInfoBuilder: TGUID = '{3B151836-DF3A-4E0A-846C-D2ADC9334333}';
  IID_ISpeechPhoneConverter: TGUID = '{C3E4F353-433F-43D6-89A1-6A62A7054C3D}';
  IID_ISpNotifySink: TGUID = '{259684DC-37C3-11D2-9603-00C04F8EE628}';
  IID_ISpNotifyTranslator: TGUID = '{ACA16614-5D3D-11D2-960E-00C04F8EE628}';
  CLASS_SpNotifyTranslator: TGUID = '{E2AE5372-5D40-11D2-960E-00C04F8EE628}';
  IID_ISpDataKey: TGUID = '{14056581-E16C-11D2-BB90-00C04F8EE6C0}';
  IID_ISpObjectTokenCategory: TGUID = '{2D3D3845-39AF-4850-BBF9-40B49780011D}';
  CLASS_SpObjectTokenCategory: TGUID = '{A910187F-0C7A-45AC-92CC-59EDAFB77B53}';
  IID_IEnumSpObjectTokens: TGUID = '{06B64F9E-7FDA-11D2-B4F2-00C04F797396}';
  IID_ISpObjectToken: TGUID = '{14056589-E16C-11D2-BB90-00C04F8EE6C0}';
  CLASS_SpObjectToken: TGUID = '{EF411752-3736-4CB4-9C8C-8EF4CCB58EFE}';
  IID_IServiceProvider: TGUID = '{6D5140C1-7436-11CE-8034-00AA006009FA}';
  IID_ISpResourceManager: TGUID = '{93384E18-5014-43D5-ADBB-A78E055926BD}';
  CLASS_SpResourceManager: TGUID = '{96749373-3391-11D2-9EE3-00C04F797396}';
  IID_ISpStreamFormat: TGUID = '{BED530BE-2606-4F4D-A1C0-54C5CDA5566F}';
  IID_ISpStreamFormatConverter: TGUID = '{678A932C-EA71-4446-9B41-78FDA6280A29}';
  CLASS_SpStreamFormatConverter: TGUID = '{7013943A-E2EC-11D2-A086-00C04F8EF9B5}';
  CLASS_SpMMAudioEnum: TGUID = '{AB1890A0-E91F-11D2-BB91-00C04F8EE6C0}';
  IID_ISpNotifySource: TGUID = '{5EFF4AEF-8487-11D2-961C-00C04F8EE628}';
  IID_ISpEventSource: TGUID = '{BE7A9CCE-5F9E-11D2-960F-00C04F8EE628}';
  IID_ISpEventSink: TGUID = '{BE7A9CC9-5F9E-11D2-960F-00C04F8EE628}';
  IID_ISpObjectWithToken: TGUID = '{5B559F40-E952-11D2-BB91-00C04F8EE6C0}';
  IID_ISpAudio: TGUID = '{C05C768F-FAE8-4EC2-8E07-338321C12452}';
  IID_ISpMMSysAudio: TGUID = '{15806F6E-1D70-4B48-98E6-3B1A007509AB}';
  CLASS_SpMMAudioIn: TGUID = '{CF3D2E50-53F2-11D2-960C-00C04F8EE628}';
  CLASS_SpMMAudioOut: TGUID = '{A8C680EB-3D32-11D2-9EE7-00C04F797396}';
  IID_ISpStream: TGUID = '{12E3CCA9-7518-44C5-A5E7-BA5A79CB929E}';
  CLASS_SpStream: TGUID = '{715D9C59-4442-11D2-9605-00C04F8EE628}';
  IID_ISpVoice: TGUID = '{6C44DF74-72B9-4992-A1EC-EF996E0422D4}';
  IID_ISpPhoneticAlphabetSelection: TGUID = '{B2745EFD-42CE-48CA-81F1-A96E02538A90}';
  CLASS_SpVoice: TGUID = '{96749377-3391-11D2-9EE3-00C04F797396}';
  IID_ISpRecoContext: TGUID = '{F740A62F-7C15-489E-8234-940A33D9272D}';
  IID_ISpRecoContext2: TGUID = '{BEAD311C-52FF-437F-9464-6B21054CA73D}';
  IID_ISpProperties: TGUID = '{5B4FB971-B115-4DE1-AD97-E482E3BF6EE4}';
  IID_ISpRecognizer: TGUID = '{C2B5F241-DAA0-4507-9E16-5A1EAA2B7A5C}';
  IID_ISpPhrase: TGUID = '{1A5C0354-B621-4B5A-8791-D306ED379E53}';
  IID_ISpGrammarBuilder: TGUID = '{8137828F-591A-4A42-BE58-49EA7EBAAC68}';
  IID_ISpRecoGrammar: TGUID = '{2177DB29-7F45-47D0-8554-067E91C80502}';
  IID_ISpRecoResult: TGUID = '{20B053BE-E235-43CD-9A2A-8D17A48B7842}';
  IID_ISpPhraseAlt: TGUID = '{8FCEBC98-4E49-4067-9C6C-D86A0E092E3D}';
  CLASS_SpSharedRecoContext: TGUID = '{47206204-5ECA-11D2-960F-00C04F8EE628}';
  IID_ISpRecognizer2: TGUID = '{8FC6D974-C81E-4098-93C5-0147F61ED4D3}';
  IID_ISpRecognizer3: TGUID = '{DF1B943C-5838-4AA2-8706-D7CD5B333499}';
  IID_ISpSerializeState: TGUID = '{21B501A0-0EC7-46C9-92C3-A2BC784C54B9}';
  IID_ISpRecoCategory: TGUID = '{DA0CD0F9-14A2-4F09-8C2A-85CC48979345}';
  CLASS_SpInprocRecognizer: TGUID = '{41B89B6B-9399-11D2-9623-00C04F8EE628}';
  CLASS_SpSharedRecognizer: TGUID = '{3BEE4890-4FE9-4A37-8C1E-5E7E12791C1F}';
  IID_ISpLexicon: TGUID = '{DA41A7C2-5383-4DB2-916B-6C1719E3DB58}';
  CLASS_SpLexicon: TGUID = '{0655E396-25D0-11D3-9C26-00C04F8EF87C}';
  CLASS_SpUnCompressedLexicon: TGUID = '{C9E37C15-DF92-4727-85D6-72E5EEB6995A}';
  CLASS_SpCompressedLexicon: TGUID = '{90903716-2F42-11D3-9C26-00C04F8EF87C}';
  IID_ISpShortcut: TGUID = '{3DF681E2-EA56-11D9-8BDE-F66BAD1E3F3A}';
  CLASS_SpShortcut: TGUID = '{0D722F1A-9FCF-4E62-96D8-6DF8F01A26AA}';
  IID_ISpPhoneConverter: TGUID = '{8445C581-0CAC-4A38-ABFE-9B2CE2826455}';
  CLASS_SpPhoneConverter: TGUID = '{9185F743-1143-4C28-86B5-BFF14F20E5C8}';
  IID_ISpPhoneticAlphabetConverter: TGUID = '{133ADCD4-19B4-4020-9FDC-842E78253B17}';
  CLASS_SpPhoneticAlphabetConverter: TGUID = '{4F414126-DFE3-4629-99EE-797978317EAD}';
  CLASS_SpNullPhoneConverter: TGUID = '{455F24E9-7396-4A16-9715-7C0FDBE3EFE3}';
  CLASS_SpTextSelectionInformation: TGUID = '{0F92030A-CBFD-4AB8-A164-FF5985547FF6}';
  CLASS_SpPhraseInfoBuilder: TGUID = '{C23FC28D-C55F-4720-8B32-91F73C2BD5D1}';
  CLASS_SpAudioFormat: TGUID = '{9EF96870-E160-4792-820D-48CF0649E4EC}';
  CLASS_SpWaveFormatEx: TGUID = '{C79A574C-63BE-44B9-801F-283F87F898BE}';
  CLASS_SpInProcRecoContext: TGUID = '{73AD6842-ACE0-45E8-A4DD-8795881A2C2A}';
  CLASS_SpCustomStream: TGUID = '{8DBEF13F-1948-4AA8-8CF0-048EEBED95D8}';
  CLASS_SpFileStream: TGUID = '{947812B3-2AE1-4644-BA86-9E90DED7EC91}';
  CLASS_SpMemoryStream: TGUID = '{5FB7EF7D-DFF4-468A-B6B7-2FCBD188F994}';
  IID_ISpXMLRecoResult: TGUID = '{AE39362B-45A8-4074-9B9E-CCF49AA2D0B6}';
  IID_ISpRecoGrammar2: TGUID = '{4B37BC9E-9ED6-44A3-93D3-18F022B79EC3}';
  IID_ISpeechResourceLoader: TGUID = '{B9AC5783-FCD0-4B21-B119-B4F8DA8FD2C3}';
  IID_IInternetSecurityManager: TGUID = '{79EAC9EE-BAF9-11CE-8C82-00AA004BA90B}';
  IID_IInternetSecurityMgrSite: TGUID = '{79EAC9ED-BAF9-11CE-8C82-00AA004BA90B}';
  IID_IEnumString: TGUID = '{00000101-0000-0000-C000-000000000046}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum SpeechDataKeyLocation
type
  SpeechDataKeyLocation = TOleEnum;
const
  SDKLDefaultLocation = $00000000;
  SDKLCurrentUser = $00000001;
  SDKLLocalMachine = $00000002;
  SDKLCurrentConfig = $00000005;

// Constants for enum SpeechTokenContext
type
  SpeechTokenContext = TOleEnum;
const
  STCInprocServer = $00000001;
  STCInprocHandler = $00000002;
  STCLocalServer = $00000004;
  STCRemoteServer = $00000010;
  STCAll = $00000017;

// Constants for enum SpeechTokenShellFolder
type
  SpeechTokenShellFolder = TOleEnum;
const
  STSF_AppData = $0000001A;
  STSF_LocalAppData = $0000001C;
  STSF_CommonAppData = $00000023;
  STSF_FlagCreate = $00008000;

// Constants for enum SpeechAudioState
type
  SpeechAudioState = TOleEnum;
const
  SASClosed = $00000000;
  SASStop = $00000001;
  SASPause = $00000002;
  SASRun = $00000003;

// Constants for enum SpeechAudioFormatType
type
  SpeechAudioFormatType = TOleEnum;
const
  SAFTDefault = $FFFFFFFF;
  SAFTNoAssignedFormat = $00000000;
  SAFTText = $00000001;
  SAFTNonStandardFormat = $00000002;
  SAFTExtendedAudioFormat = $00000003;
  SAFT8kHz8BitMono = $00000004;
  SAFT8kHz8BitStereo = $00000005;
  SAFT8kHz16BitMono = $00000006;
  SAFT8kHz16BitStereo = $00000007;
  SAFT11kHz8BitMono = $00000008;
  SAFT11kHz8BitStereo = $00000009;
  SAFT11kHz16BitMono = $0000000A;
  SAFT11kHz16BitStereo = $0000000B;
  SAFT12kHz8BitMono = $0000000C;
  SAFT12kHz8BitStereo = $0000000D;
  SAFT12kHz16BitMono = $0000000E;
  SAFT12kHz16BitStereo = $0000000F;
  SAFT16kHz8BitMono = $00000010;
  SAFT16kHz8BitStereo = $00000011;
  SAFT16kHz16BitMono = $00000012;
  SAFT16kHz16BitStereo = $00000013;
  SAFT22kHz8BitMono = $00000014;
  SAFT22kHz8BitStereo = $00000015;
  SAFT22kHz16BitMono = $00000016;
  SAFT22kHz16BitStereo = $00000017;
  SAFT24kHz8BitMono = $00000018;
  SAFT24kHz8BitStereo = $00000019;
  SAFT24kHz16BitMono = $0000001A;
  SAFT24kHz16BitStereo = $0000001B;
  SAFT32kHz8BitMono = $0000001C;
  SAFT32kHz8BitStereo = $0000001D;
  SAFT32kHz16BitMono = $0000001E;
  SAFT32kHz16BitStereo = $0000001F;
  SAFT44kHz8BitMono = $00000020;
  SAFT44kHz8BitStereo = $00000021;
  SAFT44kHz16BitMono = $00000022;
  SAFT44kHz16BitStereo = $00000023;
  SAFT48kHz8BitMono = $00000024;
  SAFT48kHz8BitStereo = $00000025;
  SAFT48kHz16BitMono = $00000026;
  SAFT48kHz16BitStereo = $00000027;
  SAFTTrueSpeech_8kHz1BitMono = $00000028;
  SAFTCCITT_ALaw_8kHzMono = $00000029;
  SAFTCCITT_ALaw_8kHzStereo = $0000002A;
  SAFTCCITT_ALaw_11kHzMono = $0000002B;
  SAFTCCITT_ALaw_11kHzStereo = $0000002C;
  SAFTCCITT_ALaw_22kHzMono = $0000002D;
  SAFTCCITT_ALaw_22kHzStereo = $0000002E;
  SAFTCCITT_ALaw_44kHzMono = $0000002F;
  SAFTCCITT_ALaw_44kHzStereo = $00000030;
  SAFTCCITT_uLaw_8kHzMono = $00000031;
  SAFTCCITT_uLaw_8kHzStereo = $00000032;
  SAFTCCITT_uLaw_11kHzMono = $00000033;
  SAFTCCITT_uLaw_11kHzStereo = $00000034;
  SAFTCCITT_uLaw_22kHzMono = $00000035;
  SAFTCCITT_uLaw_22kHzStereo = $00000036;
  SAFTCCITT_uLaw_44kHzMono = $00000037;
  SAFTCCITT_uLaw_44kHzStereo = $00000038;
  SAFTADPCM_8kHzMono = $00000039;
  SAFTADPCM_8kHzStereo = $0000003A;
  SAFTADPCM_11kHzMono = $0000003B;
  SAFTADPCM_11kHzStereo = $0000003C;
  SAFTADPCM_22kHzMono = $0000003D;
  SAFTADPCM_22kHzStereo = $0000003E;
  SAFTADPCM_44kHzMono = $0000003F;
  SAFTADPCM_44kHzStereo = $00000040;
  SAFTGSM610_8kHzMono = $00000041;
  SAFTGSM610_11kHzMono = $00000042;
  SAFTGSM610_22kHzMono = $00000043;
  SAFTGSM610_44kHzMono = $00000044;

// Constants for enum SpeechStreamSeekPositionType
type
  SpeechStreamSeekPositionType = TOleEnum;
const
  SSSPTRelativeToStart = $00000000;
  SSSPTRelativeToCurrentPosition = $00000001;
  SSSPTRelativeToEnd = $00000002;

// Constants for enum SpeechStreamFileMode
type
  SpeechStreamFileMode = TOleEnum;
const
  SSFMOpenForRead = $00000000;
  SSFMOpenReadWrite = $00000001;
  SSFMCreate = $00000002;
  SSFMCreateForWrite = $00000003;

// Constants for enum SpeechRunState
type
  SpeechRunState = TOleEnum;
const
  SRSEDone = $00000001;
  SRSEIsSpeaking = $00000002;

// Constants for enum SpeechVoiceEvents
type
  SpeechVoiceEvents = TOleEnum;
const
  SVEStartInputStream = $00000002;
  SVEEndInputStream = $00000004;
  SVEVoiceChange = $00000008;
  SVEBookmark = $00000010;
  SVEWordBoundary = $00000020;
  SVEPhoneme = $00000040;
  SVESentenceBoundary = $00000080;
  SVEViseme = $00000100;
  SVEAudioLevel = $00000200;
  SVEPrivate = $00008000;
  SVEAllEvents = $000083FE;

// Constants for enum SpeechVoicePriority
type
  SpeechVoicePriority = TOleEnum;
const
  SVPNormal = $00000000;
  SVPAlert = $00000001;
  SVPOver = $00000002;

// Constants for enum SpeechVoiceSpeakFlags
type
  SpeechVoiceSpeakFlags = TOleEnum;
const
  SVSFDefault = $00000000;
  SVSFlagsAsync = $00000001;
  SVSFPurgeBeforeSpeak = $00000002;
  SVSFIsFilename = $00000004;
  SVSFIsXML = $00000008;
  SVSFIsNotXML = $00000010;
  SVSFPersistXML = $00000020;
  SVSFNLPSpeakPunc = $00000040;
  SVSFParseSapi = $00000080;
  SVSFParseSsml = $00000100;
  SVSFParseAutodetect = $00000000;
  SVSFNLPMask = $00000040;
  SVSFParseMask = $00000180;
  SVSFVoiceMask = $000001FF;
  SVSFUnusedFlags = $FFFFFE00;

// Constants for enum SpeechVisemeFeature
type
  SpeechVisemeFeature = TOleEnum;
const
  SVF_None = $00000000;
  SVF_Stressed = $00000001;
  SVF_Emphasis = $00000002;

// Constants for enum SpeechVisemeType
type
  SpeechVisemeType = TOleEnum;
const
  SVP_0 = $00000000;
  SVP_1 = $00000001;
  SVP_2 = $00000002;
  SVP_3 = $00000003;
  SVP_4 = $00000004;
  SVP_5 = $00000005;
  SVP_6 = $00000006;
  SVP_7 = $00000007;
  SVP_8 = $00000008;
  SVP_9 = $00000009;
  SVP_10 = $0000000A;
  SVP_11 = $0000000B;
  SVP_12 = $0000000C;
  SVP_13 = $0000000D;
  SVP_14 = $0000000E;
  SVP_15 = $0000000F;
  SVP_16 = $00000010;
  SVP_17 = $00000011;
  SVP_18 = $00000012;
  SVP_19 = $00000013;
  SVP_20 = $00000014;
  SVP_21 = $00000015;

// Constants for enum SpeechRecognizerState
type
  SpeechRecognizerState = TOleEnum;
const
  SRSInactive = $00000000;
  SRSActive = $00000001;
  SRSActiveAlways = $00000002;
  SRSInactiveWithPurge = $00000003;

// Constants for enum SpeechInterference
type
  SpeechInterference = TOleEnum;
const
  SINone = $00000000;
  SINoise = $00000001;
  SINoSignal = $00000002;
  SITooLoud = $00000003;
  SITooQuiet = $00000004;
  SITooFast = $00000005;
  SITooSlow = $00000006;

// Constants for enum SpeechRecoEvents
type
  SpeechRecoEvents = TOleEnum;
const
  SREStreamEnd = $00000001;
  SRESoundStart = $00000002;
  SRESoundEnd = $00000004;
  SREPhraseStart = $00000008;
  SRERecognition = $00000010;
  SREHypothesis = $00000020;
  SREBookmark = $00000040;
  SREPropertyNumChange = $00000080;
  SREPropertyStringChange = $00000100;
  SREFalseRecognition = $00000200;
  SREInterference = $00000400;
  SRERequestUI = $00000800;
  SREStateChange = $00001000;
  SREAdaptation = $00002000;
  SREStreamStart = $00004000;
  SRERecoOtherContext = $00008000;
  SREAudioLevel = $00010000;
  SREPrivate = $00040000;
  SREAllEvents = $0005FFFF;

// Constants for enum SpeechRecoContextState
type
  SpeechRecoContextState = TOleEnum;
const
  SRCS_Disabled = $00000000;
  SRCS_Enabled = $00000001;

// Constants for enum SpeechRetainedAudioOptions
type
  SpeechRetainedAudioOptions = TOleEnum;
const
  SRAONone = $00000000;
  SRAORetainAudio = $00000001;

// Constants for enum SpeechGrammarState
type
  SpeechGrammarState = TOleEnum;
const
  SGSEnabled = $00000001;
  SGSDisabled = $00000000;
  SGSExclusive = $00000003;

// Constants for enum SpeechRuleAttributes
type
  SpeechRuleAttributes = TOleEnum;
const
  SRATopLevel = $00000001;
  SRADefaultToActive = $00000002;
  SRAExport = $00000004;
  SRAImport = $00000008;
  SRAInterpreter = $00000010;
  SRADynamic = $00000020;
  SRARoot = $00000040;

// Constants for enum SpeechGrammarRuleStateTransitionType
type
  SpeechGrammarRuleStateTransitionType = TOleEnum;
const
  SGRSTTEpsilon = $00000000;
  SGRSTTWord = $00000001;
  SGRSTTRule = $00000002;
  SGRSTTDictation = $00000003;
  SGRSTTWildcard = $00000004;
  SGRSTTTextBuffer = $00000005;

// Constants for enum SpeechGrammarWordType
type
  SpeechGrammarWordType = TOleEnum;
const
  SGDisplay = $00000000;
  SGLexical = $00000001;
  SGPronounciation = $00000002;
  SGLexicalNoSpecialChars = $00000003;

// Constants for enum SpeechSpecialTransitionType
type
  SpeechSpecialTransitionType = TOleEnum;
const
  SSTTWildcard = $00000001;
  SSTTDictation = $00000002;
  SSTTTextBuffer = $00000003;

// Constants for enum SpeechLoadOption
type
  SpeechLoadOption = TOleEnum;
const
  SLOStatic = $00000000;
  SLODynamic = $00000001;

// Constants for enum SpeechRuleState
type
  SpeechRuleState = TOleEnum;
const
  SGDSInactive = $00000000;
  SGDSActive = $00000001;
  SGDSActiveWithAutoPause = $00000003;
  SGDSActiveUserDelimited = $00000004;

// Constants for enum SpeechWordPronounceable
type
  SpeechWordPronounceable = TOleEnum;
const
  SWPUnknownWordUnpronounceable = $00000000;
  SWPUnknownWordPronounceable = $00000001;
  SWPKnownWordPronounceable = $00000002;

// Constants for enum SpeechEngineConfidence
type
  SpeechEngineConfidence = TOleEnum;
const
  SECLowConfidence = $FFFFFFFF;
  SECNormalConfidence = $00000000;
  SECHighConfidence = $00000001;

// Constants for enum SpeechDisplayAttributes
type
  SpeechDisplayAttributes = TOleEnum;
const
  SDA_No_Trailing_Space = $00000000;
  SDA_One_Trailing_Space = $00000002;
  SDA_Two_Trailing_Spaces = $00000004;
  SDA_Consume_Leading_Spaces = $00000008;

// Constants for enum SpeechDiscardType
type
  SpeechDiscardType = TOleEnum;
const
  SDTProperty = $00000001;
  SDTReplacement = $00000002;
  SDTRule = $00000004;
  SDTDisplayText = $00000008;
  SDTLexicalForm = $00000010;
  SDTPronunciation = $00000020;
  SDTAudio = $00000040;
  SDTAlternates = $00000080;
  SDTAll = $000000FF;

// Constants for enum SpeechBookmarkOptions
type
  SpeechBookmarkOptions = TOleEnum;
const
  SBONone = $00000000;
  SBOPause = $00000001;

// Constants for enum SpeechFormatType
type
  SpeechFormatType = TOleEnum;
const
  SFTInput = $00000000;
  SFTSREngine = $00000001;

// Constants for enum SpeechRecognitionType
type
  SpeechRecognitionType = TOleEnum;
const
  SRTStandard = $00000000;
  SRTAutopause = $00000001;
  SRTEmulated = $00000002;
  SRTSMLTimeout = $00000004;
  SRTExtendableParse = $00000008;
  SRTReSent = $00000010;

// Constants for enum SpeechLexiconType
type
  SpeechLexiconType = TOleEnum;
const
  SLTUser = $00000001;
  SLTApp = $00000002;

// Constants for enum SpeechWordType
type
  SpeechWordType = TOleEnum;
const
  SWTAdded = $00000001;
  SWTDeleted = $00000002;

// Constants for enum SpeechPartOfSpeech
type
  SpeechPartOfSpeech = TOleEnum;
const
  SPSNotOverriden = $FFFFFFFF;
  SPSUnknown = $00000000;
  SPSNoun = $00001000;
  SPSVerb = $00002000;
  SPSModifier = $00003000;
  SPSFunction = $00004000;
  SPSInterjection = $00005000;
  SPSLMA = $00007000;
  SPSSuppressWord = $0000F000;

// Constants for enum DISPID_SpeechDataKey
type
  DISPID_SpeechDataKey = TOleEnum;
const
  DISPID_SDKSetBinaryValue = $00000001;
  DISPID_SDKGetBinaryValue = $00000002;
  DISPID_SDKSetStringValue = $00000003;
  DISPID_SDKGetStringValue = $00000004;
  DISPID_SDKSetLongValue = $00000005;
  DISPID_SDKGetlongValue = $00000006;
  DISPID_SDKOpenKey = $00000007;
  DISPID_SDKCreateKey = $00000008;
  DISPID_SDKDeleteKey = $00000009;
  DISPID_SDKDeleteValue = $0000000A;
  DISPID_SDKEnumKeys = $0000000B;
  DISPID_SDKEnumValues = $0000000C;

// Constants for enum DISPID_SpeechObjectToken
type
  DISPID_SpeechObjectToken = TOleEnum;
const
  DISPID_SOTId = $00000001;
  DISPID_SOTDataKey = $00000002;
  DISPID_SOTCategory = $00000003;
  DISPID_SOTGetDescription = $00000004;
  DISPID_SOTSetId = $00000005;
  DISPID_SOTGetAttribute = $00000006;
  DISPID_SOTCreateInstance = $00000007;
  DISPID_SOTRemove = $00000008;
  DISPID_SOTGetStorageFileName = $00000009;
  DISPID_SOTRemoveStorageFileName = $0000000A;
  DISPID_SOTIsUISupported = $0000000B;
  DISPID_SOTDisplayUI = $0000000C;
  DISPID_SOTMatchesAttributes = $0000000D;

// Constants for enum DISPID_SpeechObjectTokens
type
  DISPID_SpeechObjectTokens = TOleEnum;
const
  DISPID_SOTsCount = $00000001;
  DISPID_SOTsItem = $00000000;
  DISPID_SOTs_NewEnum = $FFFFFFFC;

// Constants for enum DISPID_SpeechObjectTokenCategory
type
  DISPID_SpeechObjectTokenCategory = TOleEnum;
const
  DISPID_SOTCId = $00000001;
  DISPID_SOTCDefault = $00000002;
  DISPID_SOTCSetId = $00000003;
  DISPID_SOTCGetDataKey = $00000004;
  DISPID_SOTCEnumerateTokens = $00000005;

// Constants for enum DISPID_SpeechAudioFormat
type
  DISPID_SpeechAudioFormat = TOleEnum;
const
  DISPID_SAFType = $00000001;
  DISPID_SAFGuid = $00000002;
  DISPID_SAFGetWaveFormatEx = $00000003;
  DISPID_SAFSetWaveFormatEx = $00000004;

// Constants for enum DISPID_SpeechBaseStream
type
  DISPID_SpeechBaseStream = TOleEnum;
const
  DISPID_SBSFormat = $00000001;
  DISPID_SBSRead = $00000002;
  DISPID_SBSWrite = $00000003;
  DISPID_SBSSeek = $00000004;

// Constants for enum DISPID_SpeechAudio
type
  DISPID_SpeechAudio = TOleEnum;
const
  DISPID_SAStatus = $000000C8;
  DISPID_SABufferInfo = $000000C9;
  DISPID_SADefaultFormat = $000000CA;
  DISPID_SAVolume = $000000CB;
  DISPID_SABufferNotifySize = $000000CC;
  DISPID_SAEventHandle = $000000CD;
  DISPID_SASetState = $000000CE;

// Constants for enum DISPID_SpeechMMSysAudio
type
  DISPID_SpeechMMSysAudio = TOleEnum;
const
  DISPID_SMSADeviceId = $0000012C;
  DISPID_SMSALineId = $0000012D;
  DISPID_SMSAMMHandle = $0000012E;

// Constants for enum DISPID_SpeechFileStream
type
  DISPID_SpeechFileStream = TOleEnum;
const
  DISPID_SFSOpen = $00000064;
  DISPID_SFSClose = $00000065;

// Constants for enum DISPID_SpeechCustomStream
type
  DISPID_SpeechCustomStream = TOleEnum;
const
  DISPID_SCSBaseStream = $00000064;

// Constants for enum DISPID_SpeechMemoryStream
type
  DISPID_SpeechMemoryStream = TOleEnum;
const
  DISPID_SMSSetData = $00000064;
  DISPID_SMSGetData = $00000065;

// Constants for enum DISPID_SpeechAudioStatus
type
  DISPID_SpeechAudioStatus = TOleEnum;
const
  DISPID_SASFreeBufferSpace = $00000001;
  DISPID_SASNonBlockingIO = $00000002;
  DISPID_SASState = $00000003;
  DISPID_SASCurrentSeekPosition = $00000004;
  DISPID_SASCurrentDevicePosition = $00000005;

// Constants for enum DISPID_SpeechAudioBufferInfo
type
  DISPID_SpeechAudioBufferInfo = TOleEnum;
const
  DISPID_SABIMinNotification = $00000001;
  DISPID_SABIBufferSize = $00000002;
  DISPID_SABIEventBias = $00000003;

// Constants for enum DISPID_SpeechWaveFormatEx
type
  DISPID_SpeechWaveFormatEx = TOleEnum;
const
  DISPID_SWFEFormatTag = $00000001;
  DISPID_SWFEChannels = $00000002;
  DISPID_SWFESamplesPerSec = $00000003;
  DISPID_SWFEAvgBytesPerSec = $00000004;
  DISPID_SWFEBlockAlign = $00000005;
  DISPID_SWFEBitsPerSample = $00000006;
  DISPID_SWFEExtraData = $00000007;

// Constants for enum DISPID_SpeechVoice
type
  DISPID_SpeechVoice = TOleEnum;
const
  DISPID_SVStatus = $00000001;
  DISPID_SVVoice = $00000002;
  DISPID_SVAudioOutput = $00000003;
  DISPID_SVAudioOutputStream = $00000004;
  DISPID_SVRate = $00000005;
  DISPID_SVVolume = $00000006;
  DISPID_SVAllowAudioOuputFormatChangesOnNextSet = $00000007;
  DISPID_SVEventInterests = $00000008;
  DISPID_SVPriority = $00000009;
  DISPID_SVAlertBoundary = $0000000A;
  DISPID_SVSyncronousSpeakTimeout = $0000000B;
  DISPID_SVSpeak = $0000000C;
  DISPID_SVSpeakStream = $0000000D;
  DISPID_SVPause = $0000000E;
  DISPID_SVResume = $0000000F;
  DISPID_SVSkip = $00000010;
  DISPID_SVGetVoices = $00000011;
  DISPID_SVGetAudioOutputs = $00000012;
  DISPID_SVWaitUntilDone = $00000013;
  DISPID_SVSpeakCompleteEvent = $00000014;
  DISPID_SVIsUISupported = $00000015;
  DISPID_SVDisplayUI = $00000016;

// Constants for enum DISPID_SpeechVoiceStatus
type
  DISPID_SpeechVoiceStatus = TOleEnum;
const
  DISPID_SVSCurrentStreamNumber = $00000001;
  DISPID_SVSLastStreamNumberQueued = $00000002;
  DISPID_SVSLastResult = $00000003;
  DISPID_SVSRunningState = $00000004;
  DISPID_SVSInputWordPosition = $00000005;
  DISPID_SVSInputWordLength = $00000006;
  DISPID_SVSInputSentencePosition = $00000007;
  DISPID_SVSInputSentenceLength = $00000008;
  DISPID_SVSLastBookmark = $00000009;
  DISPID_SVSLastBookmarkId = $0000000A;
  DISPID_SVSPhonemeId = $0000000B;
  DISPID_SVSVisemeId = $0000000C;

// Constants for enum DISPID_SpeechVoiceEvent
type
  DISPID_SpeechVoiceEvent = TOleEnum;
const
  DISPID_SVEStreamStart = $00000001;
  DISPID_SVEStreamEnd = $00000002;
  DISPID_SVEVoiceChange = $00000003;
  DISPID_SVEBookmark = $00000004;
  DISPID_SVEWord = $00000005;
  DISPID_SVEPhoneme = $00000006;
  DISPID_SVESentenceBoundary = $00000007;
  DISPID_SVEViseme = $00000008;
  DISPID_SVEAudioLevel = $00000009;
  DISPID_SVEEnginePrivate = $0000000A;

// Constants for enum DISPID_SpeechRecognizer
type
  DISPID_SpeechRecognizer = TOleEnum;
const
  DISPID_SRRecognizer = $00000001;
  DISPID_SRAllowAudioInputFormatChangesOnNextSet = $00000002;
  DISPID_SRAudioInput = $00000003;
  DISPID_SRAudioInputStream = $00000004;
  DISPID_SRIsShared = $00000005;
  DISPID_SRState = $00000006;
  DISPID_SRStatus = $00000007;
  DISPID_SRProfile = $00000008;
  DISPID_SREmulateRecognition = $00000009;
  DISPID_SRCreateRecoContext = $0000000A;
  DISPID_SRGetFormat = $0000000B;
  DISPID_SRSetPropertyNumber = $0000000C;
  DISPID_SRGetPropertyNumber = $0000000D;
  DISPID_SRSetPropertyString = $0000000E;
  DISPID_SRGetPropertyString = $0000000F;
  DISPID_SRIsUISupported = $00000010;
  DISPID_SRDisplayUI = $00000011;
  DISPID_SRGetRecognizers = $00000012;
  DISPID_SVGetAudioInputs = $00000013;
  DISPID_SVGetProfiles = $00000014;

// Constants for enum SpeechEmulationCompareFlags
type
  SpeechEmulationCompareFlags = TOleEnum;
const
  SECFIgnoreCase = $00000001;
  SECFIgnoreKanaType = $00010000;
  SECFIgnoreWidth = $00020000;
  SECFNoSpecialChars = $20000000;
  SECFEmulateResult = $40000000;
  SECFDefault = $00030001;

// Constants for enum DISPID_SpeechRecognizerStatus
type
  DISPID_SpeechRecognizerStatus = TOleEnum;
const
  DISPID_SRSAudioStatus = $00000001;
  DISPID_SRSCurrentStreamPosition = $00000002;
  DISPID_SRSCurrentStreamNumber = $00000003;
  DISPID_SRSNumberOfActiveRules = $00000004;
  DISPID_SRSClsidEngine = $00000005;
  DISPID_SRSSupportedLanguages = $00000006;

// Constants for enum DISPID_SpeechRecoContext
type
  DISPID_SpeechRecoContext = TOleEnum;
const
  DISPID_SRCRecognizer = $00000001;
  DISPID_SRCAudioInInterferenceStatus = $00000002;
  DISPID_SRCRequestedUIType = $00000003;
  DISPID_SRCVoice = $00000004;
  DISPID_SRAllowVoiceFormatMatchingOnNextSet = $00000005;
  DISPID_SRCVoicePurgeEvent = $00000006;
  DISPID_SRCEventInterests = $00000007;
  DISPID_SRCCmdMaxAlternates = $00000008;
  DISPID_SRCState = $00000009;
  DISPID_SRCRetainedAudio = $0000000A;
  DISPID_SRCRetainedAudioFormat = $0000000B;
  DISPID_SRCPause = $0000000C;
  DISPID_SRCResume = $0000000D;
  DISPID_SRCCreateGrammar = $0000000E;
  DISPID_SRCCreateResultFromMemory = $0000000F;
  DISPID_SRCBookmark = $00000010;
  DISPID_SRCSetAdaptationData = $00000011;

// Constants for enum DISPIDSPRG
type
  DISPIDSPRG = TOleEnum;
const
  DISPID_SRGId = $00000001;
  DISPID_SRGRecoContext = $00000002;
  DISPID_SRGState = $00000003;
  DISPID_SRGRules = $00000004;
  DISPID_SRGReset = $00000005;
  DISPID_SRGCommit = $00000006;
  DISPID_SRGCmdLoadFromFile = $00000007;
  DISPID_SRGCmdLoadFromObject = $00000008;
  DISPID_SRGCmdLoadFromResource = $00000009;
  DISPID_SRGCmdLoadFromMemory = $0000000A;
  DISPID_SRGCmdLoadFromProprietaryGrammar = $0000000B;
  DISPID_SRGCmdSetRuleState = $0000000C;
  DISPID_SRGCmdSetRuleIdState = $0000000D;
  DISPID_SRGDictationLoad = $0000000E;
  DISPID_SRGDictationUnload = $0000000F;
  DISPID_SRGDictationSetState = $00000010;
  DISPID_SRGSetWordSequenceData = $00000011;
  DISPID_SRGSetTextSelection = $00000012;
  DISPID_SRGIsPronounceable = $00000013;

// Constants for enum DISPID_SpeechRecoContextEvents
type
  DISPID_SpeechRecoContextEvents = TOleEnum;
const
  DISPID_SRCEStartStream = $00000001;
  DISPID_SRCEEndStream = $00000002;
  DISPID_SRCEBookmark = $00000003;
  DISPID_SRCESoundStart = $00000004;
  DISPID_SRCESoundEnd = $00000005;
  DISPID_SRCEPhraseStart = $00000006;
  DISPID_SRCERecognition = $00000007;
  DISPID_SRCEHypothesis = $00000008;
  DISPID_SRCEPropertyNumberChange = $00000009;
  DISPID_SRCEPropertyStringChange = $0000000A;
  DISPID_SRCEFalseRecognition = $0000000B;
  DISPID_SRCEInterference = $0000000C;
  DISPID_SRCERequestUI = $0000000D;
  DISPID_SRCERecognizerStateChange = $0000000E;
  DISPID_SRCEAdaptation = $0000000F;
  DISPID_SRCERecognitionForOtherContext = $00000010;
  DISPID_SRCEAudioLevel = $00000011;
  DISPID_SRCEEnginePrivate = $00000012;

// Constants for enum DISPID_SpeechGrammarRule
type
  DISPID_SpeechGrammarRule = TOleEnum;
const
  DISPID_SGRAttributes = $00000001;
  DISPID_SGRInitialState = $00000002;
  DISPID_SGRName = $00000003;
  DISPID_SGRId = $00000004;
  DISPID_SGRClear = $00000005;
  DISPID_SGRAddResource = $00000006;
  DISPID_SGRAddState = $00000007;

// Constants for enum DISPID_SpeechGrammarRules
type
  DISPID_SpeechGrammarRules = TOleEnum;
const
  DISPID_SGRsCount = $00000001;
  DISPID_SGRsDynamic = $00000002;
  DISPID_SGRsAdd = $00000003;
  DISPID_SGRsCommit = $00000004;
  DISPID_SGRsCommitAndSave = $00000005;
  DISPID_SGRsFindRule = $00000006;
  DISPID_SGRsItem = $00000000;
  DISPID_SGRs_NewEnum = $FFFFFFFC;

// Constants for enum DISPID_SpeechGrammarRuleState
type
  DISPID_SpeechGrammarRuleState = TOleEnum;
const
  DISPID_SGRSRule = $00000001;
  DISPID_SGRSTransitions = $00000002;
  DISPID_SGRSAddWordTransition = $00000003;
  DISPID_SGRSAddRuleTransition = $00000004;
  DISPID_SGRSAddSpecialTransition = $00000005;

// Constants for enum DISPID_SpeechGrammarRuleStateTransitions
type
  DISPID_SpeechGrammarRuleStateTransitions = TOleEnum;
const
  DISPID_SGRSTsCount = $00000001;
  DISPID_SGRSTsItem = $00000000;
  DISPID_SGRSTs_NewEnum = $FFFFFFFC;

// Constants for enum DISPID_SpeechGrammarRuleStateTransition
type
  DISPID_SpeechGrammarRuleStateTransition = TOleEnum;
const
  DISPID_SGRSTType = $00000001;
  DISPID_SGRSTText = $00000002;
  DISPID_SGRSTRule = $00000003;
  DISPID_SGRSTWeight = $00000004;
  DISPID_SGRSTPropertyName = $00000005;
  DISPID_SGRSTPropertyId = $00000006;
  DISPID_SGRSTPropertyValue = $00000007;
  DISPID_SGRSTNextState = $00000008;

// Constants for enum DISPIDSPTSI
type
  DISPIDSPTSI = TOleEnum;
const
  DISPIDSPTSI_ActiveOffset = $00000001;
  DISPIDSPTSI_ActiveLength = $00000002;
  DISPIDSPTSI_SelectionOffset = $00000003;
  DISPIDSPTSI_SelectionLength = $00000004;

// Constants for enum DISPID_SpeechRecoResult
type
  DISPID_SpeechRecoResult = TOleEnum;
const
  DISPID_SRRRecoContext = $00000001;
  DISPID_SRRTimes = $00000002;
  DISPID_SRRAudioFormat = $00000003;
  DISPID_SRRPhraseInfo = $00000004;
  DISPID_SRRAlternates = $00000005;
  DISPID_SRRAudio = $00000006;
  DISPID_SRRSpeakAudio = $00000007;
  DISPID_SRRSaveToMemory = $00000008;
  DISPID_SRRDiscardResultInfo = $00000009;

// Constants for enum DISPID_SpeechXMLRecoResult
type
  DISPID_SpeechXMLRecoResult = TOleEnum;
const
  DISPID_SRRGetXMLResult = $0000000A;
  DISPID_SRRGetXMLErrorInfo = $0000000B;

// Constants for enum SPXMLRESULTOPTIONS
type
  SPXMLRESULTOPTIONS = TOleEnum;
const
  SPXRO_SML = $00000000;
  SPXRO_Alternates_SML = $00000001;

// Constants for enum DISPID_SpeechRecoResult2
type
  DISPID_SpeechRecoResult2 = TOleEnum;
const
  DISPID_SRRSetTextFeedback = $0000000C;

// Constants for enum DISPID_SpeechPhraseBuilder
type
  DISPID_SpeechPhraseBuilder = TOleEnum;
const
  DISPID_SPPBRestorePhraseFromMemory = $00000001;

// Constants for enum DISPID_SpeechRecoResultTimes
type
  DISPID_SpeechRecoResultTimes = TOleEnum;
const
  DISPID_SRRTStreamTime = $00000001;
  DISPID_SRRTLength = $00000002;
  DISPID_SRRTTickCount = $00000003;
  DISPID_SRRTOffsetFromStart = $00000004;

// Constants for enum DISPID_SpeechPhraseAlternate
type
  DISPID_SpeechPhraseAlternate = TOleEnum;
const
  DISPID_SPARecoResult = $00000001;
  DISPID_SPAStartElementInResult = $00000002;
  DISPID_SPANumberOfElementsInResult = $00000003;
  DISPID_SPAPhraseInfo = $00000004;
  DISPID_SPACommit = $00000005;

// Constants for enum DISPID_SpeechPhraseAlternates
type
  DISPID_SpeechPhraseAlternates = TOleEnum;
const
  DISPID_SPAsCount = $00000001;
  DISPID_SPAsItem = $00000000;
  DISPID_SPAs_NewEnum = $FFFFFFFC;

// Constants for enum DISPID_SpeechPhraseInfo
type
  DISPID_SpeechPhraseInfo = TOleEnum;
const
  DISPID_SPILanguageId = $00000001;
  DISPID_SPIGrammarId = $00000002;
  DISPID_SPIStartTime = $00000003;
  DISPID_SPIAudioStreamPosition = $00000004;
  DISPID_SPIAudioSizeBytes = $00000005;
  DISPID_SPIRetainedSizeBytes = $00000006;
  DISPID_SPIAudioSizeTime = $00000007;
  DISPID_SPIRule = $00000008;
  DISPID_SPIProperties = $00000009;
  DISPID_SPIElements = $0000000A;
  DISPID_SPIReplacements = $0000000B;
  DISPID_SPIEngineId = $0000000C;
  DISPID_SPIEnginePrivateData = $0000000D;
  DISPID_SPISaveToMemory = $0000000E;
  DISPID_SPIGetText = $0000000F;
  DISPID_SPIGetDisplayAttributes = $00000010;

// Constants for enum DISPID_SpeechPhraseElement
type
  DISPID_SpeechPhraseElement = TOleEnum;
const
  DISPID_SPEAudioTimeOffset = $00000001;
  DISPID_SPEAudioSizeTime = $00000002;
  DISPID_SPEAudioStreamOffset = $00000003;
  DISPID_SPEAudioSizeBytes = $00000004;
  DISPID_SPERetainedStreamOffset = $00000005;
  DISPID_SPERetainedSizeBytes = $00000006;
  DISPID_SPEDisplayText = $00000007;
  DISPID_SPELexicalForm = $00000008;
  DISPID_SPEPronunciation = $00000009;
  DISPID_SPEDisplayAttributes = $0000000A;
  DISPID_SPERequiredConfidence = $0000000B;
  DISPID_SPEActualConfidence = $0000000C;
  DISPID_SPEEngineConfidence = $0000000D;

// Constants for enum DISPID_SpeechPhraseElements
type
  DISPID_SpeechPhraseElements = TOleEnum;
const
  DISPID_SPEsCount = $00000001;
  DISPID_SPEsItem = $00000000;
  DISPID_SPEs_NewEnum = $FFFFFFFC;

// Constants for enum DISPID_SpeechPhraseReplacement
type
  DISPID_SpeechPhraseReplacement = TOleEnum;
const
  DISPID_SPRDisplayAttributes = $00000001;
  DISPID_SPRText = $00000002;
  DISPID_SPRFirstElement = $00000003;
  DISPID_SPRNumberOfElements = $00000004;

// Constants for enum DISPID_SpeechPhraseReplacements
type
  DISPID_SpeechPhraseReplacements = TOleEnum;
const
  DISPID_SPRsCount = $00000001;
  DISPID_SPRsItem = $00000000;
  DISPID_SPRs_NewEnum = $FFFFFFFC;

// Constants for enum DISPID_SpeechPhraseProperty
type
  DISPID_SpeechPhraseProperty = TOleEnum;
const
  DISPID_SPPName = $00000001;
  DISPID_SPPId = $00000002;
  DISPID_SPPValue = $00000003;
  DISPID_SPPFirstElement = $00000004;
  DISPID_SPPNumberOfElements = $00000005;
  DISPID_SPPEngineConfidence = $00000006;
  DISPID_SPPConfidence = $00000007;
  DISPID_SPPParent = $00000008;
  DISPID_SPPChildren = $00000009;

// Constants for enum DISPID_SpeechPhraseProperties
type
  DISPID_SpeechPhraseProperties = TOleEnum;
const
  DISPID_SPPsCount = $00000001;
  DISPID_SPPsItem = $00000000;
  DISPID_SPPs_NewEnum = $FFFFFFFC;

// Constants for enum DISPID_SpeechPhraseRule
type
  DISPID_SpeechPhraseRule = TOleEnum;
const
  DISPID_SPRuleName = $00000001;
  DISPID_SPRuleId = $00000002;
  DISPID_SPRuleFirstElement = $00000003;
  DISPID_SPRuleNumberOfElements = $00000004;
  DISPID_SPRuleParent = $00000005;
  DISPID_SPRuleChildren = $00000006;
  DISPID_SPRuleConfidence = $00000007;
  DISPID_SPRuleEngineConfidence = $00000008;

// Constants for enum DISPID_SpeechPhraseRules
type
  DISPID_SpeechPhraseRules = TOleEnum;
const
  DISPID_SPRulesCount = $00000001;
  DISPID_SPRulesItem = $00000000;
  DISPID_SPRules_NewEnum = $FFFFFFFC;

// Constants for enum DISPID_SpeechLexicon
type
  DISPID_SpeechLexicon = TOleEnum;
const
  DISPID_SLGenerationId = $00000001;
  DISPID_SLGetWords = $00000002;
  DISPID_SLAddPronunciation = $00000003;
  DISPID_SLAddPronunciationByPhoneIds = $00000004;
  DISPID_SLRemovePronunciation = $00000005;
  DISPID_SLRemovePronunciationByPhoneIds = $00000006;
  DISPID_SLGetPronunciations = $00000007;
  DISPID_SLGetGenerationChange = $00000008;

// Constants for enum DISPID_SpeechLexiconWords
type
  DISPID_SpeechLexiconWords = TOleEnum;
const
  DISPID_SLWsCount = $00000001;
  DISPID_SLWsItem = $00000000;
  DISPID_SLWs_NewEnum = $FFFFFFFC;

// Constants for enum DISPID_SpeechLexiconWord
type
  DISPID_SpeechLexiconWord = TOleEnum;
const
  DISPID_SLWLangId = $00000001;
  DISPID_SLWType = $00000002;
  DISPID_SLWWord = $00000003;
  DISPID_SLWPronunciations = $00000004;

// Constants for enum DISPID_SpeechLexiconProns
type
  DISPID_SpeechLexiconProns = TOleEnum;
const
  DISPID_SLPsCount = $00000001;
  DISPID_SLPsItem = $00000000;
  DISPID_SLPs_NewEnum = $FFFFFFFC;

// Constants for enum DISPID_SpeechLexiconPronunciation
type
  DISPID_SpeechLexiconPronunciation = TOleEnum;
const
  DISPID_SLPType = $00000001;
  DISPID_SLPLangId = $00000002;
  DISPID_SLPPartOfSpeech = $00000003;
  DISPID_SLPPhoneIds = $00000004;
  DISPID_SLPSymbolic = $00000005;

// Constants for enum DISPID_SpeechPhoneConverter
type
  DISPID_SpeechPhoneConverter = TOleEnum;
const
  DISPID_SPCLangId = $00000001;
  DISPID_SPCPhoneToId = $00000002;
  DISPID_SPCIdToPhone = $00000003;

// Constants for enum SPDATAKEYLOCATION
type
  SPDATAKEYLOCATION = TOleEnum;
const
  SPDKL_DefaultLocation = $00000000;
  SPDKL_CurrentUser = $00000001;
  SPDKL_LocalMachine = $00000002;
  SPDKL_CurrentConfig = $00000005;

// Constants for enum _SPAUDIOSTATE
type
  _SPAUDIOSTATE = TOleEnum;
const
  SPAS_CLOSED = $00000000;
  SPAS_STOP = $00000001;
  SPAS_PAUSE = $00000002;
  SPAS_RUN = $00000003;

// Constants for enum SPFILEMODE
type
  SPFILEMODE = TOleEnum;
const
  SPFM_OPEN_READONLY = $00000000;
  SPFM_OPEN_READWRITE = $00000001;
  SPFM_CREATE = $00000002;
  SPFM_CREATE_ALWAYS = $00000003;
  SPFM_NUM_MODES = $00000004;

// Constants for enum SPVISEMES
type
  SPVISEMES = TOleEnum;
const
  SP_VISEME_0 = $00000000;
  SP_VISEME_1 = $00000001;
  SP_VISEME_2 = $00000002;
  SP_VISEME_3 = $00000003;
  SP_VISEME_4 = $00000004;
  SP_VISEME_5 = $00000005;
  SP_VISEME_6 = $00000006;
  SP_VISEME_7 = $00000007;
  SP_VISEME_8 = $00000008;
  SP_VISEME_9 = $00000009;
  SP_VISEME_10 = $0000000A;
  SP_VISEME_11 = $0000000B;
  SP_VISEME_12 = $0000000C;
  SP_VISEME_13 = $0000000D;
  SP_VISEME_14 = $0000000E;
  SP_VISEME_15 = $0000000F;
  SP_VISEME_16 = $00000010;
  SP_VISEME_17 = $00000011;
  SP_VISEME_18 = $00000012;
  SP_VISEME_19 = $00000013;
  SP_VISEME_20 = $00000014;
  SP_VISEME_21 = $00000015;

// Constants for enum SPVPRIORITY
type
  SPVPRIORITY = TOleEnum;
const
  SPVPRI_NORMAL = $00000000;
  SPVPRI_ALERT = $00000001;
  SPVPRI_OVER = $00000002;

// Constants for enum SPEVENTENUM
type
  SPEVENTENUM = TOleEnum;
const
  SPEI_UNDEFINED = $00000000;
  SPEI_START_INPUT_STREAM = $00000001;
  SPEI_END_INPUT_STREAM = $00000002;
  SPEI_VOICE_CHANGE = $00000003;
  SPEI_TTS_BOOKMARK = $00000004;
  SPEI_WORD_BOUNDARY = $00000005;
  SPEI_PHONEME = $00000006;
  SPEI_SENTENCE_BOUNDARY = $00000007;
  SPEI_VISEME = $00000008;
  SPEI_TTS_AUDIO_LEVEL = $00000009;
  SPEI_TTS_PRIVATE = $0000000F;
  SPEI_MIN_TTS = $00000001;
  SPEI_MAX_TTS = $0000000F;
  SPEI_END_SR_STREAM = $00000022;
  SPEI_SOUND_START = $00000023;
  SPEI_SOUND_END = $00000024;
  SPEI_PHRASE_START = $00000025;
  SPEI_RECOGNITION = $00000026;
  SPEI_HYPOTHESIS = $00000027;
  SPEI_SR_BOOKMARK = $00000028;
  SPEI_PROPERTY_NUM_CHANGE = $00000029;
  SPEI_PROPERTY_STRING_CHANGE = $0000002A;
  SPEI_FALSE_RECOGNITION = $0000002B;
  SPEI_INTERFERENCE = $0000002C;
  SPEI_REQUEST_UI = $0000002D;
  SPEI_RECO_STATE_CHANGE = $0000002E;
  SPEI_ADAPTATION = $0000002F;
  SPEI_START_SR_STREAM = $00000030;
  SPEI_RECO_OTHER_CONTEXT = $00000031;
  SPEI_SR_AUDIO_LEVEL = $00000032;
  SPEI_SR_RETAINEDAUDIO = $00000033;
  SPEI_SR_PRIVATE = $00000034;
  SPEI_ACTIVE_CATEGORY_CHANGED = $00000035;
  SPEI_RESERVED5 = $00000036;
  SPEI_RESERVED6 = $00000037;
  SPEI_MIN_SR = $00000022;
  SPEI_MAX_SR = $00000037;
  SPEI_RESERVED1 = $0000001E;
  SPEI_RESERVED2 = $00000021;
  SPEI_RESERVED3 = $0000003F;

// Constants for enum SPRECOSTATE
type
  SPRECOSTATE = TOleEnum;
const
  SPRST_INACTIVE = $00000000;
  SPRST_ACTIVE = $00000001;
  SPRST_ACTIVE_ALWAYS = $00000002;
  SPRST_INACTIVE_WITH_PURGE = $00000003;
  SPRST_NUM_STATES = $00000004;

// Constants for enum SPWAVEFORMATTYPE
type
  SPWAVEFORMATTYPE = TOleEnum;
const
  SPWF_INPUT = $00000000;
  SPWF_SRENGINE = $00000001;

// Constants for enum SPSEMANTICFORMAT
type
  SPSEMANTICFORMAT = TOleEnum;
const
  SPSMF_SAPI_PROPERTIES = $00000000;
  SPSMF_SRGS_SEMANTICINTERPRETATION_MS = $00000001;
  SPSMF_SRGS_SAPIPROPERTIES = $00000002;
  SPSMF_UPS = $00000004;
  SPSMF_SRGS_SEMANTICINTERPRETATION_W3C = $00000008;

// Constants for enum SPGRAMMARWORDTYPE
type
  SPGRAMMARWORDTYPE = TOleEnum;
const
  SPWT_DISPLAY = $00000000;
  SPWT_LEXICAL = $00000001;
  SPWT_PRONUNCIATION = $00000002;
  SPWT_LEXICAL_NO_SPECIAL_CHARS = $00000003;

// Constants for enum SPLOADOPTIONS
type
  SPLOADOPTIONS = TOleEnum;
const
  SPLO_STATIC = $00000000;
  SPLO_DYNAMIC = $00000001;

// Constants for enum SPRULESTATE
type
  SPRULESTATE = TOleEnum;
const
  SPRS_INACTIVE = $00000000;
  SPRS_ACTIVE = $00000001;
  SPRS_ACTIVE_WITH_AUTO_PAUSE = $00000003;
  SPRS_ACTIVE_USER_DELIMITED = $00000004;

// Constants for enum SPWORDPRONOUNCEABLE
type
  SPWORDPRONOUNCEABLE = TOleEnum;
const
  SPWP_UNKNOWN_WORD_UNPRONOUNCEABLE = $00000000;
  SPWP_UNKNOWN_WORD_PRONOUNCEABLE = $00000001;
  SPWP_KNOWN_WORD_PRONOUNCEABLE = $00000002;

// Constants for enum SPGRAMMARSTATE
type
  SPGRAMMARSTATE = TOleEnum;
const
  SPGS_DISABLED = $00000000;
  SPGS_ENABLED = $00000001;
  SPGS_EXCLUSIVE = $00000003;

// Constants for enum SPINTERFERENCE
type
  SPINTERFERENCE = TOleEnum;
const
  SPINTERFERENCE_NONE = $00000000;
  SPINTERFERENCE_NOISE = $00000001;
  SPINTERFERENCE_NOSIGNAL = $00000002;
  SPINTERFERENCE_TOOLOUD = $00000003;
  SPINTERFERENCE_TOOQUIET = $00000004;
  SPINTERFERENCE_TOOFAST = $00000005;
  SPINTERFERENCE_TOOSLOW = $00000006;
  SPINTERFERENCE_LATENCY_WARNING = $00000007;
  SPINTERFERENCE_LATENCY_TRUNCATE_BEGIN = $00000008;
  SPINTERFERENCE_LATENCY_TRUNCATE_END = $00000009;

// Constants for enum SPAUDIOOPTIONS
type
  SPAUDIOOPTIONS = TOleEnum;
const
  SPAO_NONE = $00000000;
  SPAO_RETAIN_AUDIO = $00000001;

// Constants for enum SPBOOKMARKOPTIONS
type
  SPBOOKMARKOPTIONS = TOleEnum;
const
  SPBO_NONE = $00000000;
  SPBO_PAUSE = $00000001;
  SPBO_AHEAD = $00000002;
  SPBO_TIME_UNITS = $00000004;

// Constants for enum SPCONTEXTSTATE
type
  SPCONTEXTSTATE = TOleEnum;
const
  SPCS_DISABLED = $00000000;
  SPCS_ENABLED = $00000001;

// Constants for enum SPADAPTATIONRELEVANCE
type
  SPADAPTATIONRELEVANCE = TOleEnum;
const
  SPAR_Unknown = $00000000;
  SPAR_Low = $00000001;
  SPAR_Medium = $00000002;
  SPAR_High = $00000003;

// Constants for enum SPCATEGORYTYPE
type
  SPCATEGORYTYPE = TOleEnum;
const
  SPCT_COMMAND = $00000000;
  SPCT_DICTATION = $00000001;
  SPCT_SLEEP = $00000002;
  SPCT_SUB_COMMAND = $00000003;
  SPCT_SUB_DICTATION = $00000004;

// Constants for enum SPLEXICONTYPE
type
  SPLEXICONTYPE = TOleEnum;
const
  eLEXTYPE_USER = $00000001;
  eLEXTYPE_APP = $00000002;
  eLEXTYPE_VENDORLEXICON = $00000004;
  eLEXTYPE_LETTERTOSOUND = $00000008;
  eLEXTYPE_MORPHOLOGY = $00000010;
  eLEXTYPE_RESERVED4 = $00000020;
  eLEXTYPE_USER_SHORTCUT = $00000040;
  eLEXTYPE_RESERVED6 = $00000080;
  eLEXTYPE_RESERVED7 = $00000100;
  eLEXTYPE_RESERVED8 = $00000200;
  eLEXTYPE_RESERVED9 = $00000400;
  eLEXTYPE_RESERVED10 = $00000800;
  eLEXTYPE_PRIVATE1 = $00001000;
  eLEXTYPE_PRIVATE2 = $00002000;
  eLEXTYPE_PRIVATE3 = $00004000;
  eLEXTYPE_PRIVATE4 = $00008000;
  eLEXTYPE_PRIVATE5 = $00010000;
  eLEXTYPE_PRIVATE6 = $00020000;
  eLEXTYPE_PRIVATE7 = $00040000;
  eLEXTYPE_PRIVATE8 = $00080000;
  eLEXTYPE_PRIVATE9 = $00100000;
  eLEXTYPE_PRIVATE10 = $00200000;
  eLEXTYPE_PRIVATE11 = $00400000;
  eLEXTYPE_PRIVATE12 = $00800000;
  eLEXTYPE_PRIVATE13 = $01000000;
  eLEXTYPE_PRIVATE14 = $02000000;
  eLEXTYPE_PRIVATE15 = $04000000;
  eLEXTYPE_PRIVATE16 = $08000000;
  eLEXTYPE_PRIVATE17 = $10000000;
  eLEXTYPE_PRIVATE18 = $20000000;
  eLEXTYPE_PRIVATE19 = $40000000;
  eLEXTYPE_PRIVATE20 = $80000000;

// Constants for enum SPPARTOFSPEECH
type
  SPPARTOFSPEECH = TOleEnum;
const
  SPPS_NotOverriden = $FFFFFFFF;
  SPPS_Unknown = $00000000;
  SPPS_Noun = $00001000;
  SPPS_Verb = $00002000;
  SPPS_Modifier = $00003000;
  SPPS_Function = $00004000;
  SPPS_Interjection = $00005000;
  SPPS_Noncontent = $00006000;
  SPPS_LMA = $00007000;
  SPPS_SuppressWord = $0000F000;

// Constants for enum SPWORDTYPE
type
  SPWORDTYPE = TOleEnum;
const
  eWORDTYPE_ADDED = $00000001;
  eWORDTYPE_DELETED = $00000002;

// Constants for enum SPSHORTCUTTYPE
type
  SPSHORTCUTTYPE = TOleEnum;
const
  SPSHT_NotOverriden = $FFFFFFFF;
  SPSHT_Unknown = $00000000;
  SPSHT_EMAIL = $00001000;
  SPSHT_OTHER = $00002000;
  SPPS_RESERVED1 = $00003000;
  SPPS_RESERVED2 = $00004000;
  SPPS_RESERVED3 = $00005000;
  SPPS_RESERVED4 = $0000F000;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ISpeechDataKey = interface;
  ISpeechDataKeyDisp = dispinterface;
  ISpeechObjectToken = interface;
  ISpeechObjectTokenDisp = dispinterface;
  ISpeechObjectTokenCategory = interface;
  ISpeechObjectTokenCategoryDisp = dispinterface;
  ISpeechObjectTokens = interface;
  ISpeechObjectTokensDisp = dispinterface;
  ISpeechAudioBufferInfo = interface;
  ISpeechAudioBufferInfoDisp = dispinterface;
  ISpeechAudioStatus = interface;
  ISpeechAudioStatusDisp = dispinterface;
  ISpeechAudioFormat = interface;
  ISpeechAudioFormatDisp = dispinterface;
  ISpeechWaveFormatEx = interface;
  ISpeechWaveFormatExDisp = dispinterface;
  ISpeechBaseStream = interface;
  ISpeechBaseStreamDisp = dispinterface;
  ISpeechFileStream = interface;
  ISpeechFileStreamDisp = dispinterface;
  ISpeechMemoryStream = interface;
  ISpeechMemoryStreamDisp = dispinterface;
  ISpeechCustomStream = interface;
  ISpeechCustomStreamDisp = dispinterface;
  ISpeechAudio = interface;
  ISpeechAudioDisp = dispinterface;
  ISpeechMMSysAudio = interface;
  ISpeechMMSysAudioDisp = dispinterface;
  ISpeechVoice = interface;
  ISpeechVoiceDisp = dispinterface;
  ISpeechVoiceStatus = interface;
  ISpeechVoiceStatusDisp = dispinterface;
  _ISpeechVoiceEvents = dispinterface;
  ISpeechRecognizer = interface;
  ISpeechRecognizerDisp = dispinterface;
  ISpeechRecognizerStatus = interface;
  ISpeechRecognizerStatusDisp = dispinterface;
  ISpeechRecoContext = interface;
  ISpeechRecoContextDisp = dispinterface;
  ISpeechRecoGrammar = interface;
  ISpeechRecoGrammarDisp = dispinterface;
  ISpeechGrammarRules = interface;
  ISpeechGrammarRulesDisp = dispinterface;
  ISpeechGrammarRule = interface;
  ISpeechGrammarRuleDisp = dispinterface;
  ISpeechGrammarRuleState = interface;
  ISpeechGrammarRuleStateDisp = dispinterface;
  ISpeechGrammarRuleStateTransitions = interface;
  ISpeechGrammarRuleStateTransitionsDisp = dispinterface;
  ISpeechGrammarRuleStateTransition = interface;
  ISpeechGrammarRuleStateTransitionDisp = dispinterface;
  ISpeechTextSelectionInformation = interface;
  ISpeechTextSelectionInformationDisp = dispinterface;
  ISpeechRecoResult = interface;
  ISpeechRecoResultDisp = dispinterface;
  ISpeechRecoResultTimes = interface;
  ISpeechRecoResultTimesDisp = dispinterface;
  ISpeechPhraseInfo = interface;
  ISpeechPhraseInfoDisp = dispinterface;
  ISpeechPhraseRule = interface;
  ISpeechPhraseRuleDisp = dispinterface;
  ISpeechPhraseRules = interface;
  ISpeechPhraseRulesDisp = dispinterface;
  ISpeechPhraseProperties = interface;
  ISpeechPhrasePropertiesDisp = dispinterface;
  ISpeechPhraseProperty = interface;
  ISpeechPhrasePropertyDisp = dispinterface;
  ISpeechPhraseElements = interface;
  ISpeechPhraseElementsDisp = dispinterface;
  ISpeechPhraseElement = interface;
  ISpeechPhraseElementDisp = dispinterface;
  ISpeechPhraseReplacements = interface;
  ISpeechPhraseReplacementsDisp = dispinterface;
  ISpeechPhraseReplacement = interface;
  ISpeechPhraseReplacementDisp = dispinterface;
  ISpeechPhraseAlternates = interface;
  ISpeechPhraseAlternatesDisp = dispinterface;
  ISpeechPhraseAlternate = interface;
  ISpeechPhraseAlternateDisp = dispinterface;
  _ISpeechRecoContextEvents = dispinterface;
  ISpeechRecoResult2 = interface;
  ISpeechRecoResult2Disp = dispinterface;
  ISpeechLexicon = interface;
  ISpeechLexiconDisp = dispinterface;
  ISpeechLexiconWords = interface;
  ISpeechLexiconWordsDisp = dispinterface;
  ISpeechLexiconWord = interface;
  ISpeechLexiconWordDisp = dispinterface;
  ISpeechLexiconPronunciations = interface;
  ISpeechLexiconPronunciationsDisp = dispinterface;
  ISpeechLexiconPronunciation = interface;
  ISpeechLexiconPronunciationDisp = dispinterface;
  ISpeechXMLRecoResult = interface;
  ISpeechXMLRecoResultDisp = dispinterface;
  ISpeechRecoResultDispatch = interface;
  ISpeechRecoResultDispatchDisp = dispinterface;
  ISpeechPhraseInfoBuilder = interface;
  ISpeechPhraseInfoBuilderDisp = dispinterface;
  ISpeechPhoneConverter = interface;
  ISpeechPhoneConverterDisp = dispinterface;
  ISpNotifySink = interface;
  ISpNotifyTranslator = interface;
  ISpDataKey = interface;
  ISpObjectTokenCategory = interface;
  IEnumSpObjectTokens = interface;
  ISpObjectToken = interface;
  IServiceProvider = interface;
  ISpResourceManager = interface;
  ISpStreamFormat = interface;
  ISpStreamFormatConverter = interface;
  ISpNotifySource = interface;
  ISpEventSource = interface;
  ISpEventSink = interface;
  ISpObjectWithToken = interface;
  ISpAudio = interface;
  ISpMMSysAudio = interface;
  ISpStream = interface;
  ISpVoice = interface;
  ISpPhoneticAlphabetSelection = interface;
  ISpRecoContext = interface;
  ISpRecoContext2 = interface;
  ISpProperties = interface;
  ISpRecognizer = interface;
  ISpPhrase = interface;
  ISpGrammarBuilder = interface;
  ISpRecoGrammar = interface;
  ISpRecoResult = interface;
  ISpPhraseAlt = interface;
  ISpRecognizer2 = interface;
  ISpRecognizer3 = interface;
  ISpSerializeState = interface;
  ISpRecoCategory = interface;
  ISpLexicon = interface;
  ISpShortcut = interface;
  ISpPhoneConverter = interface;
  ISpPhoneticAlphabetConverter = interface;
  ISpXMLRecoResult = interface;
  ISpRecoGrammar2 = interface;
  ISpeechResourceLoader = interface;
  ISpeechResourceLoaderDisp = dispinterface;
  IInternetSecurityManager = interface;
  IInternetSecurityMgrSite = interface;
  IEnumString = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  SpNotifyTranslator = ISpNotifyTranslator;
  SpObjectTokenCategory = ISpeechObjectTokenCategory;
  SpObjectToken = ISpeechObjectToken;
  SpResourceManager = ISpResourceManager;
  SpStreamFormatConverter = ISpStreamFormatConverter;
  SpMMAudioEnum = IEnumSpObjectTokens;
  SpMMAudioIn = ISpeechMMSysAudio;
  SpMMAudioOut = ISpeechMMSysAudio;
  SpStream = ISpStream;
  SpVoice = ISpeechVoice;
  SpSharedRecoContext = ISpeechRecoContext;
  SpInprocRecognizer = ISpeechRecognizer;
  SpSharedRecognizer = ISpeechRecognizer;
  SpLexicon = ISpeechLexicon;
  SpUnCompressedLexicon = ISpeechLexicon;
  SpCompressedLexicon = ISpLexicon;
  SpShortcut = ISpShortcut;
  SpPhoneConverter = ISpeechPhoneConverter;
  SpPhoneticAlphabetConverter = ISpPhoneticAlphabetConverter;
  SpNullPhoneConverter = ISpPhoneConverter;
  SpTextSelectionInformation = ISpeechTextSelectionInformation;
  SpPhraseInfoBuilder = ISpeechPhraseInfoBuilder;
  SpAudioFormat = ISpeechAudioFormat;
  SpWaveFormatEx = ISpeechWaveFormatEx;
  SpInProcRecoContext = ISpeechRecoContext;
  SpCustomStream = ISpeechCustomStream;
  SpFileStream = ISpeechFileStream;
  SpMemoryStream = ISpeechMemoryStream;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  wireHWND = ^_RemotableHandle; 
  PUserType9 = ^SPPHRASERULE; {*}
  PUserType10 = ^SPPHRASEPROPERTY; {*}
  PUserType14 = ^SPWORDPRONUNCIATION; {*}
  PUserType15 = ^SPWORD; {*}
  PUserType16 = ^SPSHORTCUTPAIR; {*}
  POleVariant1 = ^OleVariant; {*}
  PPPrivateAlias1 = ^Pointer; {*}
  PByte1 = ^Byte; {*}
  PUINT1 = ^LongWord; {*}
  PUserType1 = ^TGUID; {*}
  PUserType2 = ^WAVEFORMATEX; {*}
  PUserType3 = ^SPEVENT; {*}
  PUserType4 = ^SPAUDIOBUFFERINFO; {*}
  PUserType5 = ^SPAUDIOOPTIONS; {*}
  PUserType6 = ^SPSERIALIZEDRESULT; {*}
  PUserType7 = ^SPPHRASE; {*}
  PUserType8 = ^SPSERIALIZEDPHRASE; {*}
  PUserType11 = ^SPBINARYGRAMMAR; {*}
  PWord1 = ^Word; {*}
  PUserType12 = ^SPTEXTSELECTIONINFO; {*}
  PUserType13 = ^SPPROPERTYINFO; {*}
  PUserType17 = ^SPRULE; {*}


  __MIDL_IWinTypes_0009 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: Integer);
  end;

  _RemotableHandle = record
    fContext: Integer;
    u: __MIDL_IWinTypes_0009;
  end;

  UINT_PTR = LongWord; 
  LONG_PTR = Integer; 

{$ALIGN 8}
  _LARGE_INTEGER = record
    QuadPart: Int64;
  end;

  _ULARGE_INTEGER = record
    QuadPart: Largeuint;
  end;

{$ALIGN 4}
  _FILETIME = record
    dwLowDateTime: LongWord;
    dwHighDateTime: LongWord;
  end;

{$ALIGN 8}
  tagSTATSTG = record
    pwcsName: PWideChar;
    type_: LongWord;
    cbSize: _ULARGE_INTEGER;
    mtime: _FILETIME;
    ctime: _FILETIME;
    atime: _FILETIME;
    grfMode: LongWord;
    grfLocksSupported: LongWord;
    clsid: TGUID;
    grfStateBits: LongWord;
    reserved: LongWord;
  end;

{$ALIGN 4}
  WAVEFORMATEX = record
    wFormatTag: Word;
    nChannels: Word;
    nSamplesPerSec: LongWord;
    nAvgBytesPerSec: LongWord;
    nBlockAlign: Word;
    wBitsPerSample: Word;
    cbSize: Word;
  end;

{$ALIGN 8}
  SPEVENT = record
    eEventId: Word;
    elParamType: Word;
    ulStreamNum: LongWord;
    ullAudioStreamOffset: Largeuint;
    wParam: UINT_PTR;
    lParam: LONG_PTR;
  end;

  SPEVENTSOURCEINFO = record
    ullEventInterest: Largeuint;
    ullQueuedInterest: Largeuint;
    ulCount: LongWord;
  end;

  SPAUDIOSTATE = _SPAUDIOSTATE; 

  SPAUDIOSTATUS = record
    cbFreeBuffSpace: Integer;
    cbNonBlockingIO: LongWord;
    State: SPAUDIOSTATE;
    CurSeekPos: Largeuint;
    CurDevicePos: Largeuint;
    dwAudioLevel: LongWord;
    dwReserved2: LongWord;
  end;

{$ALIGN 4}
  SPAUDIOBUFFERINFO = record
    ulMsMinNotification: LongWord;
    ulMsBufferSize: LongWord;
    ulMsEventBias: LongWord;
  end;

  SPVOICESTATUS = record
    ulCurrentStream: LongWord;
    ulLastStreamQueued: LongWord;
    hrLastResult: HResult;
    dwRunningState: LongWord;
    ulInputWordPos: LongWord;
    ulInputWordLen: LongWord;
    ulInputSentPos: LongWord;
    ulInputSentLen: LongWord;
    lBookmarkId: Integer;
    PhonemeId: Word;
    VisemeId: SPVISEMES;
    dwReserved1: LongWord;
    dwReserved2: LongWord;
  end;

{$ALIGN 8}
  SPRECOGNIZERSTATUS = record
    AudioStatus: SPAUDIOSTATUS;
    ullRecognitionStreamPos: Largeuint;
    ulStreamNumber: LongWord;
    ulNumActive: LongWord;
    ClsidEngine: TGUID;
    cLangIDs: LongWord;
    aLangID: array[0..19] of Word;
    ullRecognitionStreamTime: Largeuint;
  end;

  SPSTREAMFORMATTYPE = SPWAVEFORMATTYPE; 

{$ALIGN 4}
  SPPHRASERULE = record
    pszName: PWideChar;
    ulId: LongWord;
    ulFirstElement: LongWord;
    ulCountOfElements: LongWord;
    pNextSibling: PUserType9;
    pFirstChild: PUserType9;
    SREngineConfidence: Single;
    Confidence: Shortint;
  end;

{$ALIGN 2}
  __MIDL___MIDL_itf_sapi_0000_0020_0002 = record
    bType: Byte;
    bReserved: Byte;
    usArrayIndex: Word;
  end;

{$ALIGN 4}
  __MIDL___MIDL_itf_sapi_0000_0020_0001 = record
    case Integer of
      0: (ulId: LongWord);
      1: (__MIDL____MIDL_itf_sapi_0000_00200000: __MIDL___MIDL_itf_sapi_0000_0020_0002);
  end;


  SPPHRASEELEMENT = record
    ulAudioTimeOffset: LongWord;
    ulAudioSizeTime: LongWord;
    ulAudioStreamOffset: LongWord;
    ulAudioSizeBytes: LongWord;
    ulRetainedStreamOffset: LongWord;
    ulRetainedSizeBytes: LongWord;
    pszDisplayText: PWideChar;
    pszLexicalForm: PWideChar;
    pszPronunciation: ^Word;
    bDisplayAttributes: Byte;
    RequiredConfidence: Shortint;
    ActualConfidence: Shortint;
    reserved: Byte;
    SREngineConfidence: Single;
  end;

  SPPHRASEREPLACEMENT = record
    bDisplayAttributes: Byte;
    pszReplacementText: PWideChar;
    ulFirstElement: LongWord;
    ulCountOfElements: LongWord;
  end;

  SPSEMANTICERRORINFO = record
    ulLineNumber: LongWord;
    pszScriptLine: PWideChar;
    pszSource: PWideChar;
    pszDescription: PWideChar;
    hrResultCode: HResult;
  end;

  SPSERIALIZEDPHRASE = record
    ulSerializedSize: LongWord;
  end;

{$ALIGN 8}
  tagSPPROPERTYINFO = record
    pszName: PWideChar;
    ulId: LongWord;
    pszValue: PWideChar;
    vValue: OleVariant;
  end;

  SPPROPERTYINFO = tagSPPROPERTYINFO; 

{$ALIGN 4}
  SPBINARYGRAMMAR = record
    ulTotalSerializedSize: LongWord;
  end;

  tagSPTEXTSELECTIONINFO = record
    ulStartActiveOffset: LongWord;
    cchActiveChars: LongWord;
    ulStartSelection: LongWord;
    cchSelection: LongWord;
  end;

  SPTEXTSELECTIONINFO = tagSPTEXTSELECTIONINFO; 

  SPRECOCONTEXTSTATUS = record
    eInterference: SPINTERFERENCE;
    szRequestTypeOfUI: array[0..254] of Word;
    dwReserved1: LongWord;
    dwReserved2: LongWord;
  end;

  SPSERIALIZEDRESULT = record
    ulSerializedSize: LongWord;
  end;

{$ALIGN 8}
  SPRECORESULTTIMES = record
    ftStreamTime: _FILETIME;
    ullLength: Largeuint;
    dwTickCount: LongWord;
    ullStart: Largeuint;
  end;


{$ALIGN 4}
  SPWORDPRONUNCIATION = record
    pNextWordPronunciation: PUserType14;
    eLexiconType: SPLEXICONTYPE;
    LangId: Word;
    wPronunciationFlags: Word;
    ePartOfSpeech: SPPARTOFSPEECH;
    szPronunciation: array[0..0] of Word;
  end;


  SPWORD = record
    pNextWord: PUserType15;
    LangId: Word;
    wReserved: Word;
    eWordType: SPWORDTYPE;
    pszWord: PWideChar;
    pFirstWordPronunciation: ^SPWORDPRONUNCIATION;
  end;


  SPSHORTCUTPAIR = record
    pNextSHORTCUTPAIR: PUserType16;
    LangId: Word;
    shType: SPSHORTCUTTYPE;
    pszDisplay: PWideChar;
    pszSpoken: PWideChar;
  end;

  SPRULE = record
    pszRuleName: PWideChar;
    ulRuleId: LongWord;
    dwAttributes: LongWord;
  end;

  ULONG_PTR = LongWord; 

{$ALIGN 8}
  SPPHRASEPROPERTY = record
    pszName: PWideChar;
    __MIDL____MIDL_itf_sapi_0000_00200001: __MIDL___MIDL_itf_sapi_0000_0020_0001;
    pszValue: PWideChar;
    vValue: OleVariant;
    ulFirstElement: LongWord;
    ulCountOfElements: LongWord;
    pNextSibling: PUserType10;
    pFirstChild: PUserType10;
    SREngineConfidence: Single;
    Confidence: Shortint;
  end;

  SPPHRASE = record
    cbSize: LongWord;
    LangId: Word;
    wHomophoneGroupId: Word;
    ullGrammarID: Largeuint;
    ftStartTime: Largeuint;
    ullAudioStreamPosition: Largeuint;
    ulAudioSizeBytes: LongWord;
    ulRetainedSizeBytes: LongWord;
    ulAudioSizeTime: LongWord;
    Rule: SPPHRASERULE;
    pProperties: ^SPPHRASEPROPERTY;
    pElements: ^SPPHRASEELEMENT;
    cReplacements: LongWord;
    pReplacements: ^SPPHRASEREPLACEMENT;
    SREngineID: TGUID;
    ulSREnginePrivateDataSize: LongWord;
    pSREnginePrivateData: ^Byte;
    pSML: PWideChar;
    pSemanticErrorInfo: ^SPSEMANTICERRORINFO;
    SemanticTagFormat: SPSEMANTICFORMAT;
  end;


{$ALIGN 4}
  SPWORDPRONUNCIATIONLIST = record
    ulSize: LongWord;
    pvBuffer: ^Byte;
    pFirstWordPronunciation: ^SPWORDPRONUNCIATION;
  end;

  SPWORDLIST = record
    ulSize: LongWord;
    pvBuffer: ^Byte;
    pFirstWord: ^SPWORD;
  end;

  SPSHORTCUTPAIRLIST = record
    ulSize: LongWord;
    pvBuffer: ^Byte;
    pFirstShortcutPair: ^SPSHORTCUTPAIR;
  end;


// *********************************************************************//
// Interface: ISpeechDataKey
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CE17C09B-4EFA-44D5-A4C9-59D9585AB0CD}
// *********************************************************************//
  ISpeechDataKey = interface(IDispatch)
    ['{CE17C09B-4EFA-44D5-A4C9-59D9585AB0CD}']
    procedure SetBinaryValue(const ValueName: WideString; Value: OleVariant); safecall;
    function GetBinaryValue(const ValueName: WideString): OleVariant; safecall;
    procedure SetStringValue(const ValueName: WideString; const Value: WideString); safecall;
    function GetStringValue(const ValueName: WideString): WideString; safecall;
    procedure SetLongValue(const ValueName: WideString; Value: Integer); safecall;
    function GetLongValue(const ValueName: WideString): Integer; safecall;
    function OpenKey(const SubKeyName: WideString): ISpeechDataKey; safecall;
    function CreateKey(const SubKeyName: WideString): ISpeechDataKey; safecall;
    procedure DeleteKey(const SubKeyName: WideString); safecall;
    procedure DeleteValue(const ValueName: WideString); safecall;
    function EnumKeys(Index: Integer): WideString; safecall;
    function EnumValues(Index: Integer): WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  ISpeechDataKeyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CE17C09B-4EFA-44D5-A4C9-59D9585AB0CD}
// *********************************************************************//
  ISpeechDataKeyDisp = dispinterface
    ['{CE17C09B-4EFA-44D5-A4C9-59D9585AB0CD}']
    procedure SetBinaryValue(const ValueName: WideString; Value: OleVariant); dispid 1;
    function GetBinaryValue(const ValueName: WideString): OleVariant; dispid 2;
    procedure SetStringValue(const ValueName: WideString; const Value: WideString); dispid 3;
    function GetStringValue(const ValueName: WideString): WideString; dispid 4;
    procedure SetLongValue(const ValueName: WideString; Value: Integer); dispid 5;
    function GetLongValue(const ValueName: WideString): Integer; dispid 6;
    function OpenKey(const SubKeyName: WideString): ISpeechDataKey; dispid 7;
    function CreateKey(const SubKeyName: WideString): ISpeechDataKey; dispid 8;
    procedure DeleteKey(const SubKeyName: WideString); dispid 9;
    procedure DeleteValue(const ValueName: WideString); dispid 10;
    function EnumKeys(Index: Integer): WideString; dispid 11;
    function EnumValues(Index: Integer): WideString; dispid 12;
  end;

// *********************************************************************//
// Interface: ISpeechObjectToken
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C74A3ADC-B727-4500-A84A-B526721C8B8C}
// *********************************************************************//
  ISpeechObjectToken = interface(IDispatch)
    ['{C74A3ADC-B727-4500-A84A-B526721C8B8C}']
    function Get_Id: WideString; safecall;
    function Get_DataKey: ISpeechDataKey; safecall;
    function Get_Category: ISpeechObjectTokenCategory; safecall;
    function GetDescription(Locale: Integer): WideString; safecall;
    procedure SetId(const Id: WideString; const CategoryID: WideString; CreateIfNotExist: WordBool); safecall;
    function GetAttribute(const AttributeName: WideString): WideString; safecall;
    function CreateInstance(const pUnkOuter: IUnknown; ClsContext: SpeechTokenContext): IUnknown; safecall;
    procedure Remove(const ObjectStorageCLSID: WideString); safecall;
    function GetStorageFileName(const ObjectStorageCLSID: WideString; const KeyName: WideString; 
                                const FileName: WideString; Folder: SpeechTokenShellFolder): WideString; safecall;
    procedure RemoveStorageFileName(const ObjectStorageCLSID: WideString; 
                                    const KeyName: WideString; DeleteFile: WordBool); safecall;
    function IsUISupported(const TypeOfUI: WideString; const ExtraData: OleVariant; 
                           const Object_: IUnknown): WordBool; safecall;
    procedure DisplayUI(hWnd: Integer; const Title: WideString; const TypeOfUI: WideString; 
                        const ExtraData: OleVariant; const Object_: IUnknown); safecall;
    function MatchesAttributes(const Attributes: WideString): WordBool; safecall;
    property Id: WideString read Get_Id;
    property DataKey: ISpeechDataKey read Get_DataKey;
    property Category: ISpeechObjectTokenCategory read Get_Category;
  end;

// *********************************************************************//
// DispIntf:  ISpeechObjectTokenDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C74A3ADC-B727-4500-A84A-B526721C8B8C}
// *********************************************************************//
  ISpeechObjectTokenDisp = dispinterface
    ['{C74A3ADC-B727-4500-A84A-B526721C8B8C}']
    property Id: WideString readonly dispid 1;
    property DataKey: ISpeechDataKey readonly dispid 2;
    property Category: ISpeechObjectTokenCategory readonly dispid 3;
    function GetDescription(Locale: Integer): WideString; dispid 4;
    procedure SetId(const Id: WideString; const CategoryID: WideString; CreateIfNotExist: WordBool); dispid 5;
    function GetAttribute(const AttributeName: WideString): WideString; dispid 6;
    function CreateInstance(const pUnkOuter: IUnknown; ClsContext: SpeechTokenContext): IUnknown; dispid 7;
    procedure Remove(const ObjectStorageCLSID: WideString); dispid 8;
    function GetStorageFileName(const ObjectStorageCLSID: WideString; const KeyName: WideString; 
                                const FileName: WideString; Folder: SpeechTokenShellFolder): WideString; dispid 9;
    procedure RemoveStorageFileName(const ObjectStorageCLSID: WideString; 
                                    const KeyName: WideString; DeleteFile: WordBool); dispid 10;
    function IsUISupported(const TypeOfUI: WideString; const ExtraData: OleVariant; 
                           const Object_: IUnknown): WordBool; dispid 11;
    procedure DisplayUI(hWnd: Integer; const Title: WideString; const TypeOfUI: WideString; 
                        const ExtraData: OleVariant; const Object_: IUnknown); dispid 12;
    function MatchesAttributes(const Attributes: WideString): WordBool; dispid 13;
  end;

// *********************************************************************//
// Interface: ISpeechObjectTokenCategory
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CA7EAC50-2D01-4145-86D4-5AE7D70F4469}
// *********************************************************************//
  ISpeechObjectTokenCategory = interface(IDispatch)
    ['{CA7EAC50-2D01-4145-86D4-5AE7D70F4469}']
    function Get_Id: WideString; safecall;
    procedure Set_Default(const TokenId: WideString); safecall;
    function Get_Default: WideString; safecall;
    procedure SetId(const Id: WideString; CreateIfNotExist: WordBool); safecall;
    function GetDataKey(Location: SpeechDataKeyLocation): ISpeechDataKey; safecall;
    function EnumerateTokens(const RequiredAttributes: WideString; 
                             const OptionalAttributes: WideString): ISpeechObjectTokens; safecall;
    property Id: WideString read Get_Id;
    property Default: WideString read Get_Default write Set_Default;
  end;

// *********************************************************************//
// DispIntf:  ISpeechObjectTokenCategoryDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CA7EAC50-2D01-4145-86D4-5AE7D70F4469}
// *********************************************************************//
  ISpeechObjectTokenCategoryDisp = dispinterface
    ['{CA7EAC50-2D01-4145-86D4-5AE7D70F4469}']
    property Id: WideString readonly dispid 1;
    property Default: WideString dispid 2;
    procedure SetId(const Id: WideString; CreateIfNotExist: WordBool); dispid 3;
    function GetDataKey(Location: SpeechDataKeyLocation): ISpeechDataKey; dispid 4;
    function EnumerateTokens(const RequiredAttributes: WideString; 
                             const OptionalAttributes: WideString): ISpeechObjectTokens; dispid 5;
  end;

// *********************************************************************//
// Interface: ISpeechObjectTokens
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9285B776-2E7B-4BC0-B53E-580EB6FA967F}
// *********************************************************************//
  ISpeechObjectTokens = interface(IDispatch)
    ['{9285B776-2E7B-4BC0-B53E-580EB6FA967F}']
    function Get_Count: Integer; safecall;
    function Item(Index: Integer): ISpeechObjectToken; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISpeechObjectTokensDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9285B776-2E7B-4BC0-B53E-580EB6FA967F}
// *********************************************************************//
  ISpeechObjectTokensDisp = dispinterface
    ['{9285B776-2E7B-4BC0-B53E-580EB6FA967F}']
    property Count: Integer readonly dispid 1;
    function Item(Index: Integer): ISpeechObjectToken; dispid 0;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISpeechAudioBufferInfo
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {11B103D8-1142-4EDF-A093-82FB3915F8CC}
// *********************************************************************//
  ISpeechAudioBufferInfo = interface(IDispatch)
    ['{11B103D8-1142-4EDF-A093-82FB3915F8CC}']
    function Get_MinNotification: Integer; safecall;
    procedure Set_MinNotification(MinNotification: Integer); safecall;
    function Get_BufferSize: Integer; safecall;
    procedure Set_BufferSize(BufferSize: Integer); safecall;
    function Get_EventBias: Integer; safecall;
    procedure Set_EventBias(EventBias: Integer); safecall;
    property MinNotification: Integer read Get_MinNotification write Set_MinNotification;
    property BufferSize: Integer read Get_BufferSize write Set_BufferSize;
    property EventBias: Integer read Get_EventBias write Set_EventBias;
  end;

// *********************************************************************//
// DispIntf:  ISpeechAudioBufferInfoDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {11B103D8-1142-4EDF-A093-82FB3915F8CC}
// *********************************************************************//
  ISpeechAudioBufferInfoDisp = dispinterface
    ['{11B103D8-1142-4EDF-A093-82FB3915F8CC}']
    property MinNotification: Integer dispid 1;
    property BufferSize: Integer dispid 2;
    property EventBias: Integer dispid 3;
  end;

// *********************************************************************//
// Interface: ISpeechAudioStatus
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C62D9C91-7458-47F6-862D-1EF86FB0B278}
// *********************************************************************//
  ISpeechAudioStatus = interface(IDispatch)
    ['{C62D9C91-7458-47F6-862D-1EF86FB0B278}']
    function Get_FreeBufferSpace: Integer; safecall;
    function Get_NonBlockingIO: Integer; safecall;
    function Get_State: SpeechAudioState; safecall;
    function Get_CurrentSeekPosition: OleVariant; safecall;
    function Get_CurrentDevicePosition: OleVariant; safecall;
    property FreeBufferSpace: Integer read Get_FreeBufferSpace;
    property NonBlockingIO: Integer read Get_NonBlockingIO;
    property State: SpeechAudioState read Get_State;
    property CurrentSeekPosition: OleVariant read Get_CurrentSeekPosition;
    property CurrentDevicePosition: OleVariant read Get_CurrentDevicePosition;
  end;

// *********************************************************************//
// DispIntf:  ISpeechAudioStatusDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C62D9C91-7458-47F6-862D-1EF86FB0B278}
// *********************************************************************//
  ISpeechAudioStatusDisp = dispinterface
    ['{C62D9C91-7458-47F6-862D-1EF86FB0B278}']
    property FreeBufferSpace: Integer readonly dispid 1;
    property NonBlockingIO: Integer readonly dispid 2;
    property State: SpeechAudioState readonly dispid 3;
    property CurrentSeekPosition: OleVariant readonly dispid 4;
    property CurrentDevicePosition: OleVariant readonly dispid 5;
  end;

// *********************************************************************//
// Interface: ISpeechAudioFormat
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E6E9C590-3E18-40E3-8299-061F98BDE7C7}
// *********************************************************************//
  ISpeechAudioFormat = interface(IDispatch)
    ['{E6E9C590-3E18-40E3-8299-061F98BDE7C7}']
    function Get_type_: SpeechAudioFormatType; safecall;
    procedure Set_type_(AudioFormat: SpeechAudioFormatType); safecall;
    function Get_Guid: WideString; safecall;
    procedure Set_Guid(const Guid: WideString); safecall;
    function GetWaveFormatEx: ISpeechWaveFormatEx; safecall;
    procedure SetWaveFormatEx(const SpeechWaveFormatEx: ISpeechWaveFormatEx); safecall;
    property type_: SpeechAudioFormatType read Get_type_ write Set_type_;
    property Guid: WideString read Get_Guid write Set_Guid;
  end;

// *********************************************************************//
// DispIntf:  ISpeechAudioFormatDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E6E9C590-3E18-40E3-8299-061F98BDE7C7}
// *********************************************************************//
  ISpeechAudioFormatDisp = dispinterface
    ['{E6E9C590-3E18-40E3-8299-061F98BDE7C7}']
    property type_: SpeechAudioFormatType dispid 1;
    property Guid: WideString dispid 2;
    function GetWaveFormatEx: ISpeechWaveFormatEx; dispid 3;
    procedure SetWaveFormatEx(const SpeechWaveFormatEx: ISpeechWaveFormatEx); dispid 4;
  end;

// *********************************************************************//
// Interface: ISpeechWaveFormatEx
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7A1EF0D5-1581-4741-88E4-209A49F11A10}
// *********************************************************************//
  ISpeechWaveFormatEx = interface(IDispatch)
    ['{7A1EF0D5-1581-4741-88E4-209A49F11A10}']
    function Get_FormatTag: Smallint; safecall;
    procedure Set_FormatTag(FormatTag: Smallint); safecall;
    function Get_Channels: Smallint; safecall;
    procedure Set_Channels(Channels: Smallint); safecall;
    function Get_SamplesPerSec: Integer; safecall;
    procedure Set_SamplesPerSec(SamplesPerSec: Integer); safecall;
    function Get_AvgBytesPerSec: Integer; safecall;
    procedure Set_AvgBytesPerSec(AvgBytesPerSec: Integer); safecall;
    function Get_BlockAlign: Smallint; safecall;
    procedure Set_BlockAlign(BlockAlign: Smallint); safecall;
    function Get_BitsPerSample: Smallint; safecall;
    procedure Set_BitsPerSample(BitsPerSample: Smallint); safecall;
    function Get_ExtraData: OleVariant; safecall;
    procedure Set_ExtraData(ExtraData: OleVariant); safecall;
    property FormatTag: Smallint read Get_FormatTag write Set_FormatTag;
    property Channels: Smallint read Get_Channels write Set_Channels;
    property SamplesPerSec: Integer read Get_SamplesPerSec write Set_SamplesPerSec;
    property AvgBytesPerSec: Integer read Get_AvgBytesPerSec write Set_AvgBytesPerSec;
    property BlockAlign: Smallint read Get_BlockAlign write Set_BlockAlign;
    property BitsPerSample: Smallint read Get_BitsPerSample write Set_BitsPerSample;
    property ExtraData: OleVariant read Get_ExtraData write Set_ExtraData;
  end;

// *********************************************************************//
// DispIntf:  ISpeechWaveFormatExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7A1EF0D5-1581-4741-88E4-209A49F11A10}
// *********************************************************************//
  ISpeechWaveFormatExDisp = dispinterface
    ['{7A1EF0D5-1581-4741-88E4-209A49F11A10}']
    property FormatTag: Smallint dispid 1;
    property Channels: Smallint dispid 2;
    property SamplesPerSec: Integer dispid 3;
    property AvgBytesPerSec: Integer dispid 4;
    property BlockAlign: Smallint dispid 5;
    property BitsPerSample: Smallint dispid 6;
    property ExtraData: OleVariant dispid 7;
  end;

// *********************************************************************//
// Interface: ISpeechBaseStream
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6450336F-7D49-4CED-8097-49D6DEE37294}
// *********************************************************************//
  ISpeechBaseStream = interface(IDispatch)
    ['{6450336F-7D49-4CED-8097-49D6DEE37294}']
    function Get_Format: ISpeechAudioFormat; safecall;
    procedure _Set_Format(const AudioFormat: ISpeechAudioFormat); safecall;
    function Read(out Buffer: OleVariant; NumberOfBytes: Integer): Integer; safecall;
    function Write(Buffer: OleVariant): Integer; safecall;
    function Seek(Position: OleVariant; Origin: SpeechStreamSeekPositionType): OleVariant; safecall;
    property Format: ISpeechAudioFormat read Get_Format write _Set_Format;
  end;

// *********************************************************************//
// DispIntf:  ISpeechBaseStreamDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6450336F-7D49-4CED-8097-49D6DEE37294}
// *********************************************************************//
  ISpeechBaseStreamDisp = dispinterface
    ['{6450336F-7D49-4CED-8097-49D6DEE37294}']
    property Format: ISpeechAudioFormat dispid 1;
    function Read(out Buffer: OleVariant; NumberOfBytes: Integer): Integer; dispid 2;
    function Write(Buffer: OleVariant): Integer; dispid 3;
    function Seek(Position: OleVariant; Origin: SpeechStreamSeekPositionType): OleVariant; dispid 4;
  end;

// *********************************************************************//
// Interface: ISpeechFileStream
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AF67F125-AB39-4E93-B4A2-CC2E66E182A7}
// *********************************************************************//
  ISpeechFileStream = interface(ISpeechBaseStream)
    ['{AF67F125-AB39-4E93-B4A2-CC2E66E182A7}']
    procedure Open(const FileName: WideString; FileMode: SpeechStreamFileMode; DoEvents: WordBool); safecall;
    procedure Close; safecall;
  end;

// *********************************************************************//
// DispIntf:  ISpeechFileStreamDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AF67F125-AB39-4E93-B4A2-CC2E66E182A7}
// *********************************************************************//
  ISpeechFileStreamDisp = dispinterface
    ['{AF67F125-AB39-4E93-B4A2-CC2E66E182A7}']
    procedure Open(const FileName: WideString; FileMode: SpeechStreamFileMode; DoEvents: WordBool); dispid 100;
    procedure Close; dispid 101;
    property Format: ISpeechAudioFormat dispid 1;
    function Read(out Buffer: OleVariant; NumberOfBytes: Integer): Integer; dispid 2;
    function Write(Buffer: OleVariant): Integer; dispid 3;
    function Seek(Position: OleVariant; Origin: SpeechStreamSeekPositionType): OleVariant; dispid 4;
  end;

// *********************************************************************//
// Interface: ISpeechMemoryStream
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EEB14B68-808B-4ABE-A5EA-B51DA7588008}
// *********************************************************************//
  ISpeechMemoryStream = interface(ISpeechBaseStream)
    ['{EEB14B68-808B-4ABE-A5EA-B51DA7588008}']
    procedure SetData(Data: OleVariant); safecall;
    function GetData: OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  ISpeechMemoryStreamDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EEB14B68-808B-4ABE-A5EA-B51DA7588008}
// *********************************************************************//
  ISpeechMemoryStreamDisp = dispinterface
    ['{EEB14B68-808B-4ABE-A5EA-B51DA7588008}']
    procedure SetData(Data: OleVariant); dispid 100;
    function GetData: OleVariant; dispid 101;
    property Format: ISpeechAudioFormat dispid 1;
    function Read(out Buffer: OleVariant; NumberOfBytes: Integer): Integer; dispid 2;
    function Write(Buffer: OleVariant): Integer; dispid 3;
    function Seek(Position: OleVariant; Origin: SpeechStreamSeekPositionType): OleVariant; dispid 4;
  end;

// *********************************************************************//
// Interface: ISpeechCustomStream
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1A9E9F4F-104F-4DB8-A115-EFD7FD0C97AE}
// *********************************************************************//
  ISpeechCustomStream = interface(ISpeechBaseStream)
    ['{1A9E9F4F-104F-4DB8-A115-EFD7FD0C97AE}']
    function Get_BaseStream: IUnknown; safecall;
    procedure _Set_BaseStream(const ppUnkStream: IUnknown); safecall;
    property BaseStream: IUnknown read Get_BaseStream write _Set_BaseStream;
  end;

// *********************************************************************//
// DispIntf:  ISpeechCustomStreamDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1A9E9F4F-104F-4DB8-A115-EFD7FD0C97AE}
// *********************************************************************//
  ISpeechCustomStreamDisp = dispinterface
    ['{1A9E9F4F-104F-4DB8-A115-EFD7FD0C97AE}']
    property BaseStream: IUnknown dispid 100;
    property Format: ISpeechAudioFormat dispid 1;
    function Read(out Buffer: OleVariant; NumberOfBytes: Integer): Integer; dispid 2;
    function Write(Buffer: OleVariant): Integer; dispid 3;
    function Seek(Position: OleVariant; Origin: SpeechStreamSeekPositionType): OleVariant; dispid 4;
  end;

// *********************************************************************//
// Interface: ISpeechAudio
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CFF8E175-019E-11D3-A08E-00C04F8EF9B5}
// *********************************************************************//
  ISpeechAudio = interface(ISpeechBaseStream)
    ['{CFF8E175-019E-11D3-A08E-00C04F8EF9B5}']
    function Get_Status: ISpeechAudioStatus; safecall;
    function Get_BufferInfo: ISpeechAudioBufferInfo; safecall;
    function Get_DefaultFormat: ISpeechAudioFormat; safecall;
    function Get_Volume: Integer; safecall;
    procedure Set_Volume(Volume: Integer); safecall;
    function Get_BufferNotifySize: Integer; safecall;
    procedure Set_BufferNotifySize(BufferNotifySize: Integer); safecall;
    function Get_EventHandle: Integer; safecall;
    procedure SetState(State: SpeechAudioState); safecall;
    property Status: ISpeechAudioStatus read Get_Status;
    property BufferInfo: ISpeechAudioBufferInfo read Get_BufferInfo;
    property DefaultFormat: ISpeechAudioFormat read Get_DefaultFormat;
    property Volume: Integer read Get_Volume write Set_Volume;
    property BufferNotifySize: Integer read Get_BufferNotifySize write Set_BufferNotifySize;
    property EventHandle: Integer read Get_EventHandle;
  end;

// *********************************************************************//
// DispIntf:  ISpeechAudioDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CFF8E175-019E-11D3-A08E-00C04F8EF9B5}
// *********************************************************************//
  ISpeechAudioDisp = dispinterface
    ['{CFF8E175-019E-11D3-A08E-00C04F8EF9B5}']
    property Status: ISpeechAudioStatus readonly dispid 200;
    property BufferInfo: ISpeechAudioBufferInfo readonly dispid 201;
    property DefaultFormat: ISpeechAudioFormat readonly dispid 202;
    property Volume: Integer dispid 203;
    property BufferNotifySize: Integer dispid 204;
    property EventHandle: Integer readonly dispid 205;
    procedure SetState(State: SpeechAudioState); dispid 206;
    property Format: ISpeechAudioFormat dispid 1;
    function Read(out Buffer: OleVariant; NumberOfBytes: Integer): Integer; dispid 2;
    function Write(Buffer: OleVariant): Integer; dispid 3;
    function Seek(Position: OleVariant; Origin: SpeechStreamSeekPositionType): OleVariant; dispid 4;
  end;

// *********************************************************************//
// Interface: ISpeechMMSysAudio
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3C76AF6D-1FD7-4831-81D1-3B71D5A13C44}
// *********************************************************************//
  ISpeechMMSysAudio = interface(ISpeechAudio)
    ['{3C76AF6D-1FD7-4831-81D1-3B71D5A13C44}']
    function Get_DeviceId: Integer; safecall;
    procedure Set_DeviceId(DeviceId: Integer); safecall;
    function Get_LineId: Integer; safecall;
    procedure Set_LineId(LineId: Integer); safecall;
    function Get_MMHandle: Integer; safecall;
    property DeviceId: Integer read Get_DeviceId write Set_DeviceId;
    property LineId: Integer read Get_LineId write Set_LineId;
    property MMHandle: Integer read Get_MMHandle;
  end;

// *********************************************************************//
// DispIntf:  ISpeechMMSysAudioDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3C76AF6D-1FD7-4831-81D1-3B71D5A13C44}
// *********************************************************************//
  ISpeechMMSysAudioDisp = dispinterface
    ['{3C76AF6D-1FD7-4831-81D1-3B71D5A13C44}']
    property DeviceId: Integer dispid 300;
    property LineId: Integer dispid 301;
    property MMHandle: Integer readonly dispid 302;
    property Status: ISpeechAudioStatus readonly dispid 200;
    property BufferInfo: ISpeechAudioBufferInfo readonly dispid 201;
    property DefaultFormat: ISpeechAudioFormat readonly dispid 202;
    property Volume: Integer dispid 203;
    property BufferNotifySize: Integer dispid 204;
    property EventHandle: Integer readonly dispid 205;
    procedure SetState(State: SpeechAudioState); dispid 206;
    property Format: ISpeechAudioFormat dispid 1;
    function Read(out Buffer: OleVariant; NumberOfBytes: Integer): Integer; dispid 2;
    function Write(Buffer: OleVariant): Integer; dispid 3;
    function Seek(Position: OleVariant; Origin: SpeechStreamSeekPositionType): OleVariant; dispid 4;
  end;

// *********************************************************************//
// Interface: ISpeechVoice
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {269316D8-57BD-11D2-9EEE-00C04F797396}
// *********************************************************************//
  ISpeechVoice = interface(IDispatch)
    ['{269316D8-57BD-11D2-9EEE-00C04F797396}']
    function Get_Status: ISpeechVoiceStatus; safecall;
    function Get_Voice: ISpeechObjectToken; safecall;
    procedure _Set_Voice(const Voice: ISpeechObjectToken); safecall;
    function Get_AudioOutput: ISpeechObjectToken; safecall;
    procedure _Set_AudioOutput(const AudioOutput: ISpeechObjectToken); safecall;
    function Get_AudioOutputStream: ISpeechBaseStream; safecall;
    procedure _Set_AudioOutputStream(const AudioOutputStream: ISpeechBaseStream); safecall;
    function Get_Rate: Integer; safecall;
    procedure Set_Rate(Rate: Integer); safecall;
    function Get_Volume: Integer; safecall;
    procedure Set_Volume(Volume: Integer); safecall;
    procedure Set_AllowAudioOutputFormatChangesOnNextSet(Allow: WordBool); safecall;
    function Get_AllowAudioOutputFormatChangesOnNextSet: WordBool; safecall;
    function Get_EventInterests: SpeechVoiceEvents; safecall;
    procedure Set_EventInterests(EventInterestFlags: SpeechVoiceEvents); safecall;
    procedure Set_Priority(Priority: SpeechVoicePriority); safecall;
    function Get_Priority: SpeechVoicePriority; safecall;
    procedure Set_AlertBoundary(Boundary: SpeechVoiceEvents); safecall;
    function Get_AlertBoundary: SpeechVoiceEvents; safecall;
    procedure Set_SynchronousSpeakTimeout(msTimeout: Integer); safecall;
    function Get_SynchronousSpeakTimeout: Integer; safecall;
    function Speak(const Text: WideString; Flags: SpeechVoiceSpeakFlags): Integer; safecall;
    function SpeakStream(const Stream: ISpeechBaseStream; Flags: SpeechVoiceSpeakFlags): Integer; safecall;
    procedure Pause; safecall;
    procedure Resume; safecall;
    function Skip(const Type_: WideString; NumItems: Integer): Integer; safecall;
    function GetVoices(const RequiredAttributes: WideString; const OptionalAttributes: WideString): ISpeechObjectTokens; safecall;
    function GetAudioOutputs(const RequiredAttributes: WideString; 
                             const OptionalAttributes: WideString): ISpeechObjectTokens; safecall;
    function WaitUntilDone(msTimeout: Integer): WordBool; safecall;
    function SpeakCompleteEvent: Integer; safecall;
    function IsUISupported(const TypeOfUI: WideString; const ExtraData: OleVariant): WordBool; safecall;
    procedure DisplayUI(hWndParent: Integer; const Title: WideString; const TypeOfUI: WideString; 
                        const ExtraData: OleVariant); safecall;
    property Status: ISpeechVoiceStatus read Get_Status;
    property Voice: ISpeechObjectToken read Get_Voice write _Set_Voice;
    property AudioOutput: ISpeechObjectToken read Get_AudioOutput write _Set_AudioOutput;
    property AudioOutputStream: ISpeechBaseStream read Get_AudioOutputStream write _Set_AudioOutputStream;
    property Rate: Integer read Get_Rate write Set_Rate;
    property Volume: Integer read Get_Volume write Set_Volume;
    property AllowAudioOutputFormatChangesOnNextSet: WordBool read Get_AllowAudioOutputFormatChangesOnNextSet write Set_AllowAudioOutputFormatChangesOnNextSet;
    property EventInterests: SpeechVoiceEvents read Get_EventInterests write Set_EventInterests;
    property Priority: SpeechVoicePriority read Get_Priority write Set_Priority;
    property AlertBoundary: SpeechVoiceEvents read Get_AlertBoundary write Set_AlertBoundary;
    property SynchronousSpeakTimeout: Integer read Get_SynchronousSpeakTimeout write Set_SynchronousSpeakTimeout;
  end;

// *********************************************************************//
// DispIntf:  ISpeechVoiceDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {269316D8-57BD-11D2-9EEE-00C04F797396}
// *********************************************************************//
  ISpeechVoiceDisp = dispinterface
    ['{269316D8-57BD-11D2-9EEE-00C04F797396}']
    property Status: ISpeechVoiceStatus readonly dispid 1;
    property Voice: ISpeechObjectToken dispid 2;
    property AudioOutput: ISpeechObjectToken dispid 3;
    property AudioOutputStream: ISpeechBaseStream dispid 4;
    property Rate: Integer dispid 5;
    property Volume: Integer dispid 6;
    property AllowAudioOutputFormatChangesOnNextSet: WordBool dispid 7;
    property EventInterests: SpeechVoiceEvents dispid 8;
    property Priority: SpeechVoicePriority dispid 9;
    property AlertBoundary: SpeechVoiceEvents dispid 10;
    property SynchronousSpeakTimeout: Integer dispid 11;
    function Speak(const Text: WideString; Flags: SpeechVoiceSpeakFlags): Integer; dispid 12;
    function SpeakStream(const Stream: ISpeechBaseStream; Flags: SpeechVoiceSpeakFlags): Integer; dispid 13;
    procedure Pause; dispid 14;
    procedure Resume; dispid 15;
    function Skip(const Type_: WideString; NumItems: Integer): Integer; dispid 16;
    function GetVoices(const RequiredAttributes: WideString; const OptionalAttributes: WideString): ISpeechObjectTokens; dispid 17;
    function GetAudioOutputs(const RequiredAttributes: WideString; 
                             const OptionalAttributes: WideString): ISpeechObjectTokens; dispid 18;
    function WaitUntilDone(msTimeout: Integer): WordBool; dispid 19;
    function SpeakCompleteEvent: Integer; dispid 20;
    function IsUISupported(const TypeOfUI: WideString; const ExtraData: OleVariant): WordBool; dispid 21;
    procedure DisplayUI(hWndParent: Integer; const Title: WideString; const TypeOfUI: WideString; 
                        const ExtraData: OleVariant); dispid 22;
  end;

// *********************************************************************//
// Interface: ISpeechVoiceStatus
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8BE47B07-57F6-11D2-9EEE-00C04F797396}
// *********************************************************************//
  ISpeechVoiceStatus = interface(IDispatch)
    ['{8BE47B07-57F6-11D2-9EEE-00C04F797396}']
    function Get_CurrentStreamNumber: Integer; safecall;
    function Get_LastStreamNumberQueued: Integer; safecall;
    function Get_LastHResult: Integer; safecall;
    function Get_RunningState: SpeechRunState; safecall;
    function Get_InputWordPosition: Integer; safecall;
    function Get_InputWordLength: Integer; safecall;
    function Get_InputSentencePosition: Integer; safecall;
    function Get_InputSentenceLength: Integer; safecall;
    function Get_LastBookmark: WideString; safecall;
    function Get_LastBookmarkId: Integer; safecall;
    function Get_PhonemeId: Smallint; safecall;
    function Get_VisemeId: Smallint; safecall;
    property CurrentStreamNumber: Integer read Get_CurrentStreamNumber;
    property LastStreamNumberQueued: Integer read Get_LastStreamNumberQueued;
    property LastHResult: Integer read Get_LastHResult;
    property RunningState: SpeechRunState read Get_RunningState;
    property InputWordPosition: Integer read Get_InputWordPosition;
    property InputWordLength: Integer read Get_InputWordLength;
    property InputSentencePosition: Integer read Get_InputSentencePosition;
    property InputSentenceLength: Integer read Get_InputSentenceLength;
    property LastBookmark: WideString read Get_LastBookmark;
    property LastBookmarkId: Integer read Get_LastBookmarkId;
    property PhonemeId: Smallint read Get_PhonemeId;
    property VisemeId: Smallint read Get_VisemeId;
  end;

// *********************************************************************//
// DispIntf:  ISpeechVoiceStatusDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8BE47B07-57F6-11D2-9EEE-00C04F797396}
// *********************************************************************//
  ISpeechVoiceStatusDisp = dispinterface
    ['{8BE47B07-57F6-11D2-9EEE-00C04F797396}']
    property CurrentStreamNumber: Integer readonly dispid 1;
    property LastStreamNumberQueued: Integer readonly dispid 2;
    property LastHResult: Integer readonly dispid 3;
    property RunningState: SpeechRunState readonly dispid 4;
    property InputWordPosition: Integer readonly dispid 5;
    property InputWordLength: Integer readonly dispid 6;
    property InputSentencePosition: Integer readonly dispid 7;
    property InputSentenceLength: Integer readonly dispid 8;
    property LastBookmark: WideString readonly dispid 9;
    property LastBookmarkId: Integer readonly dispid 10;
    property PhonemeId: Smallint readonly dispid 11;
    property VisemeId: Smallint readonly dispid 12;
  end;

// *********************************************************************//
// DispIntf:  _ISpeechVoiceEvents
// Flags:     (4096) Dispatchable
// GUID:      {A372ACD1-3BEF-4BBD-8FFB-CB3E2B416AF8}
// *********************************************************************//
  _ISpeechVoiceEvents = dispinterface
    ['{A372ACD1-3BEF-4BBD-8FFB-CB3E2B416AF8}']
    procedure StartStream(StreamNumber: Integer; StreamPosition: OleVariant); dispid 1;
    procedure EndStream(StreamNumber: Integer; StreamPosition: OleVariant); dispid 2;
    procedure VoiceChange(StreamNumber: Integer; StreamPosition: OleVariant; 
                          const VoiceObjectToken: ISpeechObjectToken); dispid 3;
    procedure Bookmark(StreamNumber: Integer; StreamPosition: OleVariant; 
                       const Bookmark: WideString; BookmarkId: Integer); dispid 4;
    procedure Word(StreamNumber: Integer; StreamPosition: OleVariant; CharacterPosition: Integer; 
                   Length: Integer); dispid 5;
    procedure Sentence(StreamNumber: Integer; StreamPosition: OleVariant; 
                       CharacterPosition: Integer; Length: Integer); dispid 7;
    procedure Phoneme(StreamNumber: Integer; StreamPosition: OleVariant; Duration: Integer; 
                      NextPhoneId: Smallint; Feature: SpeechVisemeFeature; CurrentPhoneId: Smallint); dispid 6;
    procedure Viseme(StreamNumber: Integer; StreamPosition: OleVariant; Duration: Integer; 
                     NextVisemeId: SpeechVisemeType; Feature: SpeechVisemeFeature; 
                     CurrentVisemeId: SpeechVisemeType); dispid 8;
    procedure AudioLevel(StreamNumber: Integer; StreamPosition: OleVariant; AudioLevel: Integer); dispid 9;
    procedure EnginePrivate(StreamNumber: Integer; StreamPosition: Integer; EngineData: OleVariant); dispid 10;
  end;

// *********************************************************************//
// Interface: ISpeechRecognizer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2D5F1C0C-BD75-4B08-9478-3B11FEA2586C}
// *********************************************************************//
  ISpeechRecognizer = interface(IDispatch)
    ['{2D5F1C0C-BD75-4B08-9478-3B11FEA2586C}']
    procedure _Set_Recognizer(const Recognizer: ISpeechObjectToken); safecall;
    function Get_Recognizer: ISpeechObjectToken; safecall;
    procedure Set_AllowAudioInputFormatChangesOnNextSet(Allow: WordBool); safecall;
    function Get_AllowAudioInputFormatChangesOnNextSet: WordBool; safecall;
    procedure _Set_AudioInput(const AudioInput: ISpeechObjectToken); safecall;
    function Get_AudioInput: ISpeechObjectToken; safecall;
    procedure _Set_AudioInputStream(const AudioInputStream: ISpeechBaseStream); safecall;
    function Get_AudioInputStream: ISpeechBaseStream; safecall;
    function Get_IsShared: WordBool; safecall;
    procedure Set_State(State: SpeechRecognizerState); safecall;
    function Get_State: SpeechRecognizerState; safecall;
    function Get_Status: ISpeechRecognizerStatus; safecall;
    procedure _Set_Profile(const Profile: ISpeechObjectToken); safecall;
    function Get_Profile: ISpeechObjectToken; safecall;
    procedure EmulateRecognition(TextElements: OleVariant; 
                                 const ElementDisplayAttributes: OleVariant; LanguageId: Integer); safecall;
    function CreateRecoContext: ISpeechRecoContext; safecall;
    function GetFormat(Type_: SpeechFormatType): ISpeechAudioFormat; safecall;
    function SetPropertyNumber(const Name: WideString; Value: Integer): WordBool; safecall;
    function GetPropertyNumber(const Name: WideString; var Value: Integer): WordBool; safecall;
    function SetPropertyString(const Name: WideString; const Value: WideString): WordBool; safecall;
    function GetPropertyString(const Name: WideString; var Value: WideString): WordBool; safecall;
    function IsUISupported(const TypeOfUI: WideString; const ExtraData: OleVariant): WordBool; safecall;
    procedure DisplayUI(hWndParent: Integer; const Title: WideString; const TypeOfUI: WideString; 
                        const ExtraData: OleVariant); safecall;
    function GetRecognizers(const RequiredAttributes: WideString; 
                            const OptionalAttributes: WideString): ISpeechObjectTokens; safecall;
    function GetAudioInputs(const RequiredAttributes: WideString; 
                            const OptionalAttributes: WideString): ISpeechObjectTokens; safecall;
    function GetProfiles(const RequiredAttributes: WideString; const OptionalAttributes: WideString): ISpeechObjectTokens; safecall;
    property Recognizer: ISpeechObjectToken read Get_Recognizer write _Set_Recognizer;
    property AllowAudioInputFormatChangesOnNextSet: WordBool read Get_AllowAudioInputFormatChangesOnNextSet write Set_AllowAudioInputFormatChangesOnNextSet;
    property AudioInput: ISpeechObjectToken read Get_AudioInput write _Set_AudioInput;
    property AudioInputStream: ISpeechBaseStream read Get_AudioInputStream write _Set_AudioInputStream;
    property IsShared: WordBool read Get_IsShared;
    property State: SpeechRecognizerState read Get_State write Set_State;
    property Status: ISpeechRecognizerStatus read Get_Status;
    property Profile: ISpeechObjectToken read Get_Profile write _Set_Profile;
  end;

// *********************************************************************//
// DispIntf:  ISpeechRecognizerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2D5F1C0C-BD75-4B08-9478-3B11FEA2586C}
// *********************************************************************//
  ISpeechRecognizerDisp = dispinterface
    ['{2D5F1C0C-BD75-4B08-9478-3B11FEA2586C}']
    property Recognizer: ISpeechObjectToken dispid 1;
    property AllowAudioInputFormatChangesOnNextSet: WordBool dispid 2;
    property AudioInput: ISpeechObjectToken dispid 3;
    property AudioInputStream: ISpeechBaseStream dispid 4;
    property IsShared: WordBool readonly dispid 5;
    property State: SpeechRecognizerState dispid 6;
    property Status: ISpeechRecognizerStatus readonly dispid 7;
    property Profile: ISpeechObjectToken dispid 8;
    procedure EmulateRecognition(TextElements: OleVariant; 
                                 const ElementDisplayAttributes: OleVariant; LanguageId: Integer); dispid 9;
    function CreateRecoContext: ISpeechRecoContext; dispid 10;
    function GetFormat(Type_: SpeechFormatType): ISpeechAudioFormat; dispid 11;
    function SetPropertyNumber(const Name: WideString; Value: Integer): WordBool; dispid 12;
    function GetPropertyNumber(const Name: WideString; var Value: Integer): WordBool; dispid 13;
    function SetPropertyString(const Name: WideString; const Value: WideString): WordBool; dispid 14;
    function GetPropertyString(const Name: WideString; var Value: WideString): WordBool; dispid 15;
    function IsUISupported(const TypeOfUI: WideString; const ExtraData: OleVariant): WordBool; dispid 16;
    procedure DisplayUI(hWndParent: Integer; const Title: WideString; const TypeOfUI: WideString; 
                        const ExtraData: OleVariant); dispid 17;
    function GetRecognizers(const RequiredAttributes: WideString; 
                            const OptionalAttributes: WideString): ISpeechObjectTokens; dispid 18;
    function GetAudioInputs(const RequiredAttributes: WideString; 
                            const OptionalAttributes: WideString): ISpeechObjectTokens; dispid 19;
    function GetProfiles(const RequiredAttributes: WideString; const OptionalAttributes: WideString): ISpeechObjectTokens; dispid 20;
  end;

// *********************************************************************//
// Interface: ISpeechRecognizerStatus
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BFF9E781-53EC-484E-BB8A-0E1B5551E35C}
// *********************************************************************//
  ISpeechRecognizerStatus = interface(IDispatch)
    ['{BFF9E781-53EC-484E-BB8A-0E1B5551E35C}']
    function Get_AudioStatus: ISpeechAudioStatus; safecall;
    function Get_CurrentStreamPosition: OleVariant; safecall;
    function Get_CurrentStreamNumber: Integer; safecall;
    function Get_NumberOfActiveRules: Integer; safecall;
    function Get_ClsidEngine: WideString; safecall;
    function Get_SupportedLanguages: OleVariant; safecall;
    property AudioStatus: ISpeechAudioStatus read Get_AudioStatus;
    property CurrentStreamPosition: OleVariant read Get_CurrentStreamPosition;
    property CurrentStreamNumber: Integer read Get_CurrentStreamNumber;
    property NumberOfActiveRules: Integer read Get_NumberOfActiveRules;
    property ClsidEngine: WideString read Get_ClsidEngine;
    property SupportedLanguages: OleVariant read Get_SupportedLanguages;
  end;

// *********************************************************************//
// DispIntf:  ISpeechRecognizerStatusDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BFF9E781-53EC-484E-BB8A-0E1B5551E35C}
// *********************************************************************//
  ISpeechRecognizerStatusDisp = dispinterface
    ['{BFF9E781-53EC-484E-BB8A-0E1B5551E35C}']
    property AudioStatus: ISpeechAudioStatus readonly dispid 1;
    property CurrentStreamPosition: OleVariant readonly dispid 2;
    property CurrentStreamNumber: Integer readonly dispid 3;
    property NumberOfActiveRules: Integer readonly dispid 4;
    property ClsidEngine: WideString readonly dispid 5;
    property SupportedLanguages: OleVariant readonly dispid 6;
  end;

// *********************************************************************//
// Interface: ISpeechRecoContext
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {580AA49D-7E1E-4809-B8E2-57DA806104B8}
// *********************************************************************//
  ISpeechRecoContext = interface(IDispatch)
    ['{580AA49D-7E1E-4809-B8E2-57DA806104B8}']
    function Get_Recognizer: ISpeechRecognizer; safecall;
    function Get_AudioInputInterferenceStatus: SpeechInterference; safecall;
    function Get_RequestedUIType: WideString; safecall;
    procedure _Set_Voice(const Voice: ISpeechVoice); safecall;
    function Get_Voice: ISpeechVoice; safecall;
    procedure Set_AllowVoiceFormatMatchingOnNextSet(pAllow: WordBool); safecall;
    function Get_AllowVoiceFormatMatchingOnNextSet: WordBool; safecall;
    procedure Set_VoicePurgeEvent(EventInterest: SpeechRecoEvents); safecall;
    function Get_VoicePurgeEvent: SpeechRecoEvents; safecall;
    procedure Set_EventInterests(EventInterest: SpeechRecoEvents); safecall;
    function Get_EventInterests: SpeechRecoEvents; safecall;
    procedure Set_CmdMaxAlternates(MaxAlternates: Integer); safecall;
    function Get_CmdMaxAlternates: Integer; safecall;
    procedure Set_State(State: SpeechRecoContextState); safecall;
    function Get_State: SpeechRecoContextState; safecall;
    procedure Set_RetainedAudio(Option: SpeechRetainedAudioOptions); safecall;
    function Get_RetainedAudio: SpeechRetainedAudioOptions; safecall;
    procedure _Set_RetainedAudioFormat(const Format: ISpeechAudioFormat); safecall;
    function Get_RetainedAudioFormat: ISpeechAudioFormat; safecall;
    procedure Pause; safecall;
    procedure Resume; safecall;
    function CreateGrammar(GrammarId: OleVariant): ISpeechRecoGrammar; safecall;
    function CreateResultFromMemory(const ResultBlock: OleVariant): ISpeechRecoResult; safecall;
    procedure Bookmark(Options: SpeechBookmarkOptions; StreamPos: OleVariant; BookmarkId: OleVariant); safecall;
    procedure SetAdaptationData(const AdaptationString: WideString); safecall;
    property Recognizer: ISpeechRecognizer read Get_Recognizer;
    property AudioInputInterferenceStatus: SpeechInterference read Get_AudioInputInterferenceStatus;
    property RequestedUIType: WideString read Get_RequestedUIType;
    property Voice: ISpeechVoice read Get_Voice write _Set_Voice;
    property AllowVoiceFormatMatchingOnNextSet: WordBool read Get_AllowVoiceFormatMatchingOnNextSet write Set_AllowVoiceFormatMatchingOnNextSet;
    property VoicePurgeEvent: SpeechRecoEvents read Get_VoicePurgeEvent write Set_VoicePurgeEvent;
    property EventInterests: SpeechRecoEvents read Get_EventInterests write Set_EventInterests;
    property CmdMaxAlternates: Integer read Get_CmdMaxAlternates write Set_CmdMaxAlternates;
    property State: SpeechRecoContextState read Get_State write Set_State;
    property RetainedAudio: SpeechRetainedAudioOptions read Get_RetainedAudio write Set_RetainedAudio;
    property RetainedAudioFormat: ISpeechAudioFormat read Get_RetainedAudioFormat write _Set_RetainedAudioFormat;
  end;

// *********************************************************************//
// DispIntf:  ISpeechRecoContextDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {580AA49D-7E1E-4809-B8E2-57DA806104B8}
// *********************************************************************//
  ISpeechRecoContextDisp = dispinterface
    ['{580AA49D-7E1E-4809-B8E2-57DA806104B8}']
    property Recognizer: ISpeechRecognizer readonly dispid 1;
    property AudioInputInterferenceStatus: SpeechInterference readonly dispid 2;
    property RequestedUIType: WideString readonly dispid 3;
    property Voice: ISpeechVoice dispid 4;
    property AllowVoiceFormatMatchingOnNextSet: WordBool dispid 5;
    property VoicePurgeEvent: SpeechRecoEvents dispid 6;
    property EventInterests: SpeechRecoEvents dispid 7;
    property CmdMaxAlternates: Integer dispid 8;
    property State: SpeechRecoContextState dispid 9;
    property RetainedAudio: SpeechRetainedAudioOptions dispid 10;
    property RetainedAudioFormat: ISpeechAudioFormat dispid 11;
    procedure Pause; dispid 12;
    procedure Resume; dispid 13;
    function CreateGrammar(GrammarId: OleVariant): ISpeechRecoGrammar; dispid 14;
    function CreateResultFromMemory(const ResultBlock: OleVariant): ISpeechRecoResult; dispid 15;
    procedure Bookmark(Options: SpeechBookmarkOptions; StreamPos: OleVariant; BookmarkId: OleVariant); dispid 16;
    procedure SetAdaptationData(const AdaptationString: WideString); dispid 17;
  end;

// *********************************************************************//
// Interface: ISpeechRecoGrammar
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B6D6F79F-2158-4E50-B5BC-9A9CCD852A09}
// *********************************************************************//
  ISpeechRecoGrammar = interface(IDispatch)
    ['{B6D6F79F-2158-4E50-B5BC-9A9CCD852A09}']
    function Get_Id: OleVariant; safecall;
    function Get_RecoContext: ISpeechRecoContext; safecall;
    procedure Set_State(State: SpeechGrammarState); safecall;
    function Get_State: SpeechGrammarState; safecall;
    function Get_Rules: ISpeechGrammarRules; safecall;
    procedure Reset(NewLanguage: Integer); safecall;
    procedure CmdLoadFromFile(const FileName: WideString; LoadOption: SpeechLoadOption); safecall;
    procedure CmdLoadFromObject(const ClassId: WideString; const GrammarName: WideString; 
                                LoadOption: SpeechLoadOption); safecall;
    procedure CmdLoadFromResource(hModule: Integer; ResourceName: OleVariant; 
                                  ResourceType: OleVariant; LanguageId: Integer; 
                                  LoadOption: SpeechLoadOption); safecall;
    procedure CmdLoadFromMemory(GrammarData: OleVariant; LoadOption: SpeechLoadOption); safecall;
    procedure CmdLoadFromProprietaryGrammar(const ProprietaryGuid: WideString; 
                                            const ProprietaryString: WideString; 
                                            ProprietaryData: OleVariant; 
                                            LoadOption: SpeechLoadOption); safecall;
    procedure CmdSetRuleState(const Name: WideString; State: SpeechRuleState); safecall;
    procedure CmdSetRuleIdState(RuleId: Integer; State: SpeechRuleState); safecall;
    procedure DictationLoad(const TopicName: WideString; LoadOption: SpeechLoadOption); safecall;
    procedure DictationUnload; safecall;
    procedure DictationSetState(State: SpeechRuleState); safecall;
    procedure SetWordSequenceData(const Text: WideString; TextLength: Integer; 
                                  const Info: ISpeechTextSelectionInformation); safecall;
    procedure SetTextSelection(const Info: ISpeechTextSelectionInformation); safecall;
    function IsPronounceable(const Word: WideString): SpeechWordPronounceable; safecall;
    property Id: OleVariant read Get_Id;
    property RecoContext: ISpeechRecoContext read Get_RecoContext;
    property State: SpeechGrammarState read Get_State write Set_State;
    property Rules: ISpeechGrammarRules read Get_Rules;
  end;

// *********************************************************************//
// DispIntf:  ISpeechRecoGrammarDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B6D6F79F-2158-4E50-B5BC-9A9CCD852A09}
// *********************************************************************//
  ISpeechRecoGrammarDisp = dispinterface
    ['{B6D6F79F-2158-4E50-B5BC-9A9CCD852A09}']
    property Id: OleVariant readonly dispid 1;
    property RecoContext: ISpeechRecoContext readonly dispid 2;
    property State: SpeechGrammarState dispid 3;
    property Rules: ISpeechGrammarRules readonly dispid 4;
    procedure Reset(NewLanguage: Integer); dispid 5;
    procedure CmdLoadFromFile(const FileName: WideString; LoadOption: SpeechLoadOption); dispid 7;
    procedure CmdLoadFromObject(const ClassId: WideString; const GrammarName: WideString; 
                                LoadOption: SpeechLoadOption); dispid 8;
    procedure CmdLoadFromResource(hModule: Integer; ResourceName: OleVariant; 
                                  ResourceType: OleVariant; LanguageId: Integer; 
                                  LoadOption: SpeechLoadOption); dispid 9;
    procedure CmdLoadFromMemory(GrammarData: OleVariant; LoadOption: SpeechLoadOption); dispid 10;
    procedure CmdLoadFromProprietaryGrammar(const ProprietaryGuid: WideString; 
                                            const ProprietaryString: WideString; 
                                            ProprietaryData: OleVariant; 
                                            LoadOption: SpeechLoadOption); dispid 11;
    procedure CmdSetRuleState(const Name: WideString; State: SpeechRuleState); dispid 12;
    procedure CmdSetRuleIdState(RuleId: Integer; State: SpeechRuleState); dispid 13;
    procedure DictationLoad(const TopicName: WideString; LoadOption: SpeechLoadOption); dispid 14;
    procedure DictationUnload; dispid 15;
    procedure DictationSetState(State: SpeechRuleState); dispid 16;
    procedure SetWordSequenceData(const Text: WideString; TextLength: Integer; 
                                  const Info: ISpeechTextSelectionInformation); dispid 17;
    procedure SetTextSelection(const Info: ISpeechTextSelectionInformation); dispid 18;
    function IsPronounceable(const Word: WideString): SpeechWordPronounceable; dispid 19;
  end;

// *********************************************************************//
// Interface: ISpeechGrammarRules
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6FFA3B44-FC2D-40D1-8AFC-32911C7F1AD1}
// *********************************************************************//
  ISpeechGrammarRules = interface(IDispatch)
    ['{6FFA3B44-FC2D-40D1-8AFC-32911C7F1AD1}']
    function Get_Count: Integer; safecall;
    function FindRule(RuleNameOrId: OleVariant): ISpeechGrammarRule; safecall;
    function Item(Index: Integer): ISpeechGrammarRule; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Dynamic: WordBool; safecall;
    function Add(const RuleName: WideString; Attributes: SpeechRuleAttributes; RuleId: Integer): ISpeechGrammarRule; safecall;
    procedure Commit; safecall;
    function CommitAndSave(out ErrorText: WideString): OleVariant; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Dynamic: WordBool read Get_Dynamic;
  end;

// *********************************************************************//
// DispIntf:  ISpeechGrammarRulesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6FFA3B44-FC2D-40D1-8AFC-32911C7F1AD1}
// *********************************************************************//
  ISpeechGrammarRulesDisp = dispinterface
    ['{6FFA3B44-FC2D-40D1-8AFC-32911C7F1AD1}']
    property Count: Integer readonly dispid 1;
    function FindRule(RuleNameOrId: OleVariant): ISpeechGrammarRule; dispid 6;
    function Item(Index: Integer): ISpeechGrammarRule; dispid 0;
    property _NewEnum: IUnknown readonly dispid -4;
    property Dynamic: WordBool readonly dispid 2;
    function Add(const RuleName: WideString; Attributes: SpeechRuleAttributes; RuleId: Integer): ISpeechGrammarRule; dispid 3;
    procedure Commit; dispid 4;
    function CommitAndSave(out ErrorText: WideString): OleVariant; dispid 5;
  end;

// *********************************************************************//
// Interface: ISpeechGrammarRule
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AFE719CF-5DD1-44F2-999C-7A399F1CFCCC}
// *********************************************************************//
  ISpeechGrammarRule = interface(IDispatch)
    ['{AFE719CF-5DD1-44F2-999C-7A399F1CFCCC}']
    function Get_Attributes: SpeechRuleAttributes; safecall;
    function Get_InitialState: ISpeechGrammarRuleState; safecall;
    function Get_Name: WideString; safecall;
    function Get_Id: Integer; safecall;
    procedure Clear; safecall;
    procedure AddResource(const ResourceName: WideString; const ResourceValue: WideString); safecall;
    function AddState: ISpeechGrammarRuleState; safecall;
    property Attributes: SpeechRuleAttributes read Get_Attributes;
    property InitialState: ISpeechGrammarRuleState read Get_InitialState;
    property Name: WideString read Get_Name;
    property Id: Integer read Get_Id;
  end;

// *********************************************************************//
// DispIntf:  ISpeechGrammarRuleDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AFE719CF-5DD1-44F2-999C-7A399F1CFCCC}
// *********************************************************************//
  ISpeechGrammarRuleDisp = dispinterface
    ['{AFE719CF-5DD1-44F2-999C-7A399F1CFCCC}']
    property Attributes: SpeechRuleAttributes readonly dispid 1;
    property InitialState: ISpeechGrammarRuleState readonly dispid 2;
    property Name: WideString readonly dispid 3;
    property Id: Integer readonly dispid 4;
    procedure Clear; dispid 5;
    procedure AddResource(const ResourceName: WideString; const ResourceValue: WideString); dispid 6;
    function AddState: ISpeechGrammarRuleState; dispid 7;
  end;

// *********************************************************************//
// Interface: ISpeechGrammarRuleState
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D4286F2C-EE67-45AE-B928-28D695362EDA}
// *********************************************************************//
  ISpeechGrammarRuleState = interface(IDispatch)
    ['{D4286F2C-EE67-45AE-B928-28D695362EDA}']
    function Get_Rule: ISpeechGrammarRule; safecall;
    function Get_Transitions: ISpeechGrammarRuleStateTransitions; safecall;
    procedure AddWordTransition(const DestState: ISpeechGrammarRuleState; const Words: WideString; 
                                const Separators: WideString; Type_: SpeechGrammarWordType; 
                                const PropertyName: WideString; PropertyId: Integer; 
                                const PropertyValue: OleVariant; Weight: Single); safecall;
    procedure AddRuleTransition(const DestinationState: ISpeechGrammarRuleState; 
                                const Rule: ISpeechGrammarRule; const PropertyName: WideString; 
                                PropertyId: Integer; const PropertyValue: OleVariant; Weight: Single); safecall;
    procedure AddSpecialTransition(const DestinationState: ISpeechGrammarRuleState; 
                                   Type_: SpeechSpecialTransitionType; 
                                   const PropertyName: WideString; PropertyId: Integer; 
                                   const PropertyValue: OleVariant; Weight: Single); safecall;
    property Rule: ISpeechGrammarRule read Get_Rule;
    property Transitions: ISpeechGrammarRuleStateTransitions read Get_Transitions;
  end;

// *********************************************************************//
// DispIntf:  ISpeechGrammarRuleStateDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D4286F2C-EE67-45AE-B928-28D695362EDA}
// *********************************************************************//
  ISpeechGrammarRuleStateDisp = dispinterface
    ['{D4286F2C-EE67-45AE-B928-28D695362EDA}']
    property Rule: ISpeechGrammarRule readonly dispid 1;
    property Transitions: ISpeechGrammarRuleStateTransitions readonly dispid 2;
    procedure AddWordTransition(const DestState: ISpeechGrammarRuleState; const Words: WideString; 
                                const Separators: WideString; Type_: SpeechGrammarWordType; 
                                const PropertyName: WideString; PropertyId: Integer; 
                                const PropertyValue: OleVariant; Weight: Single); dispid 3;
    procedure AddRuleTransition(const DestinationState: ISpeechGrammarRuleState; 
                                const Rule: ISpeechGrammarRule; const PropertyName: WideString; 
                                PropertyId: Integer; const PropertyValue: OleVariant; Weight: Single); dispid 4;
    procedure AddSpecialTransition(const DestinationState: ISpeechGrammarRuleState; 
                                   Type_: SpeechSpecialTransitionType; 
                                   const PropertyName: WideString; PropertyId: Integer; 
                                   const PropertyValue: OleVariant; Weight: Single); dispid 5;
  end;

// *********************************************************************//
// Interface: ISpeechGrammarRuleStateTransitions
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EABCE657-75BC-44A2-AA7F-C56476742963}
// *********************************************************************//
  ISpeechGrammarRuleStateTransitions = interface(IDispatch)
    ['{EABCE657-75BC-44A2-AA7F-C56476742963}']
    function Get_Count: Integer; safecall;
    function Item(Index: Integer): ISpeechGrammarRuleStateTransition; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISpeechGrammarRuleStateTransitionsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EABCE657-75BC-44A2-AA7F-C56476742963}
// *********************************************************************//
  ISpeechGrammarRuleStateTransitionsDisp = dispinterface
    ['{EABCE657-75BC-44A2-AA7F-C56476742963}']
    property Count: Integer readonly dispid 1;
    function Item(Index: Integer): ISpeechGrammarRuleStateTransition; dispid 0;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISpeechGrammarRuleStateTransition
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CAFD1DB1-41D1-4A06-9863-E2E81DA17A9A}
// *********************************************************************//
  ISpeechGrammarRuleStateTransition = interface(IDispatch)
    ['{CAFD1DB1-41D1-4A06-9863-E2E81DA17A9A}']
    function Get_type_: SpeechGrammarRuleStateTransitionType; safecall;
    function Get_Text: WideString; safecall;
    function Get_Rule: ISpeechGrammarRule; safecall;
    function Get_Weight: OleVariant; safecall;
    function Get_PropertyName: WideString; safecall;
    function Get_PropertyId: Integer; safecall;
    function Get_PropertyValue: OleVariant; safecall;
    function Get_NextState: ISpeechGrammarRuleState; safecall;
    property type_: SpeechGrammarRuleStateTransitionType read Get_type_;
    property Text: WideString read Get_Text;
    property Rule: ISpeechGrammarRule read Get_Rule;
    property Weight: OleVariant read Get_Weight;
    property PropertyName: WideString read Get_PropertyName;
    property PropertyId: Integer read Get_PropertyId;
    property PropertyValue: OleVariant read Get_PropertyValue;
    property NextState: ISpeechGrammarRuleState read Get_NextState;
  end;

// *********************************************************************//
// DispIntf:  ISpeechGrammarRuleStateTransitionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CAFD1DB1-41D1-4A06-9863-E2E81DA17A9A}
// *********************************************************************//
  ISpeechGrammarRuleStateTransitionDisp = dispinterface
    ['{CAFD1DB1-41D1-4A06-9863-E2E81DA17A9A}']
    property type_: SpeechGrammarRuleStateTransitionType readonly dispid 1;
    property Text: WideString readonly dispid 2;
    property Rule: ISpeechGrammarRule readonly dispid 3;
    property Weight: OleVariant readonly dispid 4;
    property PropertyName: WideString readonly dispid 5;
    property PropertyId: Integer readonly dispid 6;
    property PropertyValue: OleVariant readonly dispid 7;
    property NextState: ISpeechGrammarRuleState readonly dispid 8;
  end;

// *********************************************************************//
// Interface: ISpeechTextSelectionInformation
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3B9C7E7A-6EEE-4DED-9092-11657279ADBE}
// *********************************************************************//
  ISpeechTextSelectionInformation = interface(IDispatch)
    ['{3B9C7E7A-6EEE-4DED-9092-11657279ADBE}']
    procedure Set_ActiveOffset(ActiveOffset: Integer); safecall;
    function Get_ActiveOffset: Integer; safecall;
    procedure Set_ActiveLength(ActiveLength: Integer); safecall;
    function Get_ActiveLength: Integer; safecall;
    procedure Set_SelectionOffset(SelectionOffset: Integer); safecall;
    function Get_SelectionOffset: Integer; safecall;
    procedure Set_SelectionLength(SelectionLength: Integer); safecall;
    function Get_SelectionLength: Integer; safecall;
    property ActiveOffset: Integer read Get_ActiveOffset write Set_ActiveOffset;
    property ActiveLength: Integer read Get_ActiveLength write Set_ActiveLength;
    property SelectionOffset: Integer read Get_SelectionOffset write Set_SelectionOffset;
    property SelectionLength: Integer read Get_SelectionLength write Set_SelectionLength;
  end;

// *********************************************************************//
// DispIntf:  ISpeechTextSelectionInformationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3B9C7E7A-6EEE-4DED-9092-11657279ADBE}
// *********************************************************************//
  ISpeechTextSelectionInformationDisp = dispinterface
    ['{3B9C7E7A-6EEE-4DED-9092-11657279ADBE}']
    property ActiveOffset: Integer dispid 1;
    property ActiveLength: Integer dispid 2;
    property SelectionOffset: Integer dispid 3;
    property SelectionLength: Integer dispid 4;
  end;

// *********************************************************************//
// Interface: ISpeechRecoResult
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ED2879CF-CED9-4EE6-A534-DE0191D5468D}
// *********************************************************************//
  ISpeechRecoResult = interface(IDispatch)
    ['{ED2879CF-CED9-4EE6-A534-DE0191D5468D}']
    function Get_RecoContext: ISpeechRecoContext; safecall;
    function Get_Times: ISpeechRecoResultTimes; safecall;
    procedure _Set_AudioFormat(const Format: ISpeechAudioFormat); safecall;
    function Get_AudioFormat: ISpeechAudioFormat; safecall;
    function Get_PhraseInfo: ISpeechPhraseInfo; safecall;
    function Alternates(RequestCount: Integer; StartElement: Integer; Elements: Integer): ISpeechPhraseAlternates; safecall;
    function Audio(StartElement: Integer; Elements: Integer): ISpeechMemoryStream; safecall;
    function SpeakAudio(StartElement: Integer; Elements: Integer; Flags: SpeechVoiceSpeakFlags): Integer; safecall;
    function SaveToMemory: OleVariant; safecall;
    procedure DiscardResultInfo(ValueTypes: SpeechDiscardType); safecall;
    property RecoContext: ISpeechRecoContext read Get_RecoContext;
    property Times: ISpeechRecoResultTimes read Get_Times;
    property AudioFormat: ISpeechAudioFormat read Get_AudioFormat write _Set_AudioFormat;
    property PhraseInfo: ISpeechPhraseInfo read Get_PhraseInfo;
  end;

// *********************************************************************//
// DispIntf:  ISpeechRecoResultDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ED2879CF-CED9-4EE6-A534-DE0191D5468D}
// *********************************************************************//
  ISpeechRecoResultDisp = dispinterface
    ['{ED2879CF-CED9-4EE6-A534-DE0191D5468D}']
    property RecoContext: ISpeechRecoContext readonly dispid 1;
    property Times: ISpeechRecoResultTimes readonly dispid 2;
    property AudioFormat: ISpeechAudioFormat dispid 3;
    property PhraseInfo: ISpeechPhraseInfo readonly dispid 4;
    function Alternates(RequestCount: Integer; StartElement: Integer; Elements: Integer): ISpeechPhraseAlternates; dispid 5;
    function Audio(StartElement: Integer; Elements: Integer): ISpeechMemoryStream; dispid 6;
    function SpeakAudio(StartElement: Integer; Elements: Integer; Flags: SpeechVoiceSpeakFlags): Integer; dispid 7;
    function SaveToMemory: OleVariant; dispid 8;
    procedure DiscardResultInfo(ValueTypes: SpeechDiscardType); dispid 9;
  end;

// *********************************************************************//
// Interface: ISpeechRecoResultTimes
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {62B3B8FB-F6E7-41BE-BDCB-056B1C29EFC0}
// *********************************************************************//
  ISpeechRecoResultTimes = interface(IDispatch)
    ['{62B3B8FB-F6E7-41BE-BDCB-056B1C29EFC0}']
    function Get_StreamTime: OleVariant; safecall;
    function Get_Length: OleVariant; safecall;
    function Get_TickCount: Integer; safecall;
    function Get_OffsetFromStart: OleVariant; safecall;
    property StreamTime: OleVariant read Get_StreamTime;
    property Length: OleVariant read Get_Length;
    property TickCount: Integer read Get_TickCount;
    property OffsetFromStart: OleVariant read Get_OffsetFromStart;
  end;

// *********************************************************************//
// DispIntf:  ISpeechRecoResultTimesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {62B3B8FB-F6E7-41BE-BDCB-056B1C29EFC0}
// *********************************************************************//
  ISpeechRecoResultTimesDisp = dispinterface
    ['{62B3B8FB-F6E7-41BE-BDCB-056B1C29EFC0}']
    property StreamTime: OleVariant readonly dispid 1;
    property Length: OleVariant readonly dispid 2;
    property TickCount: Integer readonly dispid 3;
    property OffsetFromStart: OleVariant readonly dispid 4;
  end;

// *********************************************************************//
// Interface: ISpeechPhraseInfo
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {961559CF-4E67-4662-8BF0-D93F1FCD61B3}
// *********************************************************************//
  ISpeechPhraseInfo = interface(IDispatch)
    ['{961559CF-4E67-4662-8BF0-D93F1FCD61B3}']
    function Get_LanguageId: Integer; safecall;
    function Get_GrammarId: OleVariant; safecall;
    function Get_StartTime: OleVariant; safecall;
    function Get_AudioStreamPosition: OleVariant; safecall;
    function Get_AudioSizeBytes: Integer; safecall;
    function Get_RetainedSizeBytes: Integer; safecall;
    function Get_AudioSizeTime: Integer; safecall;
    function Get_Rule: ISpeechPhraseRule; safecall;
    function Get_Properties: ISpeechPhraseProperties; safecall;
    function Get_Elements: ISpeechPhraseElements; safecall;
    function Get_Replacements: ISpeechPhraseReplacements; safecall;
    function Get_EngineId: WideString; safecall;
    function Get_EnginePrivateData: OleVariant; safecall;
    function SaveToMemory: OleVariant; safecall;
    function GetText(StartElement: Integer; Elements: Integer; UseReplacements: WordBool): WideString; safecall;
    function GetDisplayAttributes(StartElement: Integer; Elements: Integer; 
                                  UseReplacements: WordBool): SpeechDisplayAttributes; safecall;
    property LanguageId: Integer read Get_LanguageId;
    property GrammarId: OleVariant read Get_GrammarId;
    property StartTime: OleVariant read Get_StartTime;
    property AudioStreamPosition: OleVariant read Get_AudioStreamPosition;
    property AudioSizeBytes: Integer read Get_AudioSizeBytes;
    property RetainedSizeBytes: Integer read Get_RetainedSizeBytes;
    property AudioSizeTime: Integer read Get_AudioSizeTime;
    property Rule: ISpeechPhraseRule read Get_Rule;
    property Properties: ISpeechPhraseProperties read Get_Properties;
    property Elements: ISpeechPhraseElements read Get_Elements;
    property Replacements: ISpeechPhraseReplacements read Get_Replacements;
    property EngineId: WideString read Get_EngineId;
    property EnginePrivateData: OleVariant read Get_EnginePrivateData;
  end;

// *********************************************************************//
// DispIntf:  ISpeechPhraseInfoDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {961559CF-4E67-4662-8BF0-D93F1FCD61B3}
// *********************************************************************//
  ISpeechPhraseInfoDisp = dispinterface
    ['{961559CF-4E67-4662-8BF0-D93F1FCD61B3}']
    property LanguageId: Integer readonly dispid 1;
    property GrammarId: OleVariant readonly dispid 2;
    property StartTime: OleVariant readonly dispid 3;
    property AudioStreamPosition: OleVariant readonly dispid 4;
    property AudioSizeBytes: Integer readonly dispid 5;
    property RetainedSizeBytes: Integer readonly dispid 6;
    property AudioSizeTime: Integer readonly dispid 7;
    property Rule: ISpeechPhraseRule readonly dispid 8;
    property Properties: ISpeechPhraseProperties readonly dispid 9;
    property Elements: ISpeechPhraseElements readonly dispid 10;
    property Replacements: ISpeechPhraseReplacements readonly dispid 11;
    property EngineId: WideString readonly dispid 12;
    property EnginePrivateData: OleVariant readonly dispid 13;
    function SaveToMemory: OleVariant; dispid 14;
    function GetText(StartElement: Integer; Elements: Integer; UseReplacements: WordBool): WideString; dispid 15;
    function GetDisplayAttributes(StartElement: Integer; Elements: Integer; 
                                  UseReplacements: WordBool): SpeechDisplayAttributes; dispid 16;
  end;

// *********************************************************************//
// Interface: ISpeechPhraseRule
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7BFE112-A4A0-48D9-B602-C313843F6964}
// *********************************************************************//
  ISpeechPhraseRule = interface(IDispatch)
    ['{A7BFE112-A4A0-48D9-B602-C313843F6964}']
    function Get_Name: WideString; safecall;
    function Get_Id: Integer; safecall;
    function Get_FirstElement: Integer; safecall;
    function Get_NumberOfElements: Integer; safecall;
    function Get_Parent: ISpeechPhraseRule; safecall;
    function Get_Children: ISpeechPhraseRules; safecall;
    function Get_Confidence: SpeechEngineConfidence; safecall;
    function Get_EngineConfidence: Single; safecall;
    property Name: WideString read Get_Name;
    property Id: Integer read Get_Id;
    property FirstElement: Integer read Get_FirstElement;
    property NumberOfElements: Integer read Get_NumberOfElements;
    property Parent: ISpeechPhraseRule read Get_Parent;
    property Children: ISpeechPhraseRules read Get_Children;
    property Confidence: SpeechEngineConfidence read Get_Confidence;
    property EngineConfidence: Single read Get_EngineConfidence;
  end;

// *********************************************************************//
// DispIntf:  ISpeechPhraseRuleDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7BFE112-A4A0-48D9-B602-C313843F6964}
// *********************************************************************//
  ISpeechPhraseRuleDisp = dispinterface
    ['{A7BFE112-A4A0-48D9-B602-C313843F6964}']
    property Name: WideString readonly dispid 1;
    property Id: Integer readonly dispid 2;
    property FirstElement: Integer readonly dispid 3;
    property NumberOfElements: Integer readonly dispid 4;
    property Parent: ISpeechPhraseRule readonly dispid 5;
    property Children: ISpeechPhraseRules readonly dispid 6;
    property Confidence: SpeechEngineConfidence readonly dispid 7;
    property EngineConfidence: Single readonly dispid 8;
  end;

// *********************************************************************//
// Interface: ISpeechPhraseRules
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9047D593-01DD-4B72-81A3-E4A0CA69F407}
// *********************************************************************//
  ISpeechPhraseRules = interface(IDispatch)
    ['{9047D593-01DD-4B72-81A3-E4A0CA69F407}']
    function Get_Count: Integer; safecall;
    function Item(Index: Integer): ISpeechPhraseRule; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISpeechPhraseRulesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9047D593-01DD-4B72-81A3-E4A0CA69F407}
// *********************************************************************//
  ISpeechPhraseRulesDisp = dispinterface
    ['{9047D593-01DD-4B72-81A3-E4A0CA69F407}']
    property Count: Integer readonly dispid 1;
    function Item(Index: Integer): ISpeechPhraseRule; dispid 0;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISpeechPhraseProperties
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {08166B47-102E-4B23-A599-BDB98DBFD1F4}
// *********************************************************************//
  ISpeechPhraseProperties = interface(IDispatch)
    ['{08166B47-102E-4B23-A599-BDB98DBFD1F4}']
    function Get_Count: Integer; safecall;
    function Item(Index: Integer): ISpeechPhraseProperty; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISpeechPhrasePropertiesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {08166B47-102E-4B23-A599-BDB98DBFD1F4}
// *********************************************************************//
  ISpeechPhrasePropertiesDisp = dispinterface
    ['{08166B47-102E-4B23-A599-BDB98DBFD1F4}']
    property Count: Integer readonly dispid 1;
    function Item(Index: Integer): ISpeechPhraseProperty; dispid 0;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISpeechPhraseProperty
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CE563D48-961E-4732-A2E1-378A42B430BE}
// *********************************************************************//
  ISpeechPhraseProperty = interface(IDispatch)
    ['{CE563D48-961E-4732-A2E1-378A42B430BE}']
    function Get_Name: WideString; safecall;
    function Get_Id: Integer; safecall;
    function Get_Value: OleVariant; safecall;
    function Get_FirstElement: Integer; safecall;
    function Get_NumberOfElements: Integer; safecall;
    function Get_EngineConfidence: Single; safecall;
    function Get_Confidence: SpeechEngineConfidence; safecall;
    function Get_Parent: ISpeechPhraseProperty; safecall;
    function Get_Children: ISpeechPhraseProperties; safecall;
    property Name: WideString read Get_Name;
    property Id: Integer read Get_Id;
    property Value: OleVariant read Get_Value;
    property FirstElement: Integer read Get_FirstElement;
    property NumberOfElements: Integer read Get_NumberOfElements;
    property EngineConfidence: Single read Get_EngineConfidence;
    property Confidence: SpeechEngineConfidence read Get_Confidence;
    property Parent: ISpeechPhraseProperty read Get_Parent;
    property Children: ISpeechPhraseProperties read Get_Children;
  end;

// *********************************************************************//
// DispIntf:  ISpeechPhrasePropertyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CE563D48-961E-4732-A2E1-378A42B430BE}
// *********************************************************************//
  ISpeechPhrasePropertyDisp = dispinterface
    ['{CE563D48-961E-4732-A2E1-378A42B430BE}']
    property Name: WideString readonly dispid 1;
    property Id: Integer readonly dispid 2;
    property Value: OleVariant readonly dispid 3;
    property FirstElement: Integer readonly dispid 4;
    property NumberOfElements: Integer readonly dispid 5;
    property EngineConfidence: Single readonly dispid 6;
    property Confidence: SpeechEngineConfidence readonly dispid 7;
    property Parent: ISpeechPhraseProperty readonly dispid 8;
    property Children: ISpeechPhraseProperties readonly dispid 9;
  end;

// *********************************************************************//
// Interface: ISpeechPhraseElements
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0626B328-3478-467D-A0B3-D0853B93DDA3}
// *********************************************************************//
  ISpeechPhraseElements = interface(IDispatch)
    ['{0626B328-3478-467D-A0B3-D0853B93DDA3}']
    function Get_Count: Integer; safecall;
    function Item(Index: Integer): ISpeechPhraseElement; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISpeechPhraseElementsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0626B328-3478-467D-A0B3-D0853B93DDA3}
// *********************************************************************//
  ISpeechPhraseElementsDisp = dispinterface
    ['{0626B328-3478-467D-A0B3-D0853B93DDA3}']
    property Count: Integer readonly dispid 1;
    function Item(Index: Integer): ISpeechPhraseElement; dispid 0;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISpeechPhraseElement
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E6176F96-E373-4801-B223-3B62C068C0B4}
// *********************************************************************//
  ISpeechPhraseElement = interface(IDispatch)
    ['{E6176F96-E373-4801-B223-3B62C068C0B4}']
    function Get_AudioTimeOffset: Integer; safecall;
    function Get_AudioSizeTime: Integer; safecall;
    function Get_AudioStreamOffset: Integer; safecall;
    function Get_AudioSizeBytes: Integer; safecall;
    function Get_RetainedStreamOffset: Integer; safecall;
    function Get_RetainedSizeBytes: Integer; safecall;
    function Get_DisplayText: WideString; safecall;
    function Get_LexicalForm: WideString; safecall;
    function Get_Pronunciation: OleVariant; safecall;
    function Get_DisplayAttributes: SpeechDisplayAttributes; safecall;
    function Get_RequiredConfidence: SpeechEngineConfidence; safecall;
    function Get_ActualConfidence: SpeechEngineConfidence; safecall;
    function Get_EngineConfidence: Single; safecall;
    property AudioTimeOffset: Integer read Get_AudioTimeOffset;
    property AudioSizeTime: Integer read Get_AudioSizeTime;
    property AudioStreamOffset: Integer read Get_AudioStreamOffset;
    property AudioSizeBytes: Integer read Get_AudioSizeBytes;
    property RetainedStreamOffset: Integer read Get_RetainedStreamOffset;
    property RetainedSizeBytes: Integer read Get_RetainedSizeBytes;
    property DisplayText: WideString read Get_DisplayText;
    property LexicalForm: WideString read Get_LexicalForm;
    property Pronunciation: OleVariant read Get_Pronunciation;
    property DisplayAttributes: SpeechDisplayAttributes read Get_DisplayAttributes;
    property RequiredConfidence: SpeechEngineConfidence read Get_RequiredConfidence;
    property ActualConfidence: SpeechEngineConfidence read Get_ActualConfidence;
    property EngineConfidence: Single read Get_EngineConfidence;
  end;

// *********************************************************************//
// DispIntf:  ISpeechPhraseElementDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E6176F96-E373-4801-B223-3B62C068C0B4}
// *********************************************************************//
  ISpeechPhraseElementDisp = dispinterface
    ['{E6176F96-E373-4801-B223-3B62C068C0B4}']
    property AudioTimeOffset: Integer readonly dispid 1;
    property AudioSizeTime: Integer readonly dispid 2;
    property AudioStreamOffset: Integer readonly dispid 3;
    property AudioSizeBytes: Integer readonly dispid 4;
    property RetainedStreamOffset: Integer readonly dispid 5;
    property RetainedSizeBytes: Integer readonly dispid 6;
    property DisplayText: WideString readonly dispid 7;
    property LexicalForm: WideString readonly dispid 8;
    property Pronunciation: OleVariant readonly dispid 9;
    property DisplayAttributes: SpeechDisplayAttributes readonly dispid 10;
    property RequiredConfidence: SpeechEngineConfidence readonly dispid 11;
    property ActualConfidence: SpeechEngineConfidence readonly dispid 12;
    property EngineConfidence: Single readonly dispid 13;
  end;

// *********************************************************************//
// Interface: ISpeechPhraseReplacements
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {38BC662F-2257-4525-959E-2069D2596C05}
// *********************************************************************//
  ISpeechPhraseReplacements = interface(IDispatch)
    ['{38BC662F-2257-4525-959E-2069D2596C05}']
    function Get_Count: Integer; safecall;
    function Item(Index: Integer): ISpeechPhraseReplacement; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISpeechPhraseReplacementsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {38BC662F-2257-4525-959E-2069D2596C05}
// *********************************************************************//
  ISpeechPhraseReplacementsDisp = dispinterface
    ['{38BC662F-2257-4525-959E-2069D2596C05}']
    property Count: Integer readonly dispid 1;
    function Item(Index: Integer): ISpeechPhraseReplacement; dispid 0;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISpeechPhraseReplacement
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2890A410-53A7-4FB5-94EC-06D4998E3D02}
// *********************************************************************//
  ISpeechPhraseReplacement = interface(IDispatch)
    ['{2890A410-53A7-4FB5-94EC-06D4998E3D02}']
    function Get_DisplayAttributes: SpeechDisplayAttributes; safecall;
    function Get_Text: WideString; safecall;
    function Get_FirstElement: Integer; safecall;
    function Get_NumberOfElements: Integer; safecall;
    property DisplayAttributes: SpeechDisplayAttributes read Get_DisplayAttributes;
    property Text: WideString read Get_Text;
    property FirstElement: Integer read Get_FirstElement;
    property NumberOfElements: Integer read Get_NumberOfElements;
  end;

// *********************************************************************//
// DispIntf:  ISpeechPhraseReplacementDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2890A410-53A7-4FB5-94EC-06D4998E3D02}
// *********************************************************************//
  ISpeechPhraseReplacementDisp = dispinterface
    ['{2890A410-53A7-4FB5-94EC-06D4998E3D02}']
    property DisplayAttributes: SpeechDisplayAttributes readonly dispid 1;
    property Text: WideString readonly dispid 2;
    property FirstElement: Integer readonly dispid 3;
    property NumberOfElements: Integer readonly dispid 4;
  end;

// *********************************************************************//
// Interface: ISpeechPhraseAlternates
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B238B6D5-F276-4C3D-A6C1-2974801C3CC2}
// *********************************************************************//
  ISpeechPhraseAlternates = interface(IDispatch)
    ['{B238B6D5-F276-4C3D-A6C1-2974801C3CC2}']
    function Get_Count: Integer; safecall;
    function Item(Index: Integer): ISpeechPhraseAlternate; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISpeechPhraseAlternatesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B238B6D5-F276-4C3D-A6C1-2974801C3CC2}
// *********************************************************************//
  ISpeechPhraseAlternatesDisp = dispinterface
    ['{B238B6D5-F276-4C3D-A6C1-2974801C3CC2}']
    property Count: Integer readonly dispid 1;
    function Item(Index: Integer): ISpeechPhraseAlternate; dispid 0;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISpeechPhraseAlternate
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {27864A2A-2B9F-4CB8-92D3-0D2722FD1E73}
// *********************************************************************//
  ISpeechPhraseAlternate = interface(IDispatch)
    ['{27864A2A-2B9F-4CB8-92D3-0D2722FD1E73}']
    function Get_RecoResult: ISpeechRecoResult; safecall;
    function Get_StartElementInResult: Integer; safecall;
    function Get_NumberOfElementsInResult: Integer; safecall;
    function Get_PhraseInfo: ISpeechPhraseInfo; safecall;
    procedure Commit; safecall;
    property RecoResult: ISpeechRecoResult read Get_RecoResult;
    property StartElementInResult: Integer read Get_StartElementInResult;
    property NumberOfElementsInResult: Integer read Get_NumberOfElementsInResult;
    property PhraseInfo: ISpeechPhraseInfo read Get_PhraseInfo;
  end;

// *********************************************************************//
// DispIntf:  ISpeechPhraseAlternateDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {27864A2A-2B9F-4CB8-92D3-0D2722FD1E73}
// *********************************************************************//
  ISpeechPhraseAlternateDisp = dispinterface
    ['{27864A2A-2B9F-4CB8-92D3-0D2722FD1E73}']
    property RecoResult: ISpeechRecoResult readonly dispid 1;
    property StartElementInResult: Integer readonly dispid 2;
    property NumberOfElementsInResult: Integer readonly dispid 3;
    property PhraseInfo: ISpeechPhraseInfo readonly dispid 4;
    procedure Commit; dispid 5;
  end;

// *********************************************************************//
// DispIntf:  _ISpeechRecoContextEvents
// Flags:     (4096) Dispatchable
// GUID:      {7B8FCB42-0E9D-4F00-A048-7B04D6179D3D}
// *********************************************************************//
  _ISpeechRecoContextEvents = dispinterface
    ['{7B8FCB42-0E9D-4F00-A048-7B04D6179D3D}']
    procedure StartStream(StreamNumber: Integer; StreamPosition: OleVariant); dispid 1;
    procedure EndStream(StreamNumber: Integer; StreamPosition: OleVariant; StreamReleased: WordBool); dispid 2;
    procedure Bookmark(StreamNumber: Integer; StreamPosition: OleVariant; BookmarkId: OleVariant; 
                       Options: SpeechBookmarkOptions); dispid 3;
    procedure SoundStart(StreamNumber: Integer; StreamPosition: OleVariant); dispid 4;
    procedure SoundEnd(StreamNumber: Integer; StreamPosition: OleVariant); dispid 5;
    procedure PhraseStart(StreamNumber: Integer; StreamPosition: OleVariant); dispid 6;
    procedure Recognition(StreamNumber: Integer; StreamPosition: OleVariant; 
                          RecognitionType: SpeechRecognitionType; const Result: ISpeechRecoResult); dispid 7;
    procedure Hypothesis(StreamNumber: Integer; StreamPosition: OleVariant; 
                         const Result: ISpeechRecoResult); dispid 8;
    procedure PropertyNumberChange(StreamNumber: Integer; StreamPosition: OleVariant; 
                                   const PropertyName: WideString; NewNumberValue: Integer); dispid 9;
    procedure PropertyStringChange(StreamNumber: Integer; StreamPosition: OleVariant; 
                                   const PropertyName: WideString; const NewStringValue: WideString); dispid 10;
    procedure FalseRecognition(StreamNumber: Integer; StreamPosition: OleVariant; 
                               const Result: ISpeechRecoResult); dispid 11;
    procedure Interference(StreamNumber: Integer; StreamPosition: OleVariant; 
                           Interference: SpeechInterference); dispid 12;
    procedure RequestUI(StreamNumber: Integer; StreamPosition: OleVariant; const UIType: WideString); dispid 13;
    procedure RecognizerStateChange(StreamNumber: Integer; StreamPosition: OleVariant; 
                                    NewState: SpeechRecognizerState); dispid 14;
    procedure Adaptation(StreamNumber: Integer; StreamPosition: OleVariant); dispid 15;
    procedure RecognitionForOtherContext(StreamNumber: Integer; StreamPosition: OleVariant); dispid 16;
    procedure AudioLevel(StreamNumber: Integer; StreamPosition: OleVariant; AudioLevel: Integer); dispid 17;
    procedure EnginePrivate(StreamNumber: Integer; StreamPosition: OleVariant; 
                            EngineData: OleVariant); dispid 18;
  end;

// *********************************************************************//
// Interface: ISpeechRecoResult2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E0A246D-D3C8-45DE-8657-04290C458C3C}
// *********************************************************************//
  ISpeechRecoResult2 = interface(ISpeechRecoResult)
    ['{8E0A246D-D3C8-45DE-8657-04290C458C3C}']
    procedure SetTextFeedback(const Feedback: WideString; WasSuccessful: WordBool); safecall;
  end;

// *********************************************************************//
// DispIntf:  ISpeechRecoResult2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E0A246D-D3C8-45DE-8657-04290C458C3C}
// *********************************************************************//
  ISpeechRecoResult2Disp = dispinterface
    ['{8E0A246D-D3C8-45DE-8657-04290C458C3C}']
    procedure SetTextFeedback(const Feedback: WideString; WasSuccessful: WordBool); dispid 12;
    property RecoContext: ISpeechRecoContext readonly dispid 1;
    property Times: ISpeechRecoResultTimes readonly dispid 2;
    property AudioFormat: ISpeechAudioFormat dispid 3;
    property PhraseInfo: ISpeechPhraseInfo readonly dispid 4;
    function Alternates(RequestCount: Integer; StartElement: Integer; Elements: Integer): ISpeechPhraseAlternates; dispid 5;
    function Audio(StartElement: Integer; Elements: Integer): ISpeechMemoryStream; dispid 6;
    function SpeakAudio(StartElement: Integer; Elements: Integer; Flags: SpeechVoiceSpeakFlags): Integer; dispid 7;
    function SaveToMemory: OleVariant; dispid 8;
    procedure DiscardResultInfo(ValueTypes: SpeechDiscardType); dispid 9;
  end;

// *********************************************************************//
// Interface: ISpeechLexicon
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3DA7627A-C7AE-4B23-8708-638C50362C25}
// *********************************************************************//
  ISpeechLexicon = interface(IDispatch)
    ['{3DA7627A-C7AE-4B23-8708-638C50362C25}']
    function Get_GenerationId: Integer; safecall;
    function GetWords(Flags: SpeechLexiconType; out GenerationId: Integer): ISpeechLexiconWords; safecall;
    procedure AddPronunciation(const bstrWord: WideString; LangId: Integer; 
                               PartOfSpeech: SpeechPartOfSpeech; const bstrPronunciation: WideString); safecall;
    procedure AddPronunciationByPhoneIds(const bstrWord: WideString; LangId: Integer; 
                                         PartOfSpeech: SpeechPartOfSpeech; 
                                         const PhoneIds: OleVariant); safecall;
    procedure RemovePronunciation(const bstrWord: WideString; LangId: Integer; 
                                  PartOfSpeech: SpeechPartOfSpeech; 
                                  const bstrPronunciation: WideString); safecall;
    procedure RemovePronunciationByPhoneIds(const bstrWord: WideString; LangId: Integer; 
                                            PartOfSpeech: SpeechPartOfSpeech; 
                                            const PhoneIds: OleVariant); safecall;
    function GetPronunciations(const bstrWord: WideString; LangId: Integer; 
                               TypeFlags: SpeechLexiconType): ISpeechLexiconPronunciations; safecall;
    function GetGenerationChange(var GenerationId: Integer): ISpeechLexiconWords; safecall;
    property GenerationId: Integer read Get_GenerationId;
  end;

// *********************************************************************//
// DispIntf:  ISpeechLexiconDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3DA7627A-C7AE-4B23-8708-638C50362C25}
// *********************************************************************//
  ISpeechLexiconDisp = dispinterface
    ['{3DA7627A-C7AE-4B23-8708-638C50362C25}']
    property GenerationId: Integer readonly dispid 1;
    function GetWords(Flags: SpeechLexiconType; out GenerationId: Integer): ISpeechLexiconWords; dispid 2;
    procedure AddPronunciation(const bstrWord: WideString; LangId: Integer; 
                               PartOfSpeech: SpeechPartOfSpeech; const bstrPronunciation: WideString); dispid 3;
    procedure AddPronunciationByPhoneIds(const bstrWord: WideString; LangId: Integer; 
                                         PartOfSpeech: SpeechPartOfSpeech; 
                                         const PhoneIds: OleVariant); dispid 4;
    procedure RemovePronunciation(const bstrWord: WideString; LangId: Integer; 
                                  PartOfSpeech: SpeechPartOfSpeech; 
                                  const bstrPronunciation: WideString); dispid 5;
    procedure RemovePronunciationByPhoneIds(const bstrWord: WideString; LangId: Integer; 
                                            PartOfSpeech: SpeechPartOfSpeech; 
                                            const PhoneIds: OleVariant); dispid 6;
    function GetPronunciations(const bstrWord: WideString; LangId: Integer; 
                               TypeFlags: SpeechLexiconType): ISpeechLexiconPronunciations; dispid 7;
    function GetGenerationChange(var GenerationId: Integer): ISpeechLexiconWords; dispid 8;
  end;

// *********************************************************************//
// Interface: ISpeechLexiconWords
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8D199862-415E-47D5-AC4F-FAA608B424E6}
// *********************************************************************//
  ISpeechLexiconWords = interface(IDispatch)
    ['{8D199862-415E-47D5-AC4F-FAA608B424E6}']
    function Get_Count: Integer; safecall;
    function Item(Index: Integer): ISpeechLexiconWord; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISpeechLexiconWordsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8D199862-415E-47D5-AC4F-FAA608B424E6}
// *********************************************************************//
  ISpeechLexiconWordsDisp = dispinterface
    ['{8D199862-415E-47D5-AC4F-FAA608B424E6}']
    property Count: Integer readonly dispid 1;
    function Item(Index: Integer): ISpeechLexiconWord; dispid 0;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISpeechLexiconWord
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E5B933C-C9BE-48ED-8842-1EE51BB1D4FF}
// *********************************************************************//
  ISpeechLexiconWord = interface(IDispatch)
    ['{4E5B933C-C9BE-48ED-8842-1EE51BB1D4FF}']
    function Get_LangId: Integer; safecall;
    function Get_type_: SpeechWordType; safecall;
    function Get_Word: WideString; safecall;
    function Get_Pronunciations: ISpeechLexiconPronunciations; safecall;
    property LangId: Integer read Get_LangId;
    property type_: SpeechWordType read Get_type_;
    property Word: WideString read Get_Word;
    property Pronunciations: ISpeechLexiconPronunciations read Get_Pronunciations;
  end;

// *********************************************************************//
// DispIntf:  ISpeechLexiconWordDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E5B933C-C9BE-48ED-8842-1EE51BB1D4FF}
// *********************************************************************//
  ISpeechLexiconWordDisp = dispinterface
    ['{4E5B933C-C9BE-48ED-8842-1EE51BB1D4FF}']
    property LangId: Integer readonly dispid 1;
    property type_: SpeechWordType readonly dispid 2;
    property Word: WideString readonly dispid 3;
    property Pronunciations: ISpeechLexiconPronunciations readonly dispid 4;
  end;

// *********************************************************************//
// Interface: ISpeechLexiconPronunciations
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {72829128-5682-4704-A0D4-3E2BB6F2EAD3}
// *********************************************************************//
  ISpeechLexiconPronunciations = interface(IDispatch)
    ['{72829128-5682-4704-A0D4-3E2BB6F2EAD3}']
    function Get_Count: Integer; safecall;
    function Item(Index: Integer): ISpeechLexiconPronunciation; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISpeechLexiconPronunciationsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {72829128-5682-4704-A0D4-3E2BB6F2EAD3}
// *********************************************************************//
  ISpeechLexiconPronunciationsDisp = dispinterface
    ['{72829128-5682-4704-A0D4-3E2BB6F2EAD3}']
    property Count: Integer readonly dispid 1;
    function Item(Index: Integer): ISpeechLexiconPronunciation; dispid 0;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISpeechLexiconPronunciation
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {95252C5D-9E43-4F4A-9899-48EE73352F9F}
// *********************************************************************//
  ISpeechLexiconPronunciation = interface(IDispatch)
    ['{95252C5D-9E43-4F4A-9899-48EE73352F9F}']
    function Get_type_: SpeechLexiconType; safecall;
    function Get_LangId: Integer; safecall;
    function Get_PartOfSpeech: SpeechPartOfSpeech; safecall;
    function Get_PhoneIds: OleVariant; safecall;
    function Get_Symbolic: WideString; safecall;
    property type_: SpeechLexiconType read Get_type_;
    property LangId: Integer read Get_LangId;
    property PartOfSpeech: SpeechPartOfSpeech read Get_PartOfSpeech;
    property PhoneIds: OleVariant read Get_PhoneIds;
    property Symbolic: WideString read Get_Symbolic;
  end;

// *********************************************************************//
// DispIntf:  ISpeechLexiconPronunciationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {95252C5D-9E43-4F4A-9899-48EE73352F9F}
// *********************************************************************//
  ISpeechLexiconPronunciationDisp = dispinterface
    ['{95252C5D-9E43-4F4A-9899-48EE73352F9F}']
    property type_: SpeechLexiconType readonly dispid 1;
    property LangId: Integer readonly dispid 2;
    property PartOfSpeech: SpeechPartOfSpeech readonly dispid 3;
    property PhoneIds: OleVariant readonly dispid 4;
    property Symbolic: WideString readonly dispid 5;
  end;

// *********************************************************************//
// Interface: ISpeechXMLRecoResult
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AAEC54AF-8F85-4924-944D-B79D39D72E19}
// *********************************************************************//
  ISpeechXMLRecoResult = interface(ISpeechRecoResult)
    ['{AAEC54AF-8F85-4924-944D-B79D39D72E19}']
    function GetXMLResult(Options: SPXMLRESULTOPTIONS): WideString; safecall;
    function GetXMLErrorInfo(out LineNumber: Integer; out ScriptLine: WideString; 
                             out Source: WideString; out Description: WideString; 
                             out ResultCode: Integer): WordBool; safecall;
  end;

// *********************************************************************//
// DispIntf:  ISpeechXMLRecoResultDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AAEC54AF-8F85-4924-944D-B79D39D72E19}
// *********************************************************************//
  ISpeechXMLRecoResultDisp = dispinterface
    ['{AAEC54AF-8F85-4924-944D-B79D39D72E19}']
    function GetXMLResult(Options: SPXMLRESULTOPTIONS): WideString; dispid 10;
    function GetXMLErrorInfo(out LineNumber: Integer; out ScriptLine: WideString; 
                             out Source: WideString; out Description: WideString; 
                             out ResultCode: Integer): WordBool; dispid 11;
    property RecoContext: ISpeechRecoContext readonly dispid 1;
    property Times: ISpeechRecoResultTimes readonly dispid 2;
    property AudioFormat: ISpeechAudioFormat dispid 3;
    property PhraseInfo: ISpeechPhraseInfo readonly dispid 4;
    function Alternates(RequestCount: Integer; StartElement: Integer; Elements: Integer): ISpeechPhraseAlternates; dispid 5;
    function Audio(StartElement: Integer; Elements: Integer): ISpeechMemoryStream; dispid 6;
    function SpeakAudio(StartElement: Integer; Elements: Integer; Flags: SpeechVoiceSpeakFlags): Integer; dispid 7;
    function SaveToMemory: OleVariant; dispid 8;
    procedure DiscardResultInfo(ValueTypes: SpeechDiscardType); dispid 9;
  end;

// *********************************************************************//
// Interface: ISpeechRecoResultDispatch
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {6D60EB64-ACED-40A6-BBF3-4E557F71DEE2}
// *********************************************************************//
  ISpeechRecoResultDispatch = interface(IDispatch)
    ['{6D60EB64-ACED-40A6-BBF3-4E557F71DEE2}']
    function Get_RecoContext: ISpeechRecoContext; safecall;
    function Get_Times: ISpeechRecoResultTimes; safecall;
    procedure _Set_AudioFormat(const Format: ISpeechAudioFormat); safecall;
    function Get_AudioFormat: ISpeechAudioFormat; safecall;
    function Get_PhraseInfo: ISpeechPhraseInfo; safecall;
    function Alternates(RequestCount: Integer; StartElement: Integer; Elements: Integer): ISpeechPhraseAlternates; safecall;
    function Audio(StartElement: Integer; Elements: Integer): ISpeechMemoryStream; safecall;
    function SpeakAudio(StartElement: Integer; Elements: Integer; Flags: SpeechVoiceSpeakFlags): Integer; safecall;
    function SaveToMemory: OleVariant; safecall;
    procedure DiscardResultInfo(ValueTypes: SpeechDiscardType); safecall;
    function GetXMLResult(Options: SPXMLRESULTOPTIONS): WideString; safecall;
    function GetXMLErrorInfo(out LineNumber: Integer; out ScriptLine: WideString; 
                             out Source: WideString; out Description: WideString; 
                             out ResultCode: HResult): WordBool; safecall;
    procedure SetTextFeedback(const Feedback: WideString; WasSuccessful: WordBool); safecall;
    property RecoContext: ISpeechRecoContext read Get_RecoContext;
    property Times: ISpeechRecoResultTimes read Get_Times;
    property AudioFormat: ISpeechAudioFormat read Get_AudioFormat write _Set_AudioFormat;
    property PhraseInfo: ISpeechPhraseInfo read Get_PhraseInfo;
  end;

// *********************************************************************//
// DispIntf:  ISpeechRecoResultDispatchDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {6D60EB64-ACED-40A6-BBF3-4E557F71DEE2}
// *********************************************************************//
  ISpeechRecoResultDispatchDisp = dispinterface
    ['{6D60EB64-ACED-40A6-BBF3-4E557F71DEE2}']
    property RecoContext: ISpeechRecoContext readonly dispid 1;
    property Times: ISpeechRecoResultTimes readonly dispid 2;
    property AudioFormat: ISpeechAudioFormat dispid 3;
    property PhraseInfo: ISpeechPhraseInfo readonly dispid 4;
    function Alternates(RequestCount: Integer; StartElement: Integer; Elements: Integer): ISpeechPhraseAlternates; dispid 5;
    function Audio(StartElement: Integer; Elements: Integer): ISpeechMemoryStream; dispid 6;
    function SpeakAudio(StartElement: Integer; Elements: Integer; Flags: SpeechVoiceSpeakFlags): Integer; dispid 7;
    function SaveToMemory: OleVariant; dispid 8;
    procedure DiscardResultInfo(ValueTypes: SpeechDiscardType); dispid 9;
    function GetXMLResult(Options: SPXMLRESULTOPTIONS): WideString; dispid 10;
    function GetXMLErrorInfo(out LineNumber: Integer; out ScriptLine: WideString; 
                             out Source: WideString; out Description: WideString; 
                             out ResultCode: HResult): WordBool; dispid 11;
    procedure SetTextFeedback(const Feedback: WideString; WasSuccessful: WordBool); dispid 12;
  end;

// *********************************************************************//
// Interface: ISpeechPhraseInfoBuilder
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3B151836-DF3A-4E0A-846C-D2ADC9334333}
// *********************************************************************//
  ISpeechPhraseInfoBuilder = interface(IDispatch)
    ['{3B151836-DF3A-4E0A-846C-D2ADC9334333}']
    function RestorePhraseFromMemory(const PhraseInMemory: OleVariant): ISpeechPhraseInfo; safecall;
  end;

// *********************************************************************//
// DispIntf:  ISpeechPhraseInfoBuilderDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3B151836-DF3A-4E0A-846C-D2ADC9334333}
// *********************************************************************//
  ISpeechPhraseInfoBuilderDisp = dispinterface
    ['{3B151836-DF3A-4E0A-846C-D2ADC9334333}']
    function RestorePhraseFromMemory(const PhraseInMemory: OleVariant): ISpeechPhraseInfo; dispid 1;
  end;

// *********************************************************************//
// Interface: ISpeechPhoneConverter
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C3E4F353-433F-43D6-89A1-6A62A7054C3D}
// *********************************************************************//
  ISpeechPhoneConverter = interface(IDispatch)
    ['{C3E4F353-433F-43D6-89A1-6A62A7054C3D}']
    function Get_LanguageId: Integer; safecall;
    procedure Set_LanguageId(LanguageId: Integer); safecall;
    function PhoneToId(const Phonemes: WideString): OleVariant; safecall;
    function IdToPhone(IdArray: OleVariant): WideString; safecall;
    property LanguageId: Integer read Get_LanguageId write Set_LanguageId;
  end;

// *********************************************************************//
// DispIntf:  ISpeechPhoneConverterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C3E4F353-433F-43D6-89A1-6A62A7054C3D}
// *********************************************************************//
  ISpeechPhoneConverterDisp = dispinterface
    ['{C3E4F353-433F-43D6-89A1-6A62A7054C3D}']
    property LanguageId: Integer dispid 1;
    function PhoneToId(const Phonemes: WideString): OleVariant; dispid 2;
    function IdToPhone(IdArray: OleVariant): WideString; dispid 3;
  end;

// *********************************************************************//
// Interface: ISpNotifySink
// Flags:     (512) Restricted
// GUID:      {259684DC-37C3-11D2-9603-00C04F8EE628}
// *********************************************************************//
  ISpNotifySink = interface(IUnknown)
    ['{259684DC-37C3-11D2-9603-00C04F8EE628}']
    function Notify: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpNotifyTranslator
// Flags:     (512) Restricted
// GUID:      {ACA16614-5D3D-11D2-960E-00C04F8EE628}
// *********************************************************************//
  ISpNotifyTranslator = interface(ISpNotifySink)
    ['{ACA16614-5D3D-11D2-960E-00C04F8EE628}']
    function InitWindowMessage(var hWnd: _RemotableHandle; Msg: SYSUINT; wParam: UINT_PTR; 
                               lParam: LONG_PTR): HResult; stdcall;
    function InitCallback(pfnCallback: PPPrivateAlias1; wParam: UINT_PTR; lParam: LONG_PTR): HResult; stdcall;
    function InitSpNotifyCallback(pSpCallback: PPPrivateAlias1; wParam: UINT_PTR; lParam: LONG_PTR): HResult; stdcall;
    function InitWin32Event(hEvent: Pointer; fCloseHandleOnRelease: Integer): HResult; stdcall;
    function Wait(dwMilliseconds: LongWord): HResult; stdcall;
    function GetEventHandle: Pointer; stdcall;
  end;

// *********************************************************************//
// Interface: ISpDataKey
// Flags:     (512) Restricted
// GUID:      {14056581-E16C-11D2-BB90-00C04F8EE6C0}
// *********************************************************************//
  ISpDataKey = interface(IUnknown)
    ['{14056581-E16C-11D2-BB90-00C04F8EE6C0}']
    function SetData(pszValueName: PWideChar; cbData: LongWord; var pData: Byte): HResult; stdcall;
    function GetData(pszValueName: PWideChar; var pcbData: LongWord; out pData: Byte): HResult; stdcall;
    function SetStringValue(pszValueName: PWideChar; pszValue: PWideChar): HResult; stdcall;
    function GetStringValue(pszValueName: PWideChar; out ppszValue: PWideChar): HResult; stdcall;
    function SetDWORD(pszValueName: PWideChar; dwValue: LongWord): HResult; stdcall;
    function GetDWORD(pszValueName: PWideChar; out pdwValue: LongWord): HResult; stdcall;
    function OpenKey(pszSubKeyName: PWideChar; out ppSubKey: ISpDataKey): HResult; stdcall;
    function CreateKey(pszSubKey: PWideChar; out ppSubKey: ISpDataKey): HResult; stdcall;
    function DeleteKey(pszSubKey: PWideChar): HResult; stdcall;
    function DeleteValue(pszValueName: PWideChar): HResult; stdcall;
    function EnumKeys(Index: LongWord; out ppszSubKeyName: PWideChar): HResult; stdcall;
    function EnumValues(Index: LongWord; out ppszValueName: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpObjectTokenCategory
// Flags:     (512) Restricted
// GUID:      {2D3D3845-39AF-4850-BBF9-40B49780011D}
// *********************************************************************//
  ISpObjectTokenCategory = interface(ISpDataKey)
    ['{2D3D3845-39AF-4850-BBF9-40B49780011D}']
    function SetId(pszCategoryId: PWideChar; fCreateIfNotExist: Integer): HResult; stdcall;
    function GetId(out ppszCoMemCategoryId: PWideChar): HResult; stdcall;
    function GetDataKey(spdkl: SPDATAKEYLOCATION; out ppDataKey: ISpDataKey): HResult; stdcall;
    function EnumTokens(pzsReqAttribs: PWideChar; pszOptAttribs: PWideChar; 
                        out ppEnum: IEnumSpObjectTokens): HResult; stdcall;
    function SetDefaultTokenId(pszTokenId: PWideChar): HResult; stdcall;
    function GetDefaultTokenId(out ppszCoMemTokenId: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumSpObjectTokens
// Flags:     (512) Restricted
// GUID:      {06B64F9E-7FDA-11D2-B4F2-00C04F797396}
// *********************************************************************//
  IEnumSpObjectTokens = interface(IUnknown)
    ['{06B64F9E-7FDA-11D2-B4F2-00C04F797396}']
    function Next(celt: LongWord; out pelt: ISpObjectToken; out pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppEnum: IEnumSpObjectTokens): HResult; stdcall;
    function Item(Index: LongWord; out ppToken: ISpObjectToken): HResult; stdcall;
    function GetCount(out pCount: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpObjectToken
// Flags:     (512) Restricted
// GUID:      {14056589-E16C-11D2-BB90-00C04F8EE6C0}
// *********************************************************************//
  ISpObjectToken = interface(ISpDataKey)
    ['{14056589-E16C-11D2-BB90-00C04F8EE6C0}']
    function SetId(pszCategoryId: PWideChar; pszTokenId: PWideChar; fCreateIfNotExist: Integer): HResult; stdcall;
    function GetId(out ppszCoMemTokenId: PWideChar): HResult; stdcall;
    function GetCategory(out ppTokenCategory: ISpObjectTokenCategory): HResult; stdcall;
    function CreateInstance(const pUnkOuter: IUnknown; dwClsContext: LongWord; var riid: TGUID; 
                            out ppvObject: Pointer): HResult; stdcall;
    function GetStorageFileName(var clsidCaller: TGUID; pszValueName: PWideChar; 
                                pszFileNameSpecifier: PWideChar; nFolder: LongWord; 
                                out ppszFilePath: PWideChar): HResult; stdcall;
    function RemoveStorageFileName(var clsidCaller: TGUID; pszKeyName: PWideChar; 
                                   fDeleteFile: Integer): HResult; stdcall;
    function Remove(var pclsidCaller: TGUID): HResult; stdcall;
    function IsUISupported(pszTypeOfUI: PWideChar; pvExtraData: Pointer; cbExtraData: LongWord; 
                           const punkObject: IUnknown; out pfSupported: Integer): HResult; stdcall;
    function DisplayUI(var hWndParent: _RemotableHandle; pszTitle: PWideChar; 
                       pszTypeOfUI: PWideChar; pvExtraData: Pointer; cbExtraData: LongWord; 
                       const punkObject: IUnknown): HResult; stdcall;
    function MatchesAttributes(pszAttributes: PWideChar; out pfMatches: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IServiceProvider
// Flags:     (0)
// GUID:      {6D5140C1-7436-11CE-8034-00AA006009FA}
// *********************************************************************//
  IServiceProvider = interface(IUnknown)
    ['{6D5140C1-7436-11CE-8034-00AA006009FA}']
    function RemoteQueryService(var guidService: TGUID; var riid: TGUID; out ppvObject: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpResourceManager
// Flags:     (512) Restricted
// GUID:      {93384E18-5014-43D5-ADBB-A78E055926BD}
// *********************************************************************//
  ISpResourceManager = interface(IServiceProvider)
    ['{93384E18-5014-43D5-ADBB-A78E055926BD}']
    function SetObject(var guidServiceId: TGUID; const punkObject: IUnknown): HResult; stdcall;
    function GetObject(var guidServiceId: TGUID; var ObjectCLSID: TGUID; var ObjectIID: TGUID; 
                       fReleaseWhenLastExternalRefReleased: Integer; out ppObject: Pointer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpStreamFormat
// Flags:     (512) Restricted
// GUID:      {BED530BE-2606-4F4D-A1C0-54C5CDA5566F}
// *********************************************************************//
  ISpStreamFormat = interface(IStream)
    ['{BED530BE-2606-4F4D-A1C0-54C5CDA5566F}']
    function GetFormat(var pguidFormatId: TGUID; out ppCoMemWaveFormatEx: PUserType2): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpStreamFormatConverter
// Flags:     (512) Restricted
// GUID:      {678A932C-EA71-4446-9B41-78FDA6280A29}
// *********************************************************************//
  ISpStreamFormatConverter = interface(ISpStreamFormat)
    ['{678A932C-EA71-4446-9B41-78FDA6280A29}']
    function SetBaseStream(const pStream: ISpStreamFormat; fSetFormatToBaseStreamFormat: Integer; 
                           fWriteToBaseStream: Integer): HResult; stdcall;
    function GetBaseStream(out ppStream: ISpStreamFormat): HResult; stdcall;
    function SetFormat(var rguidFormatIdOfConvertedStream: TGUID; 
                       var pWaveFormatExOfConvertedStream: WAVEFORMATEX): HResult; stdcall;
    function ResetSeekPosition: HResult; stdcall;
    function ScaleConvertedToBaseOffset(ullOffsetConvertedStream: Largeuint; 
                                        out pullOffsetBaseStream: Largeuint): HResult; stdcall;
    function ScaleBaseToConvertedOffset(ullOffsetBaseStream: Largeuint; 
                                        out pullOffsetConvertedStream: Largeuint): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpNotifySource
// Flags:     (512) Restricted
// GUID:      {5EFF4AEF-8487-11D2-961C-00C04F8EE628}
// *********************************************************************//
  ISpNotifySource = interface(IUnknown)
    ['{5EFF4AEF-8487-11D2-961C-00C04F8EE628}']
    function SetNotifySink(const pNotifySink: ISpNotifySink): HResult; stdcall;
    function SetNotifyWindowMessage(var hWnd: _RemotableHandle; Msg: SYSUINT; wParam: UINT_PTR; 
                                    lParam: LONG_PTR): HResult; stdcall;
    function SetNotifyCallbackFunction(pfnCallback: PPPrivateAlias1; wParam: UINT_PTR; 
                                       lParam: LONG_PTR): HResult; stdcall;
    function SetNotifyCallbackInterface(pSpCallback: PPPrivateAlias1; wParam: UINT_PTR; 
                                        lParam: LONG_PTR): HResult; stdcall;
    function SetNotifyWin32Event: HResult; stdcall;
    function WaitForNotifyEvent(dwMilliseconds: LongWord): HResult; stdcall;
    function GetNotifyEventHandle: Pointer; stdcall;
  end;

// *********************************************************************//
// Interface: ISpEventSource
// Flags:     (512) Restricted
// GUID:      {BE7A9CCE-5F9E-11D2-960F-00C04F8EE628}
// *********************************************************************//
  ISpEventSource = interface(ISpNotifySource)
    ['{BE7A9CCE-5F9E-11D2-960F-00C04F8EE628}']
    function SetInterest(ullEventInterest: Largeuint; ullQueuedInterest: Largeuint): HResult; stdcall;
    function GetEvents(ulCount: LongWord; out pEventArray: SPEVENT; out pulFetched: LongWord): HResult; stdcall;
    function GetInfo(out pInfo: SPEVENTSOURCEINFO): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpEventSink
// Flags:     (512) Restricted
// GUID:      {BE7A9CC9-5F9E-11D2-960F-00C04F8EE628}
// *********************************************************************//
  ISpEventSink = interface(IUnknown)
    ['{BE7A9CC9-5F9E-11D2-960F-00C04F8EE628}']
    function AddEvents(var pEventArray: SPEVENT; ulCount: LongWord): HResult; stdcall;
    function GetEventInterest(out pullEventInterest: Largeuint): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpObjectWithToken
// Flags:     (512) Restricted
// GUID:      {5B559F40-E952-11D2-BB91-00C04F8EE6C0}
// *********************************************************************//
  ISpObjectWithToken = interface(IUnknown)
    ['{5B559F40-E952-11D2-BB91-00C04F8EE6C0}']
    function SetObjectToken(const pToken: ISpObjectToken): HResult; stdcall;
    function GetObjectToken(out ppToken: ISpObjectToken): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpAudio
// Flags:     (512) Restricted
// GUID:      {C05C768F-FAE8-4EC2-8E07-338321C12452}
// *********************************************************************//
  ISpAudio = interface(ISpStreamFormat)
    ['{C05C768F-FAE8-4EC2-8E07-338321C12452}']
    function SetState(NewState: SPAUDIOSTATE; ullReserved: Largeuint): HResult; stdcall;
    function SetFormat(var rguidFmtId: TGUID; var pWaveFormatEx: WAVEFORMATEX): HResult; stdcall;
    function GetStatus(out pStatus: SPAUDIOSTATUS): HResult; stdcall;
    function SetBufferInfo(var pBuffInfo: SPAUDIOBUFFERINFO): HResult; stdcall;
    function GetBufferInfo(out pBuffInfo: SPAUDIOBUFFERINFO): HResult; stdcall;
    function GetDefaultFormat(out pFormatId: TGUID; out ppCoMemWaveFormatEx: PUserType2): HResult; stdcall;
    function EventHandle: Pointer; stdcall;
    function GetVolumeLevel(out pLevel: LongWord): HResult; stdcall;
    function SetVolumeLevel(Level: LongWord): HResult; stdcall;
    function GetBufferNotifySize(out pcbSize: LongWord): HResult; stdcall;
    function SetBufferNotifySize(cbSize: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpMMSysAudio
// Flags:     (512) Restricted
// GUID:      {15806F6E-1D70-4B48-98E6-3B1A007509AB}
// *********************************************************************//
  ISpMMSysAudio = interface(ISpAudio)
    ['{15806F6E-1D70-4B48-98E6-3B1A007509AB}']
    function GetDeviceId(out puDeviceId: SYSUINT): HResult; stdcall;
    function SetDeviceId(uDeviceId: SYSUINT): HResult; stdcall;
    function GetMMHandle(out pHandle: Pointer): HResult; stdcall;
    function GetLineId(out puLineId: SYSUINT): HResult; stdcall;
    function SetLineId(uLineId: SYSUINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpStream
// Flags:     (512) Restricted
// GUID:      {12E3CCA9-7518-44C5-A5E7-BA5A79CB929E}
// *********************************************************************//
  ISpStream = interface(ISpStreamFormat)
    ['{12E3CCA9-7518-44C5-A5E7-BA5A79CB929E}']
    function SetBaseStream(const pStream: IStream; var rguidFormat: TGUID; 
                           var pWaveFormatEx: WAVEFORMATEX): HResult; stdcall;
    function GetBaseStream(out ppStream: IStream): HResult; stdcall;
    function BindToFile(pszFileName: PWideChar; eMode: SPFILEMODE; var pFormatId: TGUID; 
                        var pWaveFormatEx: WAVEFORMATEX; ullEventInterest: Largeuint): HResult; stdcall;
    function Close: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpVoice
// Flags:     (512) Restricted
// GUID:      {6C44DF74-72B9-4992-A1EC-EF996E0422D4}
// *********************************************************************//
  ISpVoice = interface(ISpEventSource)
    ['{6C44DF74-72B9-4992-A1EC-EF996E0422D4}']
    function SetOutput(const pUnkOutput: IUnknown; fAllowFormatChanges: Integer): HResult; stdcall;
    function GetOutputObjectToken(out ppObjectToken: ISpObjectToken): HResult; stdcall;
    function GetOutputStream(out ppStream: ISpStreamFormat): HResult; stdcall;
    function Pause: HResult; stdcall;
    function Resume: HResult; stdcall;
    function SetVoice(const pToken: ISpObjectToken): HResult; stdcall;
    function GetVoice(out ppToken: ISpObjectToken): HResult; stdcall;
    function Speak(pwcs: PWideChar; dwFlags: LongWord; out pulStreamNumber: LongWord): HResult; stdcall;
    function SpeakStream(const pStream: IStream; dwFlags: LongWord; out pulStreamNumber: LongWord): HResult; stdcall;
    function GetStatus(out pStatus: SPVOICESTATUS; out ppszLastBookmark: PWideChar): HResult; stdcall;
    function Skip(pItemType: PWideChar; lNumItems: Integer; out pulNumSkipped: LongWord): HResult; stdcall;
    function SetPriority(ePriority: SPVPRIORITY): HResult; stdcall;
    function GetPriority(out pePriority: SPVPRIORITY): HResult; stdcall;
    function SetAlertBoundary(eBoundary: SPEVENTENUM): HResult; stdcall;
    function GetAlertBoundary(out peBoundary: SPEVENTENUM): HResult; stdcall;
    function SetRate(RateAdjust: Integer): HResult; stdcall;
    function GetRate(out pRateAdjust: Integer): HResult; stdcall;
    function SetVolume(usVolume: Word): HResult; stdcall;
    function GetVolume(out pusVolume: Word): HResult; stdcall;
    function WaitUntilDone(msTimeout: LongWord): HResult; stdcall;
    function SetSyncSpeakTimeout(msTimeout: LongWord): HResult; stdcall;
    function GetSyncSpeakTimeout(out pmsTimeout: LongWord): HResult; stdcall;
    function SpeakCompleteEvent: Pointer; stdcall;
    function IsUISupported(pszTypeOfUI: PWideChar; pvExtraData: Pointer; cbExtraData: LongWord; 
                           out pfSupported: Integer): HResult; stdcall;
    function DisplayUI(var hWndParent: _RemotableHandle; pszTitle: PWideChar; 
                       pszTypeOfUI: PWideChar; pvExtraData: Pointer; cbExtraData: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpPhoneticAlphabetSelection
// Flags:     (512) Restricted
// GUID:      {B2745EFD-42CE-48CA-81F1-A96E02538A90}
// *********************************************************************//
  ISpPhoneticAlphabetSelection = interface(IUnknown)
    ['{B2745EFD-42CE-48CA-81F1-A96E02538A90}']
    function IsAlphabetUPS(out pfIsUPS: Integer): HResult; stdcall;
    function SetAlphabetToUPS(fForceUPS: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpRecoContext
// Flags:     (512) Restricted
// GUID:      {F740A62F-7C15-489E-8234-940A33D9272D}
// *********************************************************************//
  ISpRecoContext = interface(ISpEventSource)
    ['{F740A62F-7C15-489E-8234-940A33D9272D}']
    function GetRecognizer(out ppRecognizer: ISpRecognizer): HResult; stdcall;
    function CreateGrammar(ullGrammarID: Largeuint; out ppGrammar: ISpRecoGrammar): HResult; stdcall;
    function GetStatus(out pStatus: SPRECOCONTEXTSTATUS): HResult; stdcall;
    function GetMaxAlternates(var pcAlternates: LongWord): HResult; stdcall;
    function SetMaxAlternates(cAlternates: LongWord): HResult; stdcall;
    function SetAudioOptions(Options: SPAUDIOOPTIONS; var pAudioFormatId: TGUID; 
                             var pWaveFormatEx: WAVEFORMATEX): HResult; stdcall;
    function GetAudioOptions(var pOptions: SPAUDIOOPTIONS; out pAudioFormatId: TGUID; 
                             out ppCoMemWFEX: PUserType2): HResult; stdcall;
    function DeserializeResult(var pSerializedResult: SPSERIALIZEDRESULT; 
                               out ppResult: ISpRecoResult): HResult; stdcall;
    function Bookmark(Options: SPBOOKMARKOPTIONS; ullStreamPosition: Largeuint; 
                      lparamEvent: LONG_PTR): HResult; stdcall;
    function SetAdaptationData(pAdaptationData: PWideChar; cch: LongWord): HResult; stdcall;
    function Pause(dwReserved: LongWord): HResult; stdcall;
    function Resume(dwReserved: LongWord): HResult; stdcall;
    function SetVoice(const pVoice: ISpVoice; fAllowFormatChanges: Integer): HResult; stdcall;
    function GetVoice(out ppVoice: ISpVoice): HResult; stdcall;
    function SetVoicePurgeEvent(ullEventInterest: Largeuint): HResult; stdcall;
    function GetVoicePurgeEvent(out pullEventInterest: Largeuint): HResult; stdcall;
    function SetContextState(eContextState: SPCONTEXTSTATE): HResult; stdcall;
    function GetContextState(out peContextState: SPCONTEXTSTATE): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpRecoContext2
// Flags:     (512) Restricted
// GUID:      {BEAD311C-52FF-437F-9464-6B21054CA73D}
// *********************************************************************//
  ISpRecoContext2 = interface(IUnknown)
    ['{BEAD311C-52FF-437F-9464-6B21054CA73D}']
    function SetGrammarOptions(eGrammarOptions: LongWord): HResult; stdcall;
    function GetGrammarOptions(out peGrammarOptions: LongWord): HResult; stdcall;
    function SetAdaptationData2(pAdaptationData: PWideChar; cch: LongWord; pTopicName: PWideChar; 
                                eAdaptationSettings: LongWord; eRelevance: SPADAPTATIONRELEVANCE): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpProperties
// Flags:     (512) Restricted
// GUID:      {5B4FB971-B115-4DE1-AD97-E482E3BF6EE4}
// *********************************************************************//
  ISpProperties = interface(IUnknown)
    ['{5B4FB971-B115-4DE1-AD97-E482E3BF6EE4}']
    function SetPropertyNum(pName: PWideChar; lValue: Integer): HResult; stdcall;
    function GetPropertyNum(pName: PWideChar; out plValue: Integer): HResult; stdcall;
    function SetPropertyString(pName: PWideChar; pValue: PWideChar): HResult; stdcall;
    function GetPropertyString(pName: PWideChar; out ppCoMemValue: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpRecognizer
// Flags:     (512) Restricted
// GUID:      {C2B5F241-DAA0-4507-9E16-5A1EAA2B7A5C}
// *********************************************************************//
  ISpRecognizer = interface(ISpProperties)
    ['{C2B5F241-DAA0-4507-9E16-5A1EAA2B7A5C}']
    function SetRecognizer(const pRecognizer: ISpObjectToken): HResult; stdcall;
    function GetRecognizer(out ppRecognizer: ISpObjectToken): HResult; stdcall;
    function SetInput(const pUnkInput: IUnknown; fAllowFormatChanges: Integer): HResult; stdcall;
    function GetInputObjectToken(out ppToken: ISpObjectToken): HResult; stdcall;
    function GetInputStream(out ppStream: ISpStreamFormat): HResult; stdcall;
    function CreateRecoContext(out ppNewCtxt: ISpRecoContext): HResult; stdcall;
    function GetRecoProfile(out ppToken: ISpObjectToken): HResult; stdcall;
    function SetRecoProfile(const pToken: ISpObjectToken): HResult; stdcall;
    function IsSharedInstance: HResult; stdcall;
    function GetRecoState(out pState: SPRECOSTATE): HResult; stdcall;
    function SetRecoState(NewState: SPRECOSTATE): HResult; stdcall;
    function GetStatus(out pStatus: SPRECOGNIZERSTATUS): HResult; stdcall;
    function GetFormat(WaveFormatType: SPSTREAMFORMATTYPE; out pFormatId: TGUID; 
                       out ppCoMemWFEX: PUserType2): HResult; stdcall;
    function IsUISupported(pszTypeOfUI: PWideChar; pvExtraData: Pointer; cbExtraData: LongWord; 
                           out pfSupported: Integer): HResult; stdcall;
    function DisplayUI(var hWndParent: _RemotableHandle; pszTitle: PWideChar; 
                       pszTypeOfUI: PWideChar; pvExtraData: Pointer; cbExtraData: LongWord): HResult; stdcall;
    function EmulateRecognition(const pPhrase: ISpPhrase): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpPhrase
// Flags:     (512) Restricted
// GUID:      {1A5C0354-B621-4B5A-8791-D306ED379E53}
// *********************************************************************//
  ISpPhrase = interface(IUnknown)
    ['{1A5C0354-B621-4B5A-8791-D306ED379E53}']
    function GetPhrase(out ppCoMemPhrase: PUserType7): HResult; stdcall;
    function GetSerializedPhrase(out ppCoMemPhrase: PUserType8): HResult; stdcall;
    function GetText(ulStart: LongWord; ulCount: LongWord; fUseTextReplacements: Integer; 
                     out ppszCoMemText: PWideChar; out pbDisplayAttributes: Byte): HResult; stdcall;
    function Discard(dwValueTypes: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpGrammarBuilder
// Flags:     (512) Restricted
// GUID:      {8137828F-591A-4A42-BE58-49EA7EBAAC68}
// *********************************************************************//
  ISpGrammarBuilder = interface(IUnknown)
    ['{8137828F-591A-4A42-BE58-49EA7EBAAC68}']
    function ResetGrammar(NewLanguage: Word): HResult; stdcall;
    function GetRule(pszRuleName: PWideChar; dwRuleId: LongWord; dwAttributes: LongWord; 
                     fCreateIfNotExist: Integer; out phInitialState: Pointer): HResult; stdcall;
    function ClearRule(hState: Pointer): HResult; stdcall;
    function CreateNewState(hState: Pointer; out phState: Pointer): HResult; stdcall;
    function AddWordTransition(hFromState: Pointer; hToState: Pointer; psz: PWideChar; 
                               pszSeparators: PWideChar; eWordType: SPGRAMMARWORDTYPE; 
                               Weight: Single; var pPropInfo: SPPROPERTYINFO): HResult; stdcall;
    function AddRuleTransition(hFromState: Pointer; hToState: Pointer; hRule: Pointer; 
                               Weight: Single; var pPropInfo: SPPROPERTYINFO): HResult; stdcall;
    function AddResource(hRuleState: Pointer; pszResourceName: PWideChar; 
                         pszResourceValue: PWideChar): HResult; stdcall;
    function Commit(dwReserved: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpRecoGrammar
// Flags:     (512) Restricted
// GUID:      {2177DB29-7F45-47D0-8554-067E91C80502}
// *********************************************************************//
  ISpRecoGrammar = interface(ISpGrammarBuilder)
    ['{2177DB29-7F45-47D0-8554-067E91C80502}']
    function GetGrammarId(out pullGrammarId: Largeuint): HResult; stdcall;
    function GetRecoContext(out ppRecoCtxt: ISpRecoContext): HResult; stdcall;
    function LoadCmdFromFile(pszFileName: PWideChar; Options: SPLOADOPTIONS): HResult; stdcall;
    function LoadCmdFromObject(var rcid: TGUID; pszGrammarName: PWideChar; Options: SPLOADOPTIONS): HResult; stdcall;
    function LoadCmdFromResource(hModule: Pointer; pszResourceName: PWideChar; 
                                 pszResourceType: PWideChar; wLanguage: Word; Options: SPLOADOPTIONS): HResult; stdcall;
    function LoadCmdFromMemory(var pGrammar: SPBINARYGRAMMAR; Options: SPLOADOPTIONS): HResult; stdcall;
    function LoadCmdFromProprietaryGrammar(var rguidParam: TGUID; pszStringParam: PWideChar; 
                                           pvDataPrarm: Pointer; cbDataSize: LongWord; 
                                           Options: SPLOADOPTIONS): HResult; stdcall;
    function SetRuleState(pszName: PWideChar; pReserved: Pointer; NewState: SPRULESTATE): HResult; stdcall;
    function SetRuleIdState(ulRuleId: LongWord; NewState: SPRULESTATE): HResult; stdcall;
    function LoadDictation(pszTopicName: PWideChar; Options: SPLOADOPTIONS): HResult; stdcall;
    function UnloadDictation: HResult; stdcall;
    function SetDictationState(NewState: SPRULESTATE): HResult; stdcall;
    function SetWordSequenceData(var pText: Word; cchText: LongWord; var pInfo: SPTEXTSELECTIONINFO): HResult; stdcall;
    function SetTextSelection(var pInfo: SPTEXTSELECTIONINFO): HResult; stdcall;
    function IsPronounceable(pszWord: PWideChar; out pWordPronounceable: SPWORDPRONOUNCEABLE): HResult; stdcall;
    function SetGrammarState(eGrammarState: SPGRAMMARSTATE): HResult; stdcall;
    function SaveCmd(const pStream: IStream; out ppszCoMemErrorText: PWideChar): HResult; stdcall;
    function GetGrammarState(out peGrammarState: SPGRAMMARSTATE): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpRecoResult
// Flags:     (512) Restricted
// GUID:      {20B053BE-E235-43CD-9A2A-8D17A48B7842}
// *********************************************************************//
  ISpRecoResult = interface(ISpPhrase)
    ['{20B053BE-E235-43CD-9A2A-8D17A48B7842}']
    function GetResultTimes(out pTimes: SPRECORESULTTIMES): HResult; stdcall;
    function GetAlternates(ulStartElement: LongWord; cElements: LongWord; ulRequestCount: LongWord; 
                           out ppPhrases: ISpPhraseAlt; out pcPhrasesReturned: LongWord): HResult; stdcall;
    function GetAudio(ulStartElement: LongWord; cElements: LongWord; out ppStream: ISpStreamFormat): HResult; stdcall;
    function SpeakAudio(ulStartElement: LongWord; cElements: LongWord; dwFlags: LongWord; 
                        out pulStreamNumber: LongWord): HResult; stdcall;
    function Serialize(out ppCoMemSerializedResult: PUserType6): HResult; stdcall;
    function ScaleAudio(var pAudioFormatId: TGUID; var pWaveFormatEx: WAVEFORMATEX): HResult; stdcall;
    function GetRecoContext(out ppRecoContext: ISpRecoContext): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpPhraseAlt
// Flags:     (512) Restricted
// GUID:      {8FCEBC98-4E49-4067-9C6C-D86A0E092E3D}
// *********************************************************************//
  ISpPhraseAlt = interface(ISpPhrase)
    ['{8FCEBC98-4E49-4067-9C6C-D86A0E092E3D}']
    function GetAltInfo(out ppParent: ISpPhrase; out pulStartElementInParent: LongWord; 
                        out pcElementsInParent: LongWord; out pcElementsInAlt: LongWord): HResult; stdcall;
    function Commit: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpRecognizer2
// Flags:     (512) Restricted
// GUID:      {8FC6D974-C81E-4098-93C5-0147F61ED4D3}
// *********************************************************************//
  ISpRecognizer2 = interface(IUnknown)
    ['{8FC6D974-C81E-4098-93C5-0147F61ED4D3}']
    function EmulateRecognitionEx(const pPhrase: ISpPhrase; dwCompareFlags: LongWord): HResult; stdcall;
    function SetTrainingState(fDoingTraining: Integer; fAdaptFromTrainingData: Integer): HResult; stdcall;
    function ResetAcousticModelAdaptation: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpRecognizer3
// Flags:     (512) Restricted
// GUID:      {DF1B943C-5838-4AA2-8706-D7CD5B333499}
// *********************************************************************//
  ISpRecognizer3 = interface(IUnknown)
    ['{DF1B943C-5838-4AA2-8706-D7CD5B333499}']
    function GetCategory(categoryType: SPCATEGORYTYPE; out ppCategory: ISpRecoCategory): HResult; stdcall;
    function SetActiveCategory(const pCategory: ISpRecoCategory): HResult; stdcall;
    function GetActiveCategory(out ppCategory: ISpRecoCategory): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpSerializeState
// Flags:     (512) Restricted
// GUID:      {21B501A0-0EC7-46C9-92C3-A2BC784C54B9}
// *********************************************************************//
  ISpSerializeState = interface(IUnknown)
    ['{21B501A0-0EC7-46C9-92C3-A2BC784C54B9}']
    function GetSerializedState(out ppbData: PByte1; out pulSize: LongWord; dwReserved: LongWord): HResult; stdcall;
    function SetSerializedState(var pbData: Byte; ulSize: LongWord; dwReserved: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpRecoCategory
// Flags:     (512) Restricted
// GUID:      {DA0CD0F9-14A2-4F09-8C2A-85CC48979345}
// *********************************************************************//
  ISpRecoCategory = interface(IUnknown)
    ['{DA0CD0F9-14A2-4F09-8C2A-85CC48979345}']
    function GetType(out peCategoryType: SPCATEGORYTYPE): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpLexicon
// Flags:     (512) Restricted
// GUID:      {DA41A7C2-5383-4DB2-916B-6C1719E3DB58}
// *********************************************************************//
  ISpLexicon = interface(IUnknown)
    ['{DA41A7C2-5383-4DB2-916B-6C1719E3DB58}']
    function GetPronunciations(pszWord: PWideChar; LangId: Word; dwFlags: LongWord; 
                               var pWordPronunciationList: SPWORDPRONUNCIATIONLIST): HResult; stdcall;
    function AddPronunciation(pszWord: PWideChar; LangId: Word; ePartOfSpeech: SPPARTOFSPEECH; 
                              pszPronunciation: PWideChar): HResult; stdcall;
    function RemovePronunciation(pszWord: PWideChar; LangId: Word; ePartOfSpeech: SPPARTOFSPEECH; 
                                 pszPronunciation: PWideChar): HResult; stdcall;
    function GetGeneration(out pdwGeneration: LongWord): HResult; stdcall;
    function GetGenerationChange(dwFlags: LongWord; var pdwGeneration: LongWord; 
                                 var pWordList: SPWORDLIST): HResult; stdcall;
    function GetWords(dwFlags: LongWord; var pdwGeneration: LongWord; var pdwCookie: LongWord; 
                      var pWordList: SPWORDLIST): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpShortcut
// Flags:     (512) Restricted
// GUID:      {3DF681E2-EA56-11D9-8BDE-F66BAD1E3F3A}
// *********************************************************************//
  ISpShortcut = interface(IUnknown)
    ['{3DF681E2-EA56-11D9-8BDE-F66BAD1E3F3A}']
    function AddShortcut(pszDisplay: PWideChar; LangId: Word; pszSpoken: PWideChar; 
                         shType: SPSHORTCUTTYPE): HResult; stdcall;
    function RemoveShortcut(pszDisplay: PWideChar; LangId: Word; pszSpoken: PWideChar; 
                            shType: SPSHORTCUTTYPE): HResult; stdcall;
    function GetShortcuts(LangId: Word; var pShortcutpairList: SPSHORTCUTPAIRLIST): HResult; stdcall;
    function GetGeneration(out pdwGeneration: LongWord): HResult; stdcall;
    function GetWordsFromGenerationChange(var pdwGeneration: LongWord; var pWordList: SPWORDLIST): HResult; stdcall;
    function GetWords(var pdwGeneration: LongWord; var pdwCookie: LongWord; 
                      var pWordList: SPWORDLIST): HResult; stdcall;
    function GetShortcutsForGeneration(var pdwGeneration: LongWord; var pdwCookie: LongWord; 
                                       var pShortcutpairList: SPSHORTCUTPAIRLIST): HResult; stdcall;
    function GetGenerationChange(var pdwGeneration: LongWord; 
                                 var pShortcutpairList: SPSHORTCUTPAIRLIST): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpPhoneConverter
// Flags:     (512) Restricted
// GUID:      {8445C581-0CAC-4A38-ABFE-9B2CE2826455}
// *********************************************************************//
  ISpPhoneConverter = interface(ISpObjectWithToken)
    ['{8445C581-0CAC-4A38-ABFE-9B2CE2826455}']
    function PhoneToId(pszPhone: PWideChar; out pId: Word): HResult; stdcall;
    function IdToPhone(pId: PWideChar; out pszPhone: Word): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpPhoneticAlphabetConverter
// Flags:     (512) Restricted
// GUID:      {133ADCD4-19B4-4020-9FDC-842E78253B17}
// *********************************************************************//
  ISpPhoneticAlphabetConverter = interface(IUnknown)
    ['{133ADCD4-19B4-4020-9FDC-842E78253B17}']
    function GetLangId(out pLangID: Word): HResult; stdcall;
    function SetLangId(LangId: Word): HResult; stdcall;
    function SAPI2UPS(var pszSAPIId: Word; out pszUPSId: Word; cMaxLength: LongWord): HResult; stdcall;
    function UPS2SAPI(var pszUPSId: Word; out pszSAPIId: Word; cMaxLength: LongWord): HResult; stdcall;
    function GetMaxConvertLength(cSrcLength: LongWord; bSAPI2UPS: Integer; 
                                 out pcMaxDestLength: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpXMLRecoResult
// Flags:     (512) Restricted
// GUID:      {AE39362B-45A8-4074-9B9E-CCF49AA2D0B6}
// *********************************************************************//
  ISpXMLRecoResult = interface(ISpRecoResult)
    ['{AE39362B-45A8-4074-9B9E-CCF49AA2D0B6}']
    function GetXMLResult(out ppszCoMemXMLResult: PWideChar; Options: SPXMLRESULTOPTIONS): HResult; stdcall;
    function GetXMLErrorInfo(out pSemanticErrorInfo: SPSEMANTICERRORINFO): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpRecoGrammar2
// Flags:     (512) Restricted
// GUID:      {4B37BC9E-9ED6-44A3-93D3-18F022B79EC3}
// *********************************************************************//
  ISpRecoGrammar2 = interface(IUnknown)
    ['{4B37BC9E-9ED6-44A3-93D3-18F022B79EC3}']
    function GetRules(out ppCoMemRules: PUserType17; out puNumRules: SYSUINT): HResult; stdcall;
    function LoadCmdFromFile2(pszFileName: PWideChar; Options: SPLOADOPTIONS; 
                              pszSharingUri: PWideChar; pszBaseUri: PWideChar): HResult; stdcall;
    function LoadCmdFromMemory2(var pGrammar: SPBINARYGRAMMAR; Options: SPLOADOPTIONS; 
                                pszSharingUri: PWideChar; pszBaseUri: PWideChar): HResult; stdcall;
    function SetRulePriority(pszRuleName: PWideChar; ulRuleId: LongWord; nRulePriority: SYSINT): HResult; stdcall;
    function SetRuleWeight(pszRuleName: PWideChar; ulRuleId: LongWord; flWeight: Single): HResult; stdcall;
    function SetDictationWeight(flWeight: Single): HResult; stdcall;
    function SetGrammarLoader(const pLoader: ISpeechResourceLoader): HResult; stdcall;
    function SetSMLSecurityManager(const pSMLSecurityManager: IInternetSecurityManager): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpeechResourceLoader
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B9AC5783-FCD0-4B21-B119-B4F8DA8FD2C3}
// *********************************************************************//
  ISpeechResourceLoader = interface(IDispatch)
    ['{B9AC5783-FCD0-4B21-B119-B4F8DA8FD2C3}']
    procedure LoadResource(const bstrResourceUri: WideString; fAlwaysReload: WordBool; 
                           out pStream: IUnknown; out pbstrMIMEType: WideString; 
                           out pfModified: WordBool; out pbstrRedirectUrl: WideString); safecall;
    procedure GetLocalCopy(const bstrResourceUri: WideString; out pbstrLocalPath: WideString; 
                           out pbstrMIMEType: WideString; out pbstrRedirectUrl: WideString); safecall;
    procedure ReleaseLocalCopy(const pbstrLocalPath: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  ISpeechResourceLoaderDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B9AC5783-FCD0-4B21-B119-B4F8DA8FD2C3}
// *********************************************************************//
  ISpeechResourceLoaderDisp = dispinterface
    ['{B9AC5783-FCD0-4B21-B119-B4F8DA8FD2C3}']
    procedure LoadResource(const bstrResourceUri: WideString; fAlwaysReload: WordBool; 
                           out pStream: IUnknown; out pbstrMIMEType: WideString; 
                           out pfModified: WordBool; out pbstrRedirectUrl: WideString); dispid 1;
    procedure GetLocalCopy(const bstrResourceUri: WideString; out pbstrLocalPath: WideString; 
                           out pbstrMIMEType: WideString; out pbstrRedirectUrl: WideString); dispid 2;
    procedure ReleaseLocalCopy(const pbstrLocalPath: WideString); dispid 3;
  end;

// *********************************************************************//
// Interface: IInternetSecurityManager
// Flags:     (0)
// GUID:      {79EAC9EE-BAF9-11CE-8C82-00AA004BA90B}
// *********************************************************************//
  IInternetSecurityManager = interface(IUnknown)
    ['{79EAC9EE-BAF9-11CE-8C82-00AA004BA90B}']
    function SetSecuritySite(const pSite: IInternetSecurityMgrSite): HResult; stdcall;
    function GetSecuritySite(out ppSite: IInternetSecurityMgrSite): HResult; stdcall;
    function MapUrlToZone(pwszUrl: PWideChar; out pdwZone: LongWord; dwFlags: LongWord): HResult; stdcall;
    function GetSecurityId(pwszUrl: PWideChar; out pbSecurityId: Byte; var pcbSecurityId: LongWord; 
                           dwReserved: ULONG_PTR): HResult; stdcall;
    function ProcessUrlAction(pwszUrl: PWideChar; dwAction: LongWord; out pPolicy: Byte; 
                              cbPolicy: LongWord; var pContext: Byte; cbContext: LongWord; 
                              dwFlags: LongWord; dwReserved: LongWord): HResult; stdcall;
    function QueryCustomPolicy(pwszUrl: PWideChar; var guidKey: TGUID; out ppPolicy: PByte1; 
                               out pcbPolicy: LongWord; var pContext: Byte; cbContext: LongWord; 
                               dwReserved: LongWord): HResult; stdcall;
    function SetZoneMapping(dwZone: LongWord; lpszPattern: PWideChar; dwFlags: LongWord): HResult; stdcall;
    function GetZoneMappings(dwZone: LongWord; out ppenumString: IEnumString; dwFlags: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IInternetSecurityMgrSite
// Flags:     (0)
// GUID:      {79EAC9ED-BAF9-11CE-8C82-00AA004BA90B}
// *********************************************************************//
  IInternetSecurityMgrSite = interface(IUnknown)
    ['{79EAC9ED-BAF9-11CE-8C82-00AA004BA90B}']
    function GetWindow(out phwnd: wireHWND): HResult; stdcall;
    function EnableModeless(fEnable: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumString
// Flags:     (0)
// GUID:      {00000101-0000-0000-C000-000000000046}
// *********************************************************************//
  IEnumString = interface(IUnknown)
    ['{00000101-0000-0000-C000-000000000046}']
    function RemoteNext(celt: LongWord; out rgelt: PWideChar; out pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppEnum: IEnumString): HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoSpNotifyTranslator provides a Create and CreateRemote method to          
// create instances of the default interface ISpNotifyTranslator exposed by              
// the CoClass SpNotifyTranslator. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpNotifyTranslator = class
    class function Create: ISpNotifyTranslator;
    class function CreateRemote(const MachineName: string): ISpNotifyTranslator;
  end;

// *********************************************************************//
// The Class CoSpObjectTokenCategory provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechObjectTokenCategory exposed by              
// the CoClass SpObjectTokenCategory. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpObjectTokenCategory = class
    class function Create: ISpeechObjectTokenCategory;
    class function CreateRemote(const MachineName: string): ISpeechObjectTokenCategory;
  end;

// *********************************************************************//
// The Class CoSpObjectToken provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechObjectToken exposed by              
// the CoClass SpObjectToken. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpObjectToken = class
    class function Create: ISpeechObjectToken;
    class function CreateRemote(const MachineName: string): ISpeechObjectToken;
  end;

// *********************************************************************//
// The Class CoSpResourceManager provides a Create and CreateRemote method to          
// create instances of the default interface ISpResourceManager exposed by              
// the CoClass SpResourceManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpResourceManager = class
    class function Create: ISpResourceManager;
    class function CreateRemote(const MachineName: string): ISpResourceManager;
  end;

// *********************************************************************//
// The Class CoSpStreamFormatConverter provides a Create and CreateRemote method to          
// create instances of the default interface ISpStreamFormatConverter exposed by              
// the CoClass SpStreamFormatConverter. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpStreamFormatConverter = class
    class function Create: ISpStreamFormatConverter;
    class function CreateRemote(const MachineName: string): ISpStreamFormatConverter;
  end;

// *********************************************************************//
// The Class CoSpMMAudioEnum provides a Create and CreateRemote method to          
// create instances of the default interface IEnumSpObjectTokens exposed by              
// the CoClass SpMMAudioEnum. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpMMAudioEnum = class
    class function Create: IEnumSpObjectTokens;
    class function CreateRemote(const MachineName: string): IEnumSpObjectTokens;
  end;

// *********************************************************************//
// The Class CoSpMMAudioIn provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechMMSysAudio exposed by              
// the CoClass SpMMAudioIn. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpMMAudioIn = class
    class function Create: ISpeechMMSysAudio;
    class function CreateRemote(const MachineName: string): ISpeechMMSysAudio;
  end;

// *********************************************************************//
// The Class CoSpMMAudioOut provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechMMSysAudio exposed by              
// the CoClass SpMMAudioOut. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpMMAudioOut = class
    class function Create: ISpeechMMSysAudio;
    class function CreateRemote(const MachineName: string): ISpeechMMSysAudio;
  end;

// *********************************************************************//
// The Class CoSpStream provides a Create and CreateRemote method to          
// create instances of the default interface ISpStream exposed by              
// the CoClass SpStream. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpStream = class
    class function Create: ISpStream;
    class function CreateRemote(const MachineName: string): ISpStream;
  end;

// *********************************************************************//
// The Class CoSpVoice provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechVoice exposed by              
// the CoClass SpVoice. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpVoice = class
    class function Create: ISpeechVoice;
    class function CreateRemote(const MachineName: string): ISpeechVoice;
  end;

// *********************************************************************//
// The Class CoSpSharedRecoContext provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechRecoContext exposed by              
// the CoClass SpSharedRecoContext. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpSharedRecoContext = class
    class function Create: ISpeechRecoContext;
    class function CreateRemote(const MachineName: string): ISpeechRecoContext;
  end;

// *********************************************************************//
// The Class CoSpInprocRecognizer provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechRecognizer exposed by              
// the CoClass SpInprocRecognizer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpInprocRecognizer = class
    class function Create: ISpeechRecognizer;
    class function CreateRemote(const MachineName: string): ISpeechRecognizer;
  end;

// *********************************************************************//
// The Class CoSpSharedRecognizer provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechRecognizer exposed by              
// the CoClass SpSharedRecognizer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpSharedRecognizer = class
    class function Create: ISpeechRecognizer;
    class function CreateRemote(const MachineName: string): ISpeechRecognizer;
  end;

// *********************************************************************//
// The Class CoSpLexicon provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechLexicon exposed by              
// the CoClass SpLexicon. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpLexicon = class
    class function Create: ISpeechLexicon;
    class function CreateRemote(const MachineName: string): ISpeechLexicon;
  end;

// *********************************************************************//
// The Class CoSpUnCompressedLexicon provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechLexicon exposed by              
// the CoClass SpUnCompressedLexicon. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpUnCompressedLexicon = class
    class function Create: ISpeechLexicon;
    class function CreateRemote(const MachineName: string): ISpeechLexicon;
  end;

// *********************************************************************//
// The Class CoSpCompressedLexicon provides a Create and CreateRemote method to          
// create instances of the default interface ISpLexicon exposed by              
// the CoClass SpCompressedLexicon. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpCompressedLexicon = class
    class function Create: ISpLexicon;
    class function CreateRemote(const MachineName: string): ISpLexicon;
  end;

// *********************************************************************//
// The Class CoSpShortcut provides a Create and CreateRemote method to          
// create instances of the default interface ISpShortcut exposed by              
// the CoClass SpShortcut. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpShortcut = class
    class function Create: ISpShortcut;
    class function CreateRemote(const MachineName: string): ISpShortcut;
  end;

// *********************************************************************//
// The Class CoSpPhoneConverter provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechPhoneConverter exposed by              
// the CoClass SpPhoneConverter. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpPhoneConverter = class
    class function Create: ISpeechPhoneConverter;
    class function CreateRemote(const MachineName: string): ISpeechPhoneConverter;
  end;

// *********************************************************************//
// The Class CoSpPhoneticAlphabetConverter provides a Create and CreateRemote method to          
// create instances of the default interface ISpPhoneticAlphabetConverter exposed by              
// the CoClass SpPhoneticAlphabetConverter. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpPhoneticAlphabetConverter = class
    class function Create: ISpPhoneticAlphabetConverter;
    class function CreateRemote(const MachineName: string): ISpPhoneticAlphabetConverter;
  end;

// *********************************************************************//
// The Class CoSpNullPhoneConverter provides a Create and CreateRemote method to          
// create instances of the default interface ISpPhoneConverter exposed by              
// the CoClass SpNullPhoneConverter. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpNullPhoneConverter = class
    class function Create: ISpPhoneConverter;
    class function CreateRemote(const MachineName: string): ISpPhoneConverter;
  end;

// *********************************************************************//
// The Class CoSpTextSelectionInformation provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechTextSelectionInformation exposed by              
// the CoClass SpTextSelectionInformation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpTextSelectionInformation = class
    class function Create: ISpeechTextSelectionInformation;
    class function CreateRemote(const MachineName: string): ISpeechTextSelectionInformation;
  end;

// *********************************************************************//
// The Class CoSpPhraseInfoBuilder provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechPhraseInfoBuilder exposed by              
// the CoClass SpPhraseInfoBuilder. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpPhraseInfoBuilder = class
    class function Create: ISpeechPhraseInfoBuilder;
    class function CreateRemote(const MachineName: string): ISpeechPhraseInfoBuilder;
  end;

// *********************************************************************//
// The Class CoSpAudioFormat provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechAudioFormat exposed by              
// the CoClass SpAudioFormat. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpAudioFormat = class
    class function Create: ISpeechAudioFormat;
    class function CreateRemote(const MachineName: string): ISpeechAudioFormat;
  end;

// *********************************************************************//
// The Class CoSpWaveFormatEx provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechWaveFormatEx exposed by              
// the CoClass SpWaveFormatEx. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpWaveFormatEx = class
    class function Create: ISpeechWaveFormatEx;
    class function CreateRemote(const MachineName: string): ISpeechWaveFormatEx;
  end;

// *********************************************************************//
// The Class CoSpInProcRecoContext provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechRecoContext exposed by              
// the CoClass SpInProcRecoContext. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpInProcRecoContext = class
    class function Create: ISpeechRecoContext;
    class function CreateRemote(const MachineName: string): ISpeechRecoContext;
  end;

// *********************************************************************//
// The Class CoSpCustomStream provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechCustomStream exposed by              
// the CoClass SpCustomStream. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpCustomStream = class
    class function Create: ISpeechCustomStream;
    class function CreateRemote(const MachineName: string): ISpeechCustomStream;
  end;

// *********************************************************************//
// The Class CoSpFileStream provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechFileStream exposed by              
// the CoClass SpFileStream. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpFileStream = class
    class function Create: ISpeechFileStream;
    class function CreateRemote(const MachineName: string): ISpeechFileStream;
  end;

// *********************************************************************//
// The Class CoSpMemoryStream provides a Create and CreateRemote method to          
// create instances of the default interface ISpeechMemoryStream exposed by              
// the CoClass SpMemoryStream. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSpMemoryStream = class
    class function Create: ISpeechMemoryStream;
    class function CreateRemote(const MachineName: string): ISpeechMemoryStream;
  end;

implementation

uses System.Win.ComObj;

class function CoSpNotifyTranslator.Create: ISpNotifyTranslator;
begin
  Result := CreateComObject(CLASS_SpNotifyTranslator) as ISpNotifyTranslator;
end;

class function CoSpNotifyTranslator.CreateRemote(const MachineName: string): ISpNotifyTranslator;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpNotifyTranslator) as ISpNotifyTranslator;
end;

class function CoSpObjectTokenCategory.Create: ISpeechObjectTokenCategory;
begin
  Result := CreateComObject(CLASS_SpObjectTokenCategory) as ISpeechObjectTokenCategory;
end;

class function CoSpObjectTokenCategory.CreateRemote(const MachineName: string): ISpeechObjectTokenCategory;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpObjectTokenCategory) as ISpeechObjectTokenCategory;
end;

class function CoSpObjectToken.Create: ISpeechObjectToken;
begin
  Result := CreateComObject(CLASS_SpObjectToken) as ISpeechObjectToken;
end;

class function CoSpObjectToken.CreateRemote(const MachineName: string): ISpeechObjectToken;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpObjectToken) as ISpeechObjectToken;
end;

class function CoSpResourceManager.Create: ISpResourceManager;
begin
  Result := CreateComObject(CLASS_SpResourceManager) as ISpResourceManager;
end;

class function CoSpResourceManager.CreateRemote(const MachineName: string): ISpResourceManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpResourceManager) as ISpResourceManager;
end;

class function CoSpStreamFormatConverter.Create: ISpStreamFormatConverter;
begin
  Result := CreateComObject(CLASS_SpStreamFormatConverter) as ISpStreamFormatConverter;
end;

class function CoSpStreamFormatConverter.CreateRemote(const MachineName: string): ISpStreamFormatConverter;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpStreamFormatConverter) as ISpStreamFormatConverter;
end;

class function CoSpMMAudioEnum.Create: IEnumSpObjectTokens;
begin
  Result := CreateComObject(CLASS_SpMMAudioEnum) as IEnumSpObjectTokens;
end;

class function CoSpMMAudioEnum.CreateRemote(const MachineName: string): IEnumSpObjectTokens;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpMMAudioEnum) as IEnumSpObjectTokens;
end;

class function CoSpMMAudioIn.Create: ISpeechMMSysAudio;
begin
  Result := CreateComObject(CLASS_SpMMAudioIn) as ISpeechMMSysAudio;
end;

class function CoSpMMAudioIn.CreateRemote(const MachineName: string): ISpeechMMSysAudio;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpMMAudioIn) as ISpeechMMSysAudio;
end;

class function CoSpMMAudioOut.Create: ISpeechMMSysAudio;
begin
  Result := CreateComObject(CLASS_SpMMAudioOut) as ISpeechMMSysAudio;
end;

class function CoSpMMAudioOut.CreateRemote(const MachineName: string): ISpeechMMSysAudio;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpMMAudioOut) as ISpeechMMSysAudio;
end;

class function CoSpStream.Create: ISpStream;
begin
  Result := CreateComObject(CLASS_SpStream) as ISpStream;
end;

class function CoSpStream.CreateRemote(const MachineName: string): ISpStream;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpStream) as ISpStream;
end;

class function CoSpVoice.Create: ISpeechVoice;
begin
  Result := CreateComObject(CLASS_SpVoice) as ISpeechVoice;
end;

class function CoSpVoice.CreateRemote(const MachineName: string): ISpeechVoice;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpVoice) as ISpeechVoice;
end;

class function CoSpSharedRecoContext.Create: ISpeechRecoContext;
begin
  Result := CreateComObject(CLASS_SpSharedRecoContext) as ISpeechRecoContext;
end;

class function CoSpSharedRecoContext.CreateRemote(const MachineName: string): ISpeechRecoContext;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpSharedRecoContext) as ISpeechRecoContext;
end;

class function CoSpInprocRecognizer.Create: ISpeechRecognizer;
begin
  Result := CreateComObject(CLASS_SpInprocRecognizer) as ISpeechRecognizer;
end;

class function CoSpInprocRecognizer.CreateRemote(const MachineName: string): ISpeechRecognizer;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpInprocRecognizer) as ISpeechRecognizer;
end;

class function CoSpSharedRecognizer.Create: ISpeechRecognizer;
begin
  Result := CreateComObject(CLASS_SpSharedRecognizer) as ISpeechRecognizer;
end;

class function CoSpSharedRecognizer.CreateRemote(const MachineName: string): ISpeechRecognizer;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpSharedRecognizer) as ISpeechRecognizer;
end;

class function CoSpLexicon.Create: ISpeechLexicon;
begin
  Result := CreateComObject(CLASS_SpLexicon) as ISpeechLexicon;
end;

class function CoSpLexicon.CreateRemote(const MachineName: string): ISpeechLexicon;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpLexicon) as ISpeechLexicon;
end;

class function CoSpUnCompressedLexicon.Create: ISpeechLexicon;
begin
  Result := CreateComObject(CLASS_SpUnCompressedLexicon) as ISpeechLexicon;
end;

class function CoSpUnCompressedLexicon.CreateRemote(const MachineName: string): ISpeechLexicon;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpUnCompressedLexicon) as ISpeechLexicon;
end;

class function CoSpCompressedLexicon.Create: ISpLexicon;
begin
  Result := CreateComObject(CLASS_SpCompressedLexicon) as ISpLexicon;
end;

class function CoSpCompressedLexicon.CreateRemote(const MachineName: string): ISpLexicon;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpCompressedLexicon) as ISpLexicon;
end;

class function CoSpShortcut.Create: ISpShortcut;
begin
  Result := CreateComObject(CLASS_SpShortcut) as ISpShortcut;
end;

class function CoSpShortcut.CreateRemote(const MachineName: string): ISpShortcut;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpShortcut) as ISpShortcut;
end;

class function CoSpPhoneConverter.Create: ISpeechPhoneConverter;
begin
  Result := CreateComObject(CLASS_SpPhoneConverter) as ISpeechPhoneConverter;
end;

class function CoSpPhoneConverter.CreateRemote(const MachineName: string): ISpeechPhoneConverter;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpPhoneConverter) as ISpeechPhoneConverter;
end;

class function CoSpPhoneticAlphabetConverter.Create: ISpPhoneticAlphabetConverter;
begin
  Result := CreateComObject(CLASS_SpPhoneticAlphabetConverter) as ISpPhoneticAlphabetConverter;
end;

class function CoSpPhoneticAlphabetConverter.CreateRemote(const MachineName: string): ISpPhoneticAlphabetConverter;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpPhoneticAlphabetConverter) as ISpPhoneticAlphabetConverter;
end;

class function CoSpNullPhoneConverter.Create: ISpPhoneConverter;
begin
  Result := CreateComObject(CLASS_SpNullPhoneConverter) as ISpPhoneConverter;
end;

class function CoSpNullPhoneConverter.CreateRemote(const MachineName: string): ISpPhoneConverter;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpNullPhoneConverter) as ISpPhoneConverter;
end;

class function CoSpTextSelectionInformation.Create: ISpeechTextSelectionInformation;
begin
  Result := CreateComObject(CLASS_SpTextSelectionInformation) as ISpeechTextSelectionInformation;
end;

class function CoSpTextSelectionInformation.CreateRemote(const MachineName: string): ISpeechTextSelectionInformation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpTextSelectionInformation) as ISpeechTextSelectionInformation;
end;

class function CoSpPhraseInfoBuilder.Create: ISpeechPhraseInfoBuilder;
begin
  Result := CreateComObject(CLASS_SpPhraseInfoBuilder) as ISpeechPhraseInfoBuilder;
end;

class function CoSpPhraseInfoBuilder.CreateRemote(const MachineName: string): ISpeechPhraseInfoBuilder;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpPhraseInfoBuilder) as ISpeechPhraseInfoBuilder;
end;

class function CoSpAudioFormat.Create: ISpeechAudioFormat;
begin
  Result := CreateComObject(CLASS_SpAudioFormat) as ISpeechAudioFormat;
end;

class function CoSpAudioFormat.CreateRemote(const MachineName: string): ISpeechAudioFormat;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpAudioFormat) as ISpeechAudioFormat;
end;

class function CoSpWaveFormatEx.Create: ISpeechWaveFormatEx;
begin
  Result := CreateComObject(CLASS_SpWaveFormatEx) as ISpeechWaveFormatEx;
end;

class function CoSpWaveFormatEx.CreateRemote(const MachineName: string): ISpeechWaveFormatEx;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpWaveFormatEx) as ISpeechWaveFormatEx;
end;

class function CoSpInProcRecoContext.Create: ISpeechRecoContext;
begin
  Result := CreateComObject(CLASS_SpInProcRecoContext) as ISpeechRecoContext;
end;

class function CoSpInProcRecoContext.CreateRemote(const MachineName: string): ISpeechRecoContext;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpInProcRecoContext) as ISpeechRecoContext;
end;

class function CoSpCustomStream.Create: ISpeechCustomStream;
begin
  Result := CreateComObject(CLASS_SpCustomStream) as ISpeechCustomStream;
end;

class function CoSpCustomStream.CreateRemote(const MachineName: string): ISpeechCustomStream;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpCustomStream) as ISpeechCustomStream;
end;

class function CoSpFileStream.Create: ISpeechFileStream;
begin
  Result := CreateComObject(CLASS_SpFileStream) as ISpeechFileStream;
end;

class function CoSpFileStream.CreateRemote(const MachineName: string): ISpeechFileStream;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpFileStream) as ISpeechFileStream;
end;

class function CoSpMemoryStream.Create: ISpeechMemoryStream;
begin
  Result := CreateComObject(CLASS_SpMemoryStream) as ISpeechMemoryStream;
end;

class function CoSpMemoryStream.CreateRemote(const MachineName: string): ISpeechMemoryStream;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SpMemoryStream) as ISpeechMemoryStream;
end;

end.
