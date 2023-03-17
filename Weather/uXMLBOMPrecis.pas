
{***************************************************************}
{                                                               }
{                       XML Data Binding                        }
{                                                               }
{         Generated on: 12/02/2023 8:10:31 AM                   }
{       Generated from: C:\Users\geoff\Downloads\IDV10753.xml   }
{                                                               }
{***************************************************************}

unit uXMLBOMPrecis;

interface

uses Xml.xmldom, Xml.XMLDoc, Xml.XMLIntf;

type

{ Forward Decls }

  IXMLProductType = interface;
  IXMLAmocType = interface;
  IXMLSourceType = interface;
  IXMLTimestampType = interface;
  IXMLPeriodicHazardType = interface;
  IXMLPeriodicHazardTypeList = interface;
  IXMLArealistType = interface;
  IXMLArea = interface;
  IXMLPhenomenonlistType = interface;
  IXMLPhenomenon = interface;
  IXMLText = interface;
  IXMLTextList = interface;
  IXMLHazardType = interface;
  IXMLHazardTypeList = interface;
  IXMLWarningType = interface;
  IXMLWarninginfoType = interface;
  IXMLTextelementType = interface;
  IXMLTextelementTypeList = interface;
  IXMLAreaType = interface;
  IXMLAreaTypeList = interface;
  IXMLPeriodicwarningsummaryType = interface;
  IXMLPeriodicwarningsummaryTypeList = interface;
  IXMLWarningsummaryType = interface;
  IXMLWarningsummaryTypeList = interface;
  IXMLForecastperiodType = interface;
  IXMLForecastperiodTypeList = interface;
  IXMLElementType = interface;
  IXMLElementTypeList = interface;
  IXMLWarninggroupType = interface;
  IXMLWarninggroupTypeList = interface;
  IXMLForecastType = interface;
  IXMLWarningsummariesType = interface;
  IXMLObservationsType = interface;
  IXMLStationType = interface;
  IXMLObservationperiodType = interface;
  IXMLLevelType = interface;
  IXMLLevelTypeList = interface;

{ IXMLProductType }

  IXMLProductType = interface(IXMLNode)
    ['{4E3DFE57-9F30-455A-BA50-6C05A6E0EA51}']
    { Property Accessors }
    function Get_Version: UnicodeString;
    function Get_Amoc: IXMLAmocType;
    function Get_Warning: IXMLWarningType;
    function Get_Forecast: IXMLForecastType;
    function Get_Warningsummaries: IXMLWarningsummariesType;
    function Get_Observations: IXMLObservationsType;
    procedure Set_Version(const Value: UnicodeString);
    { Methods & Properties }
    property Version: UnicodeString read Get_Version write Set_Version;
    property Amoc: IXMLAmocType read Get_Amoc;
    property Warning: IXMLWarningType read Get_Warning;
    property Forecast: IXMLForecastType read Get_Forecast;
    property Warningsummaries: IXMLWarningsummariesType read Get_Warningsummaries;
    property Observations: IXMLObservationsType read Get_Observations;
  end;

{ IXMLAmocType }

  IXMLAmocType = interface(IXMLNode)
    ['{D95E1E4E-D56F-4773-B479-8921CEE12AF3}']
    { Property Accessors }
    function Get_Version: UnicodeString;
    function Get_Source: IXMLSourceType;
    function Get_Identifier: UnicodeString;
    function Get_Issuetimeutc: IXMLTimestampType;
    function Get_Issuetimelocal: IXMLTimestampType;
    function Get_Senttime: UnicodeString;
    function Get_Expirytime: UnicodeString;
    function Get_Validitybgntimelocal: IXMLTimestampType;
    function Get_Validityendtimelocal: IXMLTimestampType;
    function Get_Nextroutineissuetimeutc: IXMLTimestampType;
    function Get_Nextroutineissuetimelocal: IXMLTimestampType;
    function Get_Status: UnicodeString;
    function Get_Service: UnicodeString;
    function Get_Subservice: UnicodeString;
    function Get_Producttype: UnicodeString;
    function Get_Phase: UnicodeString;
    function Get_Hazard: IXMLHazardTypeList;
    function Get_Incidentid: UnicodeString;
    procedure Set_Version(const Value: UnicodeString);
    procedure Set_Identifier(const Value: UnicodeString);
    procedure Set_Senttime(const Value: UnicodeString);
    procedure Set_Expirytime(const Value: UnicodeString);
    procedure Set_Status(const Value: UnicodeString);
    procedure Set_Service(const Value: UnicodeString);
    procedure Set_Subservice(const Value: UnicodeString);
    procedure Set_Producttype(const Value: UnicodeString);
    procedure Set_Phase(const Value: UnicodeString);
    procedure Set_Incidentid(const Value: UnicodeString);
    { Methods & Properties }
    property Version: UnicodeString read Get_Version write Set_Version;
    property Source: IXMLSourceType read Get_Source;
    property Identifier: UnicodeString read Get_Identifier write Set_Identifier;
    property Issuetimeutc: IXMLTimestampType read Get_Issuetimeutc;
    property Issuetimelocal: IXMLTimestampType read Get_Issuetimelocal;
    property Senttime: UnicodeString read Get_Senttime write Set_Senttime;
    property Expirytime: UnicodeString read Get_Expirytime write Set_Expirytime;
    property Validitybgntimelocal: IXMLTimestampType read Get_Validitybgntimelocal;
    property Validityendtimelocal: IXMLTimestampType read Get_Validityendtimelocal;
    property Nextroutineissuetimeutc: IXMLTimestampType read Get_Nextroutineissuetimeutc;
    property Nextroutineissuetimelocal: IXMLTimestampType read Get_Nextroutineissuetimelocal;
    property Status: UnicodeString read Get_Status write Set_Status;
    property Service: UnicodeString read Get_Service write Set_Service;
    property Subservice: UnicodeString read Get_Subservice write Set_Subservice;
    property Producttype: UnicodeString read Get_Producttype write Set_Producttype;
    property Phase: UnicodeString read Get_Phase write Set_Phase;
    property Hazard: IXMLHazardTypeList read Get_Hazard;
    property Incidentid: UnicodeString read Get_Incidentid write Set_Incidentid;
  end;

{ IXMLSourceType }

  IXMLSourceType = interface(IXMLNode)
    ['{3C51DE9D-0362-4997-A60D-4CE093EC905F}']
    { Property Accessors }
    function Get_Sender: UnicodeString;
    function Get_Region: UnicodeString;
    function Get_Office: UnicodeString;
    function Get_Copyright: UnicodeString;
    function Get_Disclaimer: UnicodeString;
    function Get_Description: UnicodeString;
    procedure Set_Sender(const Value: UnicodeString);
    procedure Set_Region(const Value: UnicodeString);
    procedure Set_Office(const Value: UnicodeString);
    procedure Set_Copyright(const Value: UnicodeString);
    procedure Set_Disclaimer(const Value: UnicodeString);
    procedure Set_Description(const Value: UnicodeString);
    { Methods & Properties }
    property Sender: UnicodeString read Get_Sender write Set_Sender;
    property Region: UnicodeString read Get_Region write Set_Region;
    property Office: UnicodeString read Get_Office write Set_Office;
    property Copyright: UnicodeString read Get_Copyright write Set_Copyright;
    property Disclaimer: UnicodeString read Get_Disclaimer write Set_Disclaimer;
    property Description: UnicodeString read Get_Description write Set_Description;
  end;

{ IXMLTimestampType }

  IXMLTimestampType = interface(IXMLNode)
    ['{BEBAFFA9-8B06-4AEA-8824-5AE00957AA79}']
    { Property Accessors }
    function Get_Tz: UnicodeString;
    procedure Set_Tz(const Value: UnicodeString);
    { Methods & Properties }
    property Tz: UnicodeString read Get_Tz write Set_Tz;
  end;

{ IXMLPeriodicHazardType }

  IXMLPeriodicHazardType = interface(IXMLNode)
    ['{2A5989B3-E071-44F4-8509-DFF3C8AE4364}']
    { Property Accessors }
    function Get_Index: Integer;
    function Get_Type_: UnicodeString;
    function Get_Severity: UnicodeString;
    function Get_Urgency: UnicodeString;
    function Get_Certainty: UnicodeString;
    function Get_Phase: UnicodeString;
    function Get_Parentaac: UnicodeString;
    function Get_Arealist: IXMLArealistType;
    function Get_Phenomenonlist: IXMLPhenomenonlistType;
    function Get_Priority: UnicodeString;
    function Get_Headline: UnicodeString;
    function Get_Text: IXMLTextList;
    procedure Set_Index(const Value: Integer);
    procedure Set_Type_(const Value: UnicodeString);
    procedure Set_Severity(const Value: UnicodeString);
    procedure Set_Urgency(const Value: UnicodeString);
    procedure Set_Certainty(const Value: UnicodeString);
    procedure Set_Phase(const Value: UnicodeString);
    procedure Set_Parentaac(const Value: UnicodeString);
    procedure Set_Priority(const Value: UnicodeString);
    procedure Set_Headline(const Value: UnicodeString);
    { Methods & Properties }
    property Index: Integer read Get_Index write Set_Index;
    property Type_: UnicodeString read Get_Type_ write Set_Type_;
    property Severity: UnicodeString read Get_Severity write Set_Severity;
    property Urgency: UnicodeString read Get_Urgency write Set_Urgency;
    property Certainty: UnicodeString read Get_Certainty write Set_Certainty;
    property Phase: UnicodeString read Get_Phase write Set_Phase;
    property Parentaac: UnicodeString read Get_Parentaac write Set_Parentaac;
    property Arealist: IXMLArealistType read Get_Arealist;
    property Phenomenonlist: IXMLPhenomenonlistType read Get_Phenomenonlist;
    property Priority: UnicodeString read Get_Priority write Set_Priority;
    property Headline: UnicodeString read Get_Headline write Set_Headline;
    property Text: IXMLTextList read Get_Text;
  end;

{ IXMLPeriodicHazardTypeList }

  IXMLPeriodicHazardTypeList = interface(IXMLNodeCollection)
    ['{E4A46881-8E89-4257-95CC-DA63722240C9}']
    { Methods & Properties }
    function Add: IXMLPeriodicHazardType;
    function Insert(const Index: Integer): IXMLPeriodicHazardType;

    function Get_Item(const Index: Integer): IXMLPeriodicHazardType;
    property Items[const Index: Integer]: IXMLPeriodicHazardType read Get_Item; default;
  end;

{ IXMLArealistType }

  IXMLArealistType = interface(IXMLNodeCollection)
    ['{EC7293E7-2EEE-4F5B-8EFD-0875574C4DF6}']
    { Property Accessors }
    function Get_Area(const Index: Integer): IXMLArea;
    { Methods & Properties }
    function Add: IXMLArea;
    function Insert(const Index: Integer): IXMLArea;
    property Area[const Index: Integer]: IXMLArea read Get_Area; default;
  end;

{ IXMLArea }

  IXMLArea = interface(IXMLNode)
    ['{4D9A5450-F305-4965-8ED4-0209262C2DE1}']
    { Property Accessors }
    function Get_Aac: UnicodeString;
    function Get_Phase: UnicodeString;
    function Get_Description: UnicodeString;
    function Get_Type_: UnicodeString;
    procedure Set_Aac(const Value: UnicodeString);
    procedure Set_Phase(const Value: UnicodeString);
    procedure Set_Description(const Value: UnicodeString);
    procedure Set_Type_(const Value: UnicodeString);
    { Methods & Properties }
    property Aac: UnicodeString read Get_Aac write Set_Aac;
    property Phase: UnicodeString read Get_Phase write Set_Phase;
    property Description: UnicodeString read Get_Description write Set_Description;
    property Type_: UnicodeString read Get_Type_ write Set_Type_;
  end;

{ IXMLPhenomenonlistType }

  IXMLPhenomenonlistType = interface(IXMLNodeCollection)
    ['{6335DF48-C0D3-4F5D-B6D3-62AE94415CCA}']
    { Property Accessors }
    function Get_Phenomenon(const Index: Integer): IXMLPhenomenon;
    { Methods & Properties }
    function Add: IXMLPhenomenon;
    function Insert(const Index: Integer): IXMLPhenomenon;
    property Phenomenon[const Index: Integer]: IXMLPhenomenon read Get_Phenomenon; default;
  end;

{ IXMLPhenomenon }

  IXMLPhenomenon = interface(IXMLNode)
    ['{FE3CF5AE-0F62-4F6B-B467-4DD5D60DE8B8}']
    { Property Accessors }
    function Get_Type_: UnicodeString;
    procedure Set_Type_(const Value: UnicodeString);
    { Methods & Properties }
    property Type_: UnicodeString read Get_Type_ write Set_Type_;
  end;

{ IXMLText }

  IXMLText = interface(IXMLNodeCollection)
    ['{44DA44B9-5015-4BF2-9AA7-E0E07F90A74D}']
    { Property Accessors }
    function Get_Type_: UnicodeString;
    function Get_P(const Index: Integer): UnicodeString;
    procedure Set_Type_(const Value: UnicodeString);
    { Methods & Properties }
    function Add(const P: UnicodeString): IXMLNode;
    function Insert(const Index: Integer; const P: UnicodeString): IXMLNode;
    property Type_: UnicodeString read Get_Type_ write Set_Type_;
    property P[const Index: Integer]: UnicodeString read Get_P; default;
  end;

{ IXMLTextList }

  IXMLTextList = interface(IXMLNodeCollection)
    ['{7A78F685-325A-418B-BACE-B865BF5FD281}']
    { Methods & Properties }
    function Add: IXMLText;
    function Insert(const Index: Integer): IXMLText;

    function Get_Item(const Index: Integer): IXMLText;
    property Items[const Index: Integer]: IXMLText read Get_Item; default;
  end;

{ IXMLHazardType }

  IXMLHazardType = interface(IXMLPeriodicHazardType)
    ['{E71FE436-53D9-4375-A697-5264140B99D0}']
    { Property Accessors }
    function Get_Starttimeutc: UnicodeString;
    function Get_Starttimelocal: UnicodeString;
    function Get_Endtimeutc: UnicodeString;
    function Get_Endtimelocal: UnicodeString;
    procedure Set_Starttimeutc(const Value: UnicodeString);
    procedure Set_Starttimelocal(const Value: UnicodeString);
    procedure Set_Endtimeutc(const Value: UnicodeString);
    procedure Set_Endtimelocal(const Value: UnicodeString);
    { Methods & Properties }
    property Starttimeutc: UnicodeString read Get_Starttimeutc write Set_Starttimeutc;
    property Starttimelocal: UnicodeString read Get_Starttimelocal write Set_Starttimelocal;
    property Endtimeutc: UnicodeString read Get_Endtimeutc write Set_Endtimeutc;
    property Endtimelocal: UnicodeString read Get_Endtimelocal write Set_Endtimelocal;
  end;

{ IXMLHazardTypeList }

  IXMLHazardTypeList = interface(IXMLNodeCollection)
    ['{4264DEF0-89E1-4575-B0BE-8055A51B7F4E}']
    { Methods & Properties }
    function Add: IXMLHazardType;
    function Insert(const Index: Integer): IXMLHazardType;

    function Get_Item(const Index: Integer): IXMLHazardType;
    property Items[const Index: Integer]: IXMLHazardType read Get_Item; default;
  end;

{ IXMLWarningType }

  IXMLWarningType = interface(IXMLNode)
    ['{56DC318C-C4B5-43F3-9A3B-09D09BBEDBEE}']
    { Property Accessors }
    function Get_Warninginfo: IXMLWarninginfoType;
    function Get_Area: IXMLAreaTypeList;
    { Methods & Properties }
    property Warninginfo: IXMLWarninginfoType read Get_Warninginfo;
    property Area: IXMLAreaTypeList read Get_Area;
  end;

{ IXMLWarninginfoType }

  IXMLWarninginfoType = interface(IXMLNodeCollection)
    ['{A2025DE5-9B16-4C65-A137-1A77E49F35BD}']
    { Property Accessors }
    function Get_Text(const Index: Integer): IXMLTextelementType;
    { Methods & Properties }
    function Add: IXMLTextelementType;
    function Insert(const Index: Integer): IXMLTextelementType;
    property Text[const Index: Integer]: IXMLTextelementType read Get_Text; default;
  end;

{ IXMLTextelementType }

  IXMLTextelementType = interface(IXMLNodeCollection)
    ['{8BFDB1D3-CEFC-430E-B1D9-F7D07A880AE3}']
    { Property Accessors }
    function Get_Type_: UnicodeString;
    function Get_P(const Index: Integer): UnicodeString;
    procedure Set_Type_(const Value: UnicodeString);
    { Methods & Properties }
    function Add(const P: UnicodeString): IXMLNode;
    function Insert(const Index: Integer; const P: UnicodeString): IXMLNode;
    property Type_: UnicodeString read Get_Type_ write Set_Type_;
    property P[const Index: Integer]: UnicodeString read Get_P; default;
  end;

{ IXMLTextelementTypeList }

  IXMLTextelementTypeList = interface(IXMLNodeCollection)
    ['{07ABA195-D27B-4376-8F13-7FA22FD30EB6}']
    { Methods & Properties }
    function Add: IXMLTextelementType;
    function Insert(const Index: Integer): IXMLTextelementType;

    function Get_Item(const Index: Integer): IXMLTextelementType;
    property Items[const Index: Integer]: IXMLTextelementType read Get_Item; default;
  end;

{ IXMLAreaType }

  IXMLAreaType = interface(IXMLNode)
    ['{1DC0862C-A1BC-46B6-B4BC-9DDF35099953}']
    { Property Accessors }
    function Get_Aac: UnicodeString;
    function Get_Parentaac: UnicodeString;
    function Get_Description: UnicodeString;
    function Get_Type_: UnicodeString;
    function Get_Warningsummary: IXMLWarningsummaryTypeList;
    function Get_Forecastperiod: IXMLForecastperiodTypeList;
    function Get_Hazard: IXMLHazardTypeList;
    function Get_Warninggroup: IXMLWarninggroupTypeList;
    procedure Set_Aac(const Value: UnicodeString);
    procedure Set_Parentaac(const Value: UnicodeString);
    procedure Set_Description(const Value: UnicodeString);
    procedure Set_Type_(const Value: UnicodeString);
    { Methods & Properties }
    property Aac: UnicodeString read Get_Aac write Set_Aac;
    property Parentaac: UnicodeString read Get_Parentaac write Set_Parentaac;
    property Description: UnicodeString read Get_Description write Set_Description;
    property Type_: UnicodeString read Get_Type_ write Set_Type_;
    property Warningsummary: IXMLWarningsummaryTypeList read Get_Warningsummary;
    property Forecastperiod: IXMLForecastperiodTypeList read Get_Forecastperiod;
    property Hazard: IXMLHazardTypeList read Get_Hazard;
    property Warninggroup: IXMLWarninggroupTypeList read Get_Warninggroup;
  end;

{ IXMLAreaTypeList }

  IXMLAreaTypeList = interface(IXMLNodeCollection)
    ['{F60BAB3B-0D4B-46CA-B4F1-CA42D4515189}']
    { Methods & Properties }
    function Add: IXMLAreaType;
    function Insert(const Index: Integer): IXMLAreaType;

    function Get_Item(const Index: Integer): IXMLAreaType;
    property Items[const Index: Integer]: IXMLAreaType read Get_Item; default;
  end;

{ IXMLPeriodicwarningsummaryType }

  IXMLPeriodicwarningsummaryType = interface(IXMLNodeCollection)
    ['{C55999F6-8223-4C23-8804-CCB6892339C8}']
    { Property Accessors }
    function Get_Type_: UnicodeString;
    function Get_Productidentifier: UnicodeString;
    function Get_Producturl: UnicodeString;
    function Get_Aac: UnicodeString;
    function Get_Area: UnicodeString;
    function Get_Areatype: UnicodeString;
    function Get_P(const Index: Integer): UnicodeString;
    procedure Set_Type_(const Value: UnicodeString);
    procedure Set_Productidentifier(const Value: UnicodeString);
    procedure Set_Producturl(const Value: UnicodeString);
    procedure Set_Aac(const Value: UnicodeString);
    procedure Set_Area(const Value: UnicodeString);
    procedure Set_Areatype(const Value: UnicodeString);
    { Methods & Properties }
    function Add(const P: UnicodeString): IXMLNode;
    function Insert(const Index: Integer; const P: UnicodeString): IXMLNode;
    property Type_: UnicodeString read Get_Type_ write Set_Type_;
    property Productidentifier: UnicodeString read Get_Productidentifier write Set_Productidentifier;
    property Producturl: UnicodeString read Get_Producturl write Set_Producturl;
    property Aac: UnicodeString read Get_Aac write Set_Aac;
    property Area: UnicodeString read Get_Area write Set_Area;
    property Areatype: UnicodeString read Get_Areatype write Set_Areatype;
    property P[const Index: Integer]: UnicodeString read Get_P; default;
  end;

{ IXMLPeriodicwarningsummaryTypeList }

  IXMLPeriodicwarningsummaryTypeList = interface(IXMLNodeCollection)
    ['{08965ACD-EC24-4684-BC4F-ECCF201DE266}']
    { Methods & Properties }
    function Add: IXMLPeriodicwarningsummaryType;
    function Insert(const Index: Integer): IXMLPeriodicwarningsummaryType;

    function Get_Item(const Index: Integer): IXMLPeriodicwarningsummaryType;
    property Items[const Index: Integer]: IXMLPeriodicwarningsummaryType read Get_Item; default;
  end;

{ IXMLWarningsummaryType }

  IXMLWarningsummaryType = interface(IXMLPeriodicwarningsummaryType)
    ['{10DD57D6-C228-41EB-8FE7-4F5D0F2CA06C}']
    { Property Accessors }
    function Get_Starttimeutc: UnicodeString;
    function Get_Starttimelocal: UnicodeString;
    function Get_Endtimeutc: UnicodeString;
    function Get_Endtimelocal: UnicodeString;
    procedure Set_Starttimeutc(const Value: UnicodeString);
    procedure Set_Starttimelocal(const Value: UnicodeString);
    procedure Set_Endtimeutc(const Value: UnicodeString);
    procedure Set_Endtimelocal(const Value: UnicodeString);
    { Methods & Properties }
    property Starttimeutc: UnicodeString read Get_Starttimeutc write Set_Starttimeutc;
    property Starttimelocal: UnicodeString read Get_Starttimelocal write Set_Starttimelocal;
    property Endtimeutc: UnicodeString read Get_Endtimeutc write Set_Endtimeutc;
    property Endtimelocal: UnicodeString read Get_Endtimelocal write Set_Endtimelocal;
  end;

{ IXMLWarningsummaryTypeList }

  IXMLWarningsummaryTypeList = interface(IXMLNodeCollection)
    ['{D5A43B9C-E795-4BE5-B39C-9F4F528D8D14}']
    { Methods & Properties }
    function Add: IXMLWarningsummaryType;
    function Insert(const Index: Integer): IXMLWarningsummaryType;

    function Get_Item(const Index: Integer): IXMLWarningsummaryType;
    property Items[const Index: Integer]: IXMLWarningsummaryType read Get_Item; default;
  end;

{ IXMLForecastperiodType }

  IXMLForecastperiodType = interface(IXMLNode)
    ['{E01B83D8-814D-4906-9C1B-A849A4095A80}']
    { Property Accessors }
    function Get_Index: LongWord;
    function Get_Endindex: LongWord;
    function Get_Indextag: UnicodeString;
    function Get_Starttimeutc: UnicodeString;
    function Get_Starttimelocal: UnicodeString;
    function Get_Endtimeutc: UnicodeString;
    function Get_Endtimelocal: UnicodeString;
    function Get_Enddatelocal: UnicodeString;
    function Get_Warningsummary: IXMLPeriodicwarningsummaryTypeList;
    function Get_Element: IXMLElementTypeList;
    function Get_Text: IXMLTextelementTypeList;
    function Get_Hazard: IXMLPeriodicHazardTypeList;
    procedure Set_Index(const Value: LongWord);
    procedure Set_Endindex(const Value: LongWord);
    procedure Set_Indextag(const Value: UnicodeString);
    procedure Set_Starttimeutc(const Value: UnicodeString);
    procedure Set_Starttimelocal(const Value: UnicodeString);
    procedure Set_Endtimeutc(const Value: UnicodeString);
    procedure Set_Endtimelocal(const Value: UnicodeString);
    procedure Set_Enddatelocal(const Value: UnicodeString);
    { Methods & Properties }
    property Index: LongWord read Get_Index write Set_Index;
    property Endindex: LongWord read Get_Endindex write Set_Endindex;
    property Indextag: UnicodeString read Get_Indextag write Set_Indextag;
    property Starttimeutc: UnicodeString read Get_Starttimeutc write Set_Starttimeutc;
    property Starttimelocal: UnicodeString read Get_Starttimelocal write Set_Starttimelocal;
    property Endtimeutc: UnicodeString read Get_Endtimeutc write Set_Endtimeutc;
    property Endtimelocal: UnicodeString read Get_Endtimelocal write Set_Endtimelocal;
    property Enddatelocal: UnicodeString read Get_Enddatelocal write Set_Enddatelocal;
    property Warningsummary: IXMLPeriodicwarningsummaryTypeList read Get_Warningsummary;
    property Element: IXMLElementTypeList read Get_Element;
    property Text: IXMLTextelementTypeList read Get_Text;
    property Hazard: IXMLPeriodicHazardTypeList read Get_Hazard;
  end;

{ IXMLForecastperiodTypeList }

  IXMLForecastperiodTypeList = interface(IXMLNodeCollection)
    ['{B9F13741-285D-4393-81F3-C8A724B637A3}']
    { Methods & Properties }
    function Add: IXMLForecastperiodType;
    function Insert(const Index: Integer): IXMLForecastperiodType;

    function Get_Item(const Index: Integer): IXMLForecastperiodType;
    property Items[const Index: Integer]: IXMLForecastperiodType read Get_Item; default;
  end;

{ IXMLElementType }

  IXMLElementType = interface(IXMLNode)
    ['{9D6F777A-E55E-4312-A594-D6E0D6872960}']
    { Property Accessors }
    function Get_Type_: UnicodeString;
    function Get_Units: UnicodeString;
    function Get_Instance: UnicodeString;
    function Get_Sequence: LongWord;
    function Get_Duration: LongWord;
    function Get_Timeutc: UnicodeString;
    function Get_Timelocal: UnicodeString;
    function Get_Starttimeutc: UnicodeString;
    function Get_Starttimelocal: UnicodeString;
    function Get_Endtimeutc: UnicodeString;
    function Get_Endtimelocal: UnicodeString;
    function Get_Startmonth: UnicodeString;
    function Get_Endmonth: UnicodeString;
    procedure Set_Type_(const Value: UnicodeString);
    procedure Set_Units(const Value: UnicodeString);
    procedure Set_Instance(const Value: UnicodeString);
    procedure Set_Sequence(const Value: LongWord);
    procedure Set_Duration(const Value: LongWord);
    procedure Set_Timeutc(const Value: UnicodeString);
    procedure Set_Timelocal(const Value: UnicodeString);
    procedure Set_Starttimeutc(const Value: UnicodeString);
    procedure Set_Starttimelocal(const Value: UnicodeString);
    procedure Set_Endtimeutc(const Value: UnicodeString);
    procedure Set_Endtimelocal(const Value: UnicodeString);
    procedure Set_Startmonth(const Value: UnicodeString);
    procedure Set_Endmonth(const Value: UnicodeString);
    { Methods & Properties }
    property Type_: UnicodeString read Get_Type_ write Set_Type_;
    property Units: UnicodeString read Get_Units write Set_Units;
    property Instance: UnicodeString read Get_Instance write Set_Instance;
    property Sequence: LongWord read Get_Sequence write Set_Sequence;
    property Duration: LongWord read Get_Duration write Set_Duration;
    property Timeutc: UnicodeString read Get_Timeutc write Set_Timeutc;
    property Timelocal: UnicodeString read Get_Timelocal write Set_Timelocal;
    property Starttimeutc: UnicodeString read Get_Starttimeutc write Set_Starttimeutc;
    property Starttimelocal: UnicodeString read Get_Starttimelocal write Set_Starttimelocal;
    property Endtimeutc: UnicodeString read Get_Endtimeutc write Set_Endtimeutc;
    property Endtimelocal: UnicodeString read Get_Endtimelocal write Set_Endtimelocal;
    property Startmonth: UnicodeString read Get_Startmonth write Set_Startmonth;
    property Endmonth: UnicodeString read Get_Endmonth write Set_Endmonth;
  end;

{ IXMLElementTypeList }

  IXMLElementTypeList = interface(IXMLNodeCollection)
    ['{26B1DB6F-6211-4511-A214-34F069F0F7DB}']
    { Methods & Properties }
    function Add: IXMLElementType;
    function Insert(const Index: Integer): IXMLElementType;

    function Get_Item(const Index: Integer): IXMLElementType;
    property Items[const Index: Integer]: IXMLElementType read Get_Item; default;
  end;

{ IXMLWarninggroupType }

  IXMLWarninggroupType = interface(IXMLNodeCollection)
    ['{857F02A7-D895-40CC-8457-C17B9A26006D}']
    { Property Accessors }
    function Get_Priorityorder: LongWord;
    function Get_Type_: UnicodeString;
    function Get_Coveragearea(const Index: Integer): IXMLAreaType;
    procedure Set_Priorityorder(const Value: LongWord);
    procedure Set_Type_(const Value: UnicodeString);
    { Methods & Properties }
    function Add: IXMLAreaType;
    function Insert(const Index: Integer): IXMLAreaType;
    property Priorityorder: LongWord read Get_Priorityorder write Set_Priorityorder;
    property Type_: UnicodeString read Get_Type_ write Set_Type_;
    property Coveragearea[const Index: Integer]: IXMLAreaType read Get_Coveragearea; default;
  end;

{ IXMLWarninggroupTypeList }

  IXMLWarninggroupTypeList = interface(IXMLNodeCollection)
    ['{B9BD5BC4-A733-4C84-A020-AD18FDA2EB0E}']
    { Methods & Properties }
    function Add: IXMLWarninggroupType;
    function Insert(const Index: Integer): IXMLWarninggroupType;

    function Get_Item(const Index: Integer): IXMLWarninggroupType;
    property Items[const Index: Integer]: IXMLWarninggroupType read Get_Item; default;
  end;

{ IXMLForecastType }

  IXMLForecastType = interface(IXMLNodeCollection)
    ['{4E1D71AD-B7C9-4419-94D8-B570E88FEC26}']
    { Property Accessors }
    function Get_Area(const Index: Integer): IXMLAreaType;
    { Methods & Properties }
    function Add: IXMLAreaType;
    function Insert(const Index: Integer): IXMLAreaType;
    property Area[const Index: Integer]: IXMLAreaType read Get_Area; default;
  end;

{ IXMLWarningsummariesType }

  IXMLWarningsummariesType = interface(IXMLNodeCollection)
    ['{25951371-3CDC-454F-942A-229AF7A64B99}']
    { Property Accessors }
    function Get_Forecastarea(const Index: Integer): IXMLAreaType;
    { Methods & Properties }
    function Add: IXMLAreaType;
    function Insert(const Index: Integer): IXMLAreaType;
    property Forecastarea[const Index: Integer]: IXMLAreaType read Get_Forecastarea; default;
  end;

{ IXMLObservationsType }

  IXMLObservationsType = interface(IXMLNodeCollection)
    ['{7906051F-8E04-490A-8077-54DA7551087D}']
    { Property Accessors }
    function Get_Station(const Index: Integer): IXMLStationType;
    { Methods & Properties }
    function Add: IXMLStationType;
    function Insert(const Index: Integer): IXMLStationType;
    property Station[const Index: Integer]: IXMLStationType read Get_Station; default;
  end;

{ IXMLStationType }

  IXMLStationType = interface(IXMLNodeCollection)
    ['{E27B241B-F6AD-4533-9D51-98FA374DF94B}']
    { Property Accessors }
    function Get_Bomid: UnicodeString;
    function Get_Wmoid: UnicodeString;
    function Get_Aviationid: UnicodeString;
    function Get_Description: UnicodeString;
    function Get_Tz: UnicodeString;
    function Get_Stnname: UnicodeString;
    function Get_Stnheight: Single;
    function Get_Type_: UnicodeString;
    function Get_Owner: UnicodeString;
    function Get_Lat: Single;
    function Get_Lon: Single;
    function Get_Forecastdistrictid: UnicodeString;
    function Get_Period(const Index: Integer): IXMLObservationperiodType;
    procedure Set_Bomid(const Value: UnicodeString);
    procedure Set_Wmoid(const Value: UnicodeString);
    procedure Set_Aviationid(const Value: UnicodeString);
    procedure Set_Description(const Value: UnicodeString);
    procedure Set_Tz(const Value: UnicodeString);
    procedure Set_Stnname(const Value: UnicodeString);
    procedure Set_Stnheight(const Value: Single);
    procedure Set_Type_(const Value: UnicodeString);
    procedure Set_Owner(const Value: UnicodeString);
    procedure Set_Lat(const Value: Single);
    procedure Set_Lon(const Value: Single);
    procedure Set_Forecastdistrictid(const Value: UnicodeString);
    { Methods & Properties }
    function Add: IXMLObservationperiodType;
    function Insert(const Index: Integer): IXMLObservationperiodType;
    property Bomid: UnicodeString read Get_Bomid write Set_Bomid;
    property Wmoid: UnicodeString read Get_Wmoid write Set_Wmoid;
    property Aviationid: UnicodeString read Get_Aviationid write Set_Aviationid;
    property Description: UnicodeString read Get_Description write Set_Description;
    property Tz: UnicodeString read Get_Tz write Set_Tz;
    property Stnname: UnicodeString read Get_Stnname write Set_Stnname;
    property Stnheight: Single read Get_Stnheight write Set_Stnheight;
    property Type_: UnicodeString read Get_Type_ write Set_Type_;
    property Owner: UnicodeString read Get_Owner write Set_Owner;
    property Lat: Single read Get_Lat write Set_Lat;
    property Lon: Single read Get_Lon write Set_Lon;
    property Forecastdistrictid: UnicodeString read Get_Forecastdistrictid write Set_Forecastdistrictid;
    property Period[const Index: Integer]: IXMLObservationperiodType read Get_Period; default;
  end;

{ IXMLObservationperiodType }

  IXMLObservationperiodType = interface(IXMLNode)
    ['{3F5DA76B-B4DF-4142-A85E-26FC87393DA4}']
    { Property Accessors }
    function Get_Index: LongWord;
    function Get_Timeutc: UnicodeString;
    function Get_Timelocal: UnicodeString;
    function Get_Windsrc: UnicodeString;
    function Get_Level: IXMLLevelTypeList;
    function Get_Element: IXMLElementTypeList;
    procedure Set_Index(const Value: LongWord);
    procedure Set_Timeutc(const Value: UnicodeString);
    procedure Set_Timelocal(const Value: UnicodeString);
    procedure Set_Windsrc(const Value: UnicodeString);
    { Methods & Properties }
    property Index: LongWord read Get_Index write Set_Index;
    property Timeutc: UnicodeString read Get_Timeutc write Set_Timeutc;
    property Timelocal: UnicodeString read Get_Timelocal write Set_Timelocal;
    property Windsrc: UnicodeString read Get_Windsrc write Set_Windsrc;
    property Level: IXMLLevelTypeList read Get_Level;
    property Element: IXMLElementTypeList read Get_Element;
  end;

{ IXMLLevelType }

  IXMLLevelType = interface(IXMLNodeCollection)
    ['{8F2F3709-DF72-42C3-B584-EF47113D0967}']
    { Property Accessors }
    function Get_Index: LongWord;
    function Get_Type_: UnicodeString;
    function Get_Element(const Index: Integer): IXMLElementType;
    procedure Set_Index(const Value: LongWord);
    procedure Set_Type_(const Value: UnicodeString);
    { Methods & Properties }
    function Add: IXMLElementType;
    function Insert(const Index: Integer): IXMLElementType;
    property Index: LongWord read Get_Index write Set_Index;
    property Type_: UnicodeString read Get_Type_ write Set_Type_;
    property Element[const Index: Integer]: IXMLElementType read Get_Element; default;
  end;

{ IXMLLevelTypeList }

  IXMLLevelTypeList = interface(IXMLNodeCollection)
    ['{841A976A-1C54-4EC6-9BD3-A7FB8A8116D6}']
    { Methods & Properties }
    function Add: IXMLLevelType;
    function Insert(const Index: Integer): IXMLLevelType;

    function Get_Item(const Index: Integer): IXMLLevelType;
    property Items[const Index: Integer]: IXMLLevelType read Get_Item; default;
  end;

{ Forward Decls }

  TXMLProductType = class;
  TXMLAmocType = class;
  TXMLSourceType = class;
  TXMLTimestampType = class;
  TXMLPeriodicHazardType = class;
  TXMLPeriodicHazardTypeList = class;
  TXMLArealistType = class;
  TXMLArea = class;
  TXMLPhenomenonlistType = class;
  TXMLPhenomenon = class;
  TXMLText = class;
  TXMLTextList = class;
  TXMLHazardType = class;
  TXMLHazardTypeList = class;
  TXMLWarningType = class;
  TXMLWarninginfoType = class;
  TXMLTextelementType = class;
  TXMLTextelementTypeList = class;
  TXMLAreaType = class;
  TXMLAreaTypeList = class;
  TXMLPeriodicwarningsummaryType = class;
  TXMLPeriodicwarningsummaryTypeList = class;
  TXMLWarningsummaryType = class;
  TXMLWarningsummaryTypeList = class;
  TXMLForecastperiodType = class;
  TXMLForecastperiodTypeList = class;
  TXMLElementType = class;
  TXMLElementTypeList = class;
  TXMLWarninggroupType = class;
  TXMLWarninggroupTypeList = class;
  TXMLForecastType = class;
  TXMLWarningsummariesType = class;
  TXMLObservationsType = class;
  TXMLStationType = class;
  TXMLObservationperiodType = class;
  TXMLLevelType = class;
  TXMLLevelTypeList = class;

{ TXMLProductType }

  TXMLProductType = class(TXMLNode, IXMLProductType)
  protected
    { IXMLProductType }
    function Get_Version: UnicodeString;
    function Get_Amoc: IXMLAmocType;
    function Get_Warning: IXMLWarningType;
    function Get_Forecast: IXMLForecastType;
    function Get_Warningsummaries: IXMLWarningsummariesType;
    function Get_Observations: IXMLObservationsType;
    procedure Set_Version(const Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLAmocType }

  TXMLAmocType = class(TXMLNode, IXMLAmocType)
  private
    FHazard: IXMLHazardTypeList;
  protected
    { IXMLAmocType }
    function Get_Version: UnicodeString;
    function Get_Source: IXMLSourceType;
    function Get_Identifier: UnicodeString;
    function Get_Issuetimeutc: IXMLTimestampType;
    function Get_Issuetimelocal: IXMLTimestampType;
    function Get_Senttime: UnicodeString;
    function Get_Expirytime: UnicodeString;
    function Get_Validitybgntimelocal: IXMLTimestampType;
    function Get_Validityendtimelocal: IXMLTimestampType;
    function Get_Nextroutineissuetimeutc: IXMLTimestampType;
    function Get_Nextroutineissuetimelocal: IXMLTimestampType;
    function Get_Status: UnicodeString;
    function Get_Service: UnicodeString;
    function Get_Subservice: UnicodeString;
    function Get_Producttype: UnicodeString;
    function Get_Phase: UnicodeString;
    function Get_Hazard: IXMLHazardTypeList;
    function Get_Incidentid: UnicodeString;
    procedure Set_Version(const Value: UnicodeString);
    procedure Set_Identifier(const Value: UnicodeString);
    procedure Set_Senttime(const Value: UnicodeString);
    procedure Set_Expirytime(const Value: UnicodeString);
    procedure Set_Status(const Value: UnicodeString);
    procedure Set_Service(const Value: UnicodeString);
    procedure Set_Subservice(const Value: UnicodeString);
    procedure Set_Producttype(const Value: UnicodeString);
    procedure Set_Phase(const Value: UnicodeString);
    procedure Set_Incidentid(const Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSourceType }

  TXMLSourceType = class(TXMLNode, IXMLSourceType)
  protected
    { IXMLSourceType }
    function Get_Sender: UnicodeString;
    function Get_Region: UnicodeString;
    function Get_Office: UnicodeString;
    function Get_Copyright: UnicodeString;
    function Get_Disclaimer: UnicodeString;
    function Get_Description: UnicodeString;
    procedure Set_Sender(const Value: UnicodeString);
    procedure Set_Region(const Value: UnicodeString);
    procedure Set_Office(const Value: UnicodeString);
    procedure Set_Copyright(const Value: UnicodeString);
    procedure Set_Disclaimer(const Value: UnicodeString);
    procedure Set_Description(const Value: UnicodeString);
  end;

{ TXMLTimestampType }

  TXMLTimestampType = class(TXMLNode, IXMLTimestampType)
  protected
    { IXMLTimestampType }
    function Get_Tz: UnicodeString;
    procedure Set_Tz(const Value: UnicodeString);
  end;

{ TXMLPeriodicHazardType }

  TXMLPeriodicHazardType = class(TXMLNode, IXMLPeriodicHazardType)
  private
    FText: IXMLTextList;
  protected
    { IXMLPeriodicHazardType }
    function Get_Index: Integer;
    function Get_Type_: UnicodeString;
    function Get_Severity: UnicodeString;
    function Get_Urgency: UnicodeString;
    function Get_Certainty: UnicodeString;
    function Get_Phase: UnicodeString;
    function Get_Parentaac: UnicodeString;
    function Get_Arealist: IXMLArealistType;
    function Get_Phenomenonlist: IXMLPhenomenonlistType;
    function Get_Priority: UnicodeString;
    function Get_Headline: UnicodeString;
    function Get_Text: IXMLTextList;
    procedure Set_Index(const Value: Integer);
    procedure Set_Type_(const Value: UnicodeString);
    procedure Set_Severity(const Value: UnicodeString);
    procedure Set_Urgency(const Value: UnicodeString);
    procedure Set_Certainty(const Value: UnicodeString);
    procedure Set_Phase(const Value: UnicodeString);
    procedure Set_Parentaac(const Value: UnicodeString);
    procedure Set_Priority(const Value: UnicodeString);
    procedure Set_Headline(const Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPeriodicHazardTypeList }

  TXMLPeriodicHazardTypeList = class(TXMLNodeCollection, IXMLPeriodicHazardTypeList)
  protected
    { IXMLPeriodicHazardTypeList }
    function Add: IXMLPeriodicHazardType;
    function Insert(const Index: Integer): IXMLPeriodicHazardType;

    function Get_Item(const Index: Integer): IXMLPeriodicHazardType;
  end;

{ TXMLArealistType }

  TXMLArealistType = class(TXMLNodeCollection, IXMLArealistType)
  protected
    { IXMLArealistType }
    function Get_Area(const Index: Integer): IXMLArea;
    function Add: IXMLArea;
    function Insert(const Index: Integer): IXMLArea;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLArea }

  TXMLArea = class(TXMLNode, IXMLArea)
  protected
    { IXMLArea }
    function Get_Aac: UnicodeString;
    function Get_Phase: UnicodeString;
    function Get_Description: UnicodeString;
    function Get_Type_: UnicodeString;
    procedure Set_Aac(const Value: UnicodeString);
    procedure Set_Phase(const Value: UnicodeString);
    procedure Set_Description(const Value: UnicodeString);
    procedure Set_Type_(const Value: UnicodeString);
  end;

{ TXMLPhenomenonlistType }

  TXMLPhenomenonlistType = class(TXMLNodeCollection, IXMLPhenomenonlistType)
  protected
    { IXMLPhenomenonlistType }
    function Get_Phenomenon(const Index: Integer): IXMLPhenomenon;
    function Add: IXMLPhenomenon;
    function Insert(const Index: Integer): IXMLPhenomenon;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPhenomenon }

  TXMLPhenomenon = class(TXMLNode, IXMLPhenomenon)
  protected
    { IXMLPhenomenon }
    function Get_Type_: UnicodeString;
    procedure Set_Type_(const Value: UnicodeString);
  end;

{ TXMLText }

  TXMLText = class(TXMLNodeCollection, IXMLText)
  protected
    { IXMLText }
    function Get_Type_: UnicodeString;
    function Get_P(const Index: Integer): UnicodeString;
    procedure Set_Type_(const Value: UnicodeString);
    function Add(const P: UnicodeString): IXMLNode;
    function Insert(const Index: Integer; const P: UnicodeString): IXMLNode;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTextList }

  TXMLTextList = class(TXMLNodeCollection, IXMLTextList)
  protected
    { IXMLTextList }
    function Add: IXMLText;
    function Insert(const Index: Integer): IXMLText;

    function Get_Item(const Index: Integer): IXMLText;
  end;

{ TXMLHazardType }

  TXMLHazardType = class(TXMLPeriodicHazardType, IXMLHazardType)
  protected
    { IXMLHazardType }
    function Get_Starttimeutc: UnicodeString;
    function Get_Starttimelocal: UnicodeString;
    function Get_Endtimeutc: UnicodeString;
    function Get_Endtimelocal: UnicodeString;
    procedure Set_Starttimeutc(const Value: UnicodeString);
    procedure Set_Starttimelocal(const Value: UnicodeString);
    procedure Set_Endtimeutc(const Value: UnicodeString);
    procedure Set_Endtimelocal(const Value: UnicodeString);
  end;

{ TXMLHazardTypeList }

  TXMLHazardTypeList = class(TXMLNodeCollection, IXMLHazardTypeList)
  protected
    { IXMLHazardTypeList }
    function Add: IXMLHazardType;
    function Insert(const Index: Integer): IXMLHazardType;

    function Get_Item(const Index: Integer): IXMLHazardType;
  end;

{ TXMLWarningType }

  TXMLWarningType = class(TXMLNode, IXMLWarningType)
  private
    FArea: IXMLAreaTypeList;
  protected
    { IXMLWarningType }
    function Get_Warninginfo: IXMLWarninginfoType;
    function Get_Area: IXMLAreaTypeList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLWarninginfoType }

  TXMLWarninginfoType = class(TXMLNodeCollection, IXMLWarninginfoType)
  protected
    { IXMLWarninginfoType }
    function Get_Text(const Index: Integer): IXMLTextelementType;
    function Add: IXMLTextelementType;
    function Insert(const Index: Integer): IXMLTextelementType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTextelementType }

  TXMLTextelementType = class(TXMLNodeCollection, IXMLTextelementType)
  protected
    { IXMLTextelementType }
    function Get_Type_: UnicodeString;
    function Get_P(const Index: Integer): UnicodeString;
    procedure Set_Type_(const Value: UnicodeString);
    function Add(const P: UnicodeString): IXMLNode;
    function Insert(const Index: Integer; const P: UnicodeString): IXMLNode;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTextelementTypeList }

  TXMLTextelementTypeList = class(TXMLNodeCollection, IXMLTextelementTypeList)
  protected
    { IXMLTextelementTypeList }
    function Add: IXMLTextelementType;
    function Insert(const Index: Integer): IXMLTextelementType;

    function Get_Item(const Index: Integer): IXMLTextelementType;
  end;

{ TXMLAreaType }

  TXMLAreaType = class(TXMLNode, IXMLAreaType)
  private
    FWarningsummary: IXMLWarningsummaryTypeList;
    FForecastperiod: IXMLForecastperiodTypeList;
    FHazard: IXMLHazardTypeList;
    FWarninggroup: IXMLWarninggroupTypeList;
  protected
    { IXMLAreaType }
    function Get_Aac: UnicodeString;
    function Get_Parentaac: UnicodeString;
    function Get_Description: UnicodeString;
    function Get_Type_: UnicodeString;
    function Get_Warningsummary: IXMLWarningsummaryTypeList;
    function Get_Forecastperiod: IXMLForecastperiodTypeList;
    function Get_Hazard: IXMLHazardTypeList;
    function Get_Warninggroup: IXMLWarninggroupTypeList;
    procedure Set_Aac(const Value: UnicodeString);
    procedure Set_Parentaac(const Value: UnicodeString);
    procedure Set_Description(const Value: UnicodeString);
    procedure Set_Type_(const Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLAreaTypeList }

  TXMLAreaTypeList = class(TXMLNodeCollection, IXMLAreaTypeList)
  protected
    { IXMLAreaTypeList }
    function Add: IXMLAreaType;
    function Insert(const Index: Integer): IXMLAreaType;

    function Get_Item(const Index: Integer): IXMLAreaType;
  end;

{ TXMLPeriodicwarningsummaryType }

  TXMLPeriodicwarningsummaryType = class(TXMLNodeCollection, IXMLPeriodicwarningsummaryType)
  protected
    { IXMLPeriodicwarningsummaryType }
    function Get_Type_: UnicodeString;
    function Get_Productidentifier: UnicodeString;
    function Get_Producturl: UnicodeString;
    function Get_Aac: UnicodeString;
    function Get_Area: UnicodeString;
    function Get_Areatype: UnicodeString;
    function Get_P(const Index: Integer): UnicodeString;
    procedure Set_Type_(const Value: UnicodeString);
    procedure Set_Productidentifier(const Value: UnicodeString);
    procedure Set_Producturl(const Value: UnicodeString);
    procedure Set_Aac(const Value: UnicodeString);
    procedure Set_Area(const Value: UnicodeString);
    procedure Set_Areatype(const Value: UnicodeString);
    function Add(const P: UnicodeString): IXMLNode;
    function Insert(const Index: Integer; const P: UnicodeString): IXMLNode;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPeriodicwarningsummaryTypeList }

  TXMLPeriodicwarningsummaryTypeList = class(TXMLNodeCollection, IXMLPeriodicwarningsummaryTypeList)
  protected
    { IXMLPeriodicwarningsummaryTypeList }
    function Add: IXMLPeriodicwarningsummaryType;
    function Insert(const Index: Integer): IXMLPeriodicwarningsummaryType;

    function Get_Item(const Index: Integer): IXMLPeriodicwarningsummaryType;
  end;

{ TXMLWarningsummaryType }

  TXMLWarningsummaryType = class(TXMLPeriodicwarningsummaryType, IXMLWarningsummaryType)
  protected
    { IXMLWarningsummaryType }
    function Get_Starttimeutc: UnicodeString;
    function Get_Starttimelocal: UnicodeString;
    function Get_Endtimeutc: UnicodeString;
    function Get_Endtimelocal: UnicodeString;
    procedure Set_Starttimeutc(const Value: UnicodeString);
    procedure Set_Starttimelocal(const Value: UnicodeString);
    procedure Set_Endtimeutc(const Value: UnicodeString);
    procedure Set_Endtimelocal(const Value: UnicodeString);
  end;

{ TXMLWarningsummaryTypeList }

  TXMLWarningsummaryTypeList = class(TXMLNodeCollection, IXMLWarningsummaryTypeList)
  protected
    { IXMLWarningsummaryTypeList }
    function Add: IXMLWarningsummaryType;
    function Insert(const Index: Integer): IXMLWarningsummaryType;

    function Get_Item(const Index: Integer): IXMLWarningsummaryType;
  end;

{ TXMLForecastperiodType }

  TXMLForecastperiodType = class(TXMLNode, IXMLForecastperiodType)
  private
    FWarningsummary: IXMLPeriodicwarningsummaryTypeList;
    FElement: IXMLElementTypeList;
    FText: IXMLTextelementTypeList;
    FHazard: IXMLPeriodicHazardTypeList;
  protected
    { IXMLForecastperiodType }
    function Get_Index: LongWord;
    function Get_Endindex: LongWord;
    function Get_Indextag: UnicodeString;
    function Get_Starttimeutc: UnicodeString;
    function Get_Starttimelocal: UnicodeString;
    function Get_Endtimeutc: UnicodeString;
    function Get_Endtimelocal: UnicodeString;
    function Get_Enddatelocal: UnicodeString;
    function Get_Warningsummary: IXMLPeriodicwarningsummaryTypeList;
    function Get_Element: IXMLElementTypeList;
    function Get_Text: IXMLTextelementTypeList;
    function Get_Hazard: IXMLPeriodicHazardTypeList;
    procedure Set_Index(const Value: LongWord);
    procedure Set_Endindex(const Value: LongWord);
    procedure Set_Indextag(const Value: UnicodeString);
    procedure Set_Starttimeutc(const Value: UnicodeString);
    procedure Set_Starttimelocal(const Value: UnicodeString);
    procedure Set_Endtimeutc(const Value: UnicodeString);
    procedure Set_Endtimelocal(const Value: UnicodeString);
    procedure Set_Enddatelocal(const Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLForecastperiodTypeList }

  TXMLForecastperiodTypeList = class(TXMLNodeCollection, IXMLForecastperiodTypeList)
  protected
    { IXMLForecastperiodTypeList }
    function Add: IXMLForecastperiodType;
    function Insert(const Index: Integer): IXMLForecastperiodType;

    function Get_Item(const Index: Integer): IXMLForecastperiodType;
  end;

{ TXMLElementType }

  TXMLElementType = class(TXMLNode, IXMLElementType)
  protected
    { IXMLElementType }
    function Get_Type_: UnicodeString;
    function Get_Units: UnicodeString;
    function Get_Instance: UnicodeString;
    function Get_Sequence: LongWord;
    function Get_Duration: LongWord;
    function Get_Timeutc: UnicodeString;
    function Get_Timelocal: UnicodeString;
    function Get_Starttimeutc: UnicodeString;
    function Get_Starttimelocal: UnicodeString;
    function Get_Endtimeutc: UnicodeString;
    function Get_Endtimelocal: UnicodeString;
    function Get_Startmonth: UnicodeString;
    function Get_Endmonth: UnicodeString;
    procedure Set_Type_(const Value: UnicodeString);
    procedure Set_Units(const Value: UnicodeString);
    procedure Set_Instance(const Value: UnicodeString);
    procedure Set_Sequence(const Value: LongWord);
    procedure Set_Duration(const Value: LongWord);
    procedure Set_Timeutc(const Value: UnicodeString);
    procedure Set_Timelocal(const Value: UnicodeString);
    procedure Set_Starttimeutc(const Value: UnicodeString);
    procedure Set_Starttimelocal(const Value: UnicodeString);
    procedure Set_Endtimeutc(const Value: UnicodeString);
    procedure Set_Endtimelocal(const Value: UnicodeString);
    procedure Set_Startmonth(const Value: UnicodeString);
    procedure Set_Endmonth(const Value: UnicodeString);
  end;

{ TXMLElementTypeList }

  TXMLElementTypeList = class(TXMLNodeCollection, IXMLElementTypeList)
  protected
    { IXMLElementTypeList }
    function Add: IXMLElementType;
    function Insert(const Index: Integer): IXMLElementType;

    function Get_Item(const Index: Integer): IXMLElementType;
  end;

{ TXMLWarninggroupType }

  TXMLWarninggroupType = class(TXMLNodeCollection, IXMLWarninggroupType)
  protected
    { IXMLWarninggroupType }
    function Get_Priorityorder: LongWord;
    function Get_Type_: UnicodeString;
    function Get_Coveragearea(const Index: Integer): IXMLAreaType;
    procedure Set_Priorityorder(const Value: LongWord);
    procedure Set_Type_(const Value: UnicodeString);
    function Add: IXMLAreaType;
    function Insert(const Index: Integer): IXMLAreaType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLWarninggroupTypeList }

  TXMLWarninggroupTypeList = class(TXMLNodeCollection, IXMLWarninggroupTypeList)
  protected
    { IXMLWarninggroupTypeList }
    function Add: IXMLWarninggroupType;
    function Insert(const Index: Integer): IXMLWarninggroupType;

    function Get_Item(const Index: Integer): IXMLWarninggroupType;
  end;

{ TXMLForecastType }

  TXMLForecastType = class(TXMLNodeCollection, IXMLForecastType)
  protected
    { IXMLForecastType }
    function Get_Area(const Index: Integer): IXMLAreaType;
    function Add: IXMLAreaType;
    function Insert(const Index: Integer): IXMLAreaType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLWarningsummariesType }

  TXMLWarningsummariesType = class(TXMLNodeCollection, IXMLWarningsummariesType)
  protected
    { IXMLWarningsummariesType }
    function Get_Forecastarea(const Index: Integer): IXMLAreaType;
    function Add: IXMLAreaType;
    function Insert(const Index: Integer): IXMLAreaType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLObservationsType }

  TXMLObservationsType = class(TXMLNodeCollection, IXMLObservationsType)
  protected
    { IXMLObservationsType }
    function Get_Station(const Index: Integer): IXMLStationType;
    function Add: IXMLStationType;
    function Insert(const Index: Integer): IXMLStationType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLStationType }

  TXMLStationType = class(TXMLNodeCollection, IXMLStationType)
  protected
    { IXMLStationType }
    function Get_Bomid: UnicodeString;
    function Get_Wmoid: UnicodeString;
    function Get_Aviationid: UnicodeString;
    function Get_Description: UnicodeString;
    function Get_Tz: UnicodeString;
    function Get_Stnname: UnicodeString;
    function Get_Stnheight: Single;
    function Get_Type_: UnicodeString;
    function Get_Owner: UnicodeString;
    function Get_Lat: Single;
    function Get_Lon: Single;
    function Get_Forecastdistrictid: UnicodeString;
    function Get_Period(const Index: Integer): IXMLObservationperiodType;
    procedure Set_Bomid(const Value: UnicodeString);
    procedure Set_Wmoid(const Value: UnicodeString);
    procedure Set_Aviationid(const Value: UnicodeString);
    procedure Set_Description(const Value: UnicodeString);
    procedure Set_Tz(const Value: UnicodeString);
    procedure Set_Stnname(const Value: UnicodeString);
    procedure Set_Stnheight(const Value: Single);
    procedure Set_Type_(const Value: UnicodeString);
    procedure Set_Owner(const Value: UnicodeString);
    procedure Set_Lat(const Value: Single);
    procedure Set_Lon(const Value: Single);
    procedure Set_Forecastdistrictid(const Value: UnicodeString);
    function Add: IXMLObservationperiodType;
    function Insert(const Index: Integer): IXMLObservationperiodType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLObservationperiodType }

  TXMLObservationperiodType = class(TXMLNode, IXMLObservationperiodType)
  private
    FLevel: IXMLLevelTypeList;
    FElement: IXMLElementTypeList;
  protected
    { IXMLObservationperiodType }
    function Get_Index: LongWord;
    function Get_Timeutc: UnicodeString;
    function Get_Timelocal: UnicodeString;
    function Get_Windsrc: UnicodeString;
    function Get_Level: IXMLLevelTypeList;
    function Get_Element: IXMLElementTypeList;
    procedure Set_Index(const Value: LongWord);
    procedure Set_Timeutc(const Value: UnicodeString);
    procedure Set_Timelocal(const Value: UnicodeString);
    procedure Set_Windsrc(const Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLLevelType }

  TXMLLevelType = class(TXMLNodeCollection, IXMLLevelType)
  protected
    { IXMLLevelType }
    function Get_Index: LongWord;
    function Get_Type_: UnicodeString;
    function Get_Element(const Index: Integer): IXMLElementType;
    procedure Set_Index(const Value: LongWord);
    procedure Set_Type_(const Value: UnicodeString);
    function Add: IXMLElementType;
    function Insert(const Index: Integer): IXMLElementType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLLevelTypeList }

  TXMLLevelTypeList = class(TXMLNodeCollection, IXMLLevelTypeList)
  protected
    { IXMLLevelTypeList }
    function Add: IXMLLevelType;
    function Insert(const Index: Integer): IXMLLevelType;

    function Get_Item(const Index: Integer): IXMLLevelType;
  end;

{ Global Functions }

function Getproduct(Doc: IXMLDocument): IXMLProductType;
function Loadproduct(const FileName: string): IXMLProductType;
function Newproduct: IXMLProductType;

const
  TargetNamespace = '';

implementation

uses System.Variants, System.SysUtils, Xml.xmlutil;

{ Global Functions }

function Getproduct(Doc: IXMLDocument): IXMLProductType;
begin
  Result := Doc.GetDocBinding('product', TXMLProductType, TargetNamespace) as IXMLProductType;
end;

function Loadproduct(const FileName: string): IXMLProductType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('product', TXMLProductType, TargetNamespace) as IXMLProductType;
end;

function Newproduct: IXMLProductType;
begin
  Result := NewXMLDocument.GetDocBinding('product', TXMLProductType, TargetNamespace) as IXMLProductType;
end;

{ TXMLProductType }

procedure TXMLProductType.AfterConstruction;
begin
  RegisterChildNode('amoc', TXMLAmocType);
  RegisterChildNode('warning', TXMLWarningType);
  RegisterChildNode('forecast', TXMLForecastType);
  RegisterChildNode('warning-summaries', TXMLWarningsummariesType);
  RegisterChildNode('observations', TXMLObservationsType);
  inherited;
end;

function TXMLProductType.Get_Version: UnicodeString;
begin
  Result := AttributeNodes['version'].Text;
end;

procedure TXMLProductType.Set_Version(const Value: UnicodeString);
begin
  SetAttribute('version', Value);
end;

function TXMLProductType.Get_Amoc: IXMLAmocType;
begin
  Result := ChildNodes['amoc'] as IXMLAmocType;
end;

function TXMLProductType.Get_Warning: IXMLWarningType;
begin
  Result := ChildNodes['warning'] as IXMLWarningType;
end;

function TXMLProductType.Get_Forecast: IXMLForecastType;
begin
  Result := ChildNodes['forecast'] as IXMLForecastType;
end;

function TXMLProductType.Get_Warningsummaries: IXMLWarningsummariesType;
begin
  Result := ChildNodes['warning-summaries'] as IXMLWarningsummariesType;
end;

function TXMLProductType.Get_Observations: IXMLObservationsType;
begin
  Result := ChildNodes['observations'] as IXMLObservationsType;
end;

{ TXMLAmocType }

procedure TXMLAmocType.AfterConstruction;
begin
  RegisterChildNode('source', TXMLSourceType);
  RegisterChildNode('issue-time-utc', TXMLTimestampType);
  RegisterChildNode('issue-time-local', TXMLTimestampType);
  RegisterChildNode('validity-bgn-time-local', TXMLTimestampType);
  RegisterChildNode('validity-end-time-local', TXMLTimestampType);
  RegisterChildNode('next-routine-issue-time-utc', TXMLTimestampType);
  RegisterChildNode('next-routine-issue-time-local', TXMLTimestampType);
  RegisterChildNode('hazard', TXMLHazardType);
  FHazard := CreateCollection(TXMLHazardTypeList, IXMLHazardType, 'hazard') as IXMLHazardTypeList;
  inherited;
end;

function TXMLAmocType.Get_Version: UnicodeString;
begin
  Result := AttributeNodes['version'].Text;
end;

procedure TXMLAmocType.Set_Version(const Value: UnicodeString);
begin
  SetAttribute('version', Value);
end;

function TXMLAmocType.Get_Source: IXMLSourceType;
begin
  Result := ChildNodes['source'] as IXMLSourceType;
end;

function TXMLAmocType.Get_Identifier: UnicodeString;
begin
  Result := ChildNodes['identifier'].Text;
end;

procedure TXMLAmocType.Set_Identifier(const Value: UnicodeString);
begin
  ChildNodes['identifier'].NodeValue := Value;
end;

function TXMLAmocType.Get_Issuetimeutc: IXMLTimestampType;
begin
  Result := ChildNodes['issue-time-utc'] as IXMLTimestampType;
end;

function TXMLAmocType.Get_Issuetimelocal: IXMLTimestampType;
begin
  Result := ChildNodes['issue-time-local'] as IXMLTimestampType;
end;

function TXMLAmocType.Get_Senttime: UnicodeString;
begin
  Result := ChildNodes['sent-time'].Text;
end;

procedure TXMLAmocType.Set_Senttime(const Value: UnicodeString);
begin
  ChildNodes['sent-time'].NodeValue := Value;
end;

function TXMLAmocType.Get_Expirytime: UnicodeString;
begin
  Result := ChildNodes['expiry-time'].Text;
end;

procedure TXMLAmocType.Set_Expirytime(const Value: UnicodeString);
begin
  ChildNodes['expiry-time'].NodeValue := Value;
end;

function TXMLAmocType.Get_Validitybgntimelocal: IXMLTimestampType;
begin
  Result := ChildNodes['validity-bgn-time-local'] as IXMLTimestampType;
end;

function TXMLAmocType.Get_Validityendtimelocal: IXMLTimestampType;
begin
  Result := ChildNodes['validity-end-time-local'] as IXMLTimestampType;
end;

function TXMLAmocType.Get_Nextroutineissuetimeutc: IXMLTimestampType;
begin
  Result := ChildNodes['next-routine-issue-time-utc'] as IXMLTimestampType;
end;

function TXMLAmocType.Get_Nextroutineissuetimelocal: IXMLTimestampType;
begin
  Result := ChildNodes['next-routine-issue-time-local'] as IXMLTimestampType;
end;

function TXMLAmocType.Get_Status: UnicodeString;
begin
  Result := ChildNodes['status'].Text;
end;

procedure TXMLAmocType.Set_Status(const Value: UnicodeString);
begin
  ChildNodes['status'].NodeValue := Value;
end;

function TXMLAmocType.Get_Service: UnicodeString;
begin
  Result := ChildNodes['service'].Text;
end;

procedure TXMLAmocType.Set_Service(const Value: UnicodeString);
begin
  ChildNodes['service'].NodeValue := Value;
end;

function TXMLAmocType.Get_Subservice: UnicodeString;
begin
  Result := ChildNodes['sub-service'].Text;
end;

procedure TXMLAmocType.Set_Subservice(const Value: UnicodeString);
begin
  ChildNodes['sub-service'].NodeValue := Value;
end;

function TXMLAmocType.Get_Producttype: UnicodeString;
begin
  Result := ChildNodes['product-type'].Text;
end;

procedure TXMLAmocType.Set_Producttype(const Value: UnicodeString);
begin
  ChildNodes['product-type'].NodeValue := Value;
end;

function TXMLAmocType.Get_Phase: UnicodeString;
begin
  Result := ChildNodes['phase'].Text;
end;

procedure TXMLAmocType.Set_Phase(const Value: UnicodeString);
begin
  ChildNodes['phase'].NodeValue := Value;
end;

function TXMLAmocType.Get_Hazard: IXMLHazardTypeList;
begin
  Result := FHazard;
end;

function TXMLAmocType.Get_Incidentid: UnicodeString;
begin
  Result := ChildNodes['incident-id'].Text;
end;

procedure TXMLAmocType.Set_Incidentid(const Value: UnicodeString);
begin
  ChildNodes['incident-id'].NodeValue := Value;
end;

{ TXMLSourceType }

function TXMLSourceType.Get_Sender: UnicodeString;
begin
  Result := ChildNodes['sender'].Text;
end;

procedure TXMLSourceType.Set_Sender(const Value: UnicodeString);
begin
  ChildNodes['sender'].NodeValue := Value;
end;

function TXMLSourceType.Get_Region: UnicodeString;
begin
  Result := ChildNodes['region'].Text;
end;

procedure TXMLSourceType.Set_Region(const Value: UnicodeString);
begin
  ChildNodes['region'].NodeValue := Value;
end;

function TXMLSourceType.Get_Office: UnicodeString;
begin
  Result := ChildNodes['office'].Text;
end;

procedure TXMLSourceType.Set_Office(const Value: UnicodeString);
begin
  ChildNodes['office'].NodeValue := Value;
end;

function TXMLSourceType.Get_Copyright: UnicodeString;
begin
  Result := ChildNodes['copyright'].Text;
end;

procedure TXMLSourceType.Set_Copyright(const Value: UnicodeString);
begin
  ChildNodes['copyright'].NodeValue := Value;
end;

function TXMLSourceType.Get_Disclaimer: UnicodeString;
begin
  Result := ChildNodes['disclaimer'].Text;
end;

procedure TXMLSourceType.Set_Disclaimer(const Value: UnicodeString);
begin
  ChildNodes['disclaimer'].NodeValue := Value;
end;

function TXMLSourceType.Get_Description: UnicodeString;
begin
  Result := ChildNodes['description'].Text;
end;

procedure TXMLSourceType.Set_Description(const Value: UnicodeString);
begin
  ChildNodes['description'].NodeValue := Value;
end;

{ TXMLTimestampType }

function TXMLTimestampType.Get_Tz: UnicodeString;
begin
  Result := AttributeNodes['tz'].Text;
end;

procedure TXMLTimestampType.Set_Tz(const Value: UnicodeString);
begin
  SetAttribute('tz', Value);
end;

{ TXMLPeriodicHazardType }

procedure TXMLPeriodicHazardType.AfterConstruction;
begin
  RegisterChildNode('area-list', TXMLArealistType);
  RegisterChildNode('phenomenon-list', TXMLPhenomenonlistType);
  RegisterChildNode('text', TXMLText);
  FText := CreateCollection(TXMLTextList, IXMLText, 'text') as IXMLTextList;
  inherited;
end;

function TXMLPeriodicHazardType.Get_Index: Integer;
begin
  Result := XmlStrToInt(AttributeNodes['index'].Text);
end;

procedure TXMLPeriodicHazardType.Set_Index(const Value: Integer);
begin
  SetAttribute('index', Value);
end;

function TXMLPeriodicHazardType.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLPeriodicHazardType.Set_Type_(const Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLPeriodicHazardType.Get_Severity: UnicodeString;
begin
  Result := AttributeNodes['severity'].Text;
end;

procedure TXMLPeriodicHazardType.Set_Severity(const Value: UnicodeString);
begin
  SetAttribute('severity', Value);
end;

function TXMLPeriodicHazardType.Get_Urgency: UnicodeString;
begin
  Result := AttributeNodes['urgency'].Text;
end;

procedure TXMLPeriodicHazardType.Set_Urgency(const Value: UnicodeString);
begin
  SetAttribute('urgency', Value);
end;

function TXMLPeriodicHazardType.Get_Certainty: UnicodeString;
begin
  Result := AttributeNodes['certainty'].Text;
end;

procedure TXMLPeriodicHazardType.Set_Certainty(const Value: UnicodeString);
begin
  SetAttribute('certainty', Value);
end;

function TXMLPeriodicHazardType.Get_Phase: UnicodeString;
begin
  Result := AttributeNodes['phase'].Text;
end;

procedure TXMLPeriodicHazardType.Set_Phase(const Value: UnicodeString);
begin
  SetAttribute('phase', Value);
end;

function TXMLPeriodicHazardType.Get_Parentaac: UnicodeString;
begin
  Result := AttributeNodes['parent-aac'].Text;
end;

procedure TXMLPeriodicHazardType.Set_Parentaac(const Value: UnicodeString);
begin
  SetAttribute('parent-aac', Value);
end;

function TXMLPeriodicHazardType.Get_Arealist: IXMLArealistType;
begin
  Result := ChildNodes['area-list'] as IXMLArealistType;
end;

function TXMLPeriodicHazardType.Get_Phenomenonlist: IXMLPhenomenonlistType;
begin
  Result := ChildNodes['phenomenon-list'] as IXMLPhenomenonlistType;
end;

function TXMLPeriodicHazardType.Get_Priority: UnicodeString;
begin
  Result := ChildNodes['priority'].Text;
end;

procedure TXMLPeriodicHazardType.Set_Priority(const Value: UnicodeString);
begin
  ChildNodes['priority'].NodeValue := Value;
end;

function TXMLPeriodicHazardType.Get_Headline: UnicodeString;
begin
  Result := ChildNodes['headline'].Text;
end;

procedure TXMLPeriodicHazardType.Set_Headline(const Value: UnicodeString);
begin
  ChildNodes['headline'].NodeValue := Value;
end;

function TXMLPeriodicHazardType.Get_Text: IXMLTextList;
begin
  Result := FText;
end;

{ TXMLPeriodicHazardTypeList }

function TXMLPeriodicHazardTypeList.Add: IXMLPeriodicHazardType;
begin
  Result := AddItem(-1) as IXMLPeriodicHazardType;
end;

function TXMLPeriodicHazardTypeList.Insert(const Index: Integer): IXMLPeriodicHazardType;
begin
  Result := AddItem(Index) as IXMLPeriodicHazardType;
end;

function TXMLPeriodicHazardTypeList.Get_Item(const Index: Integer): IXMLPeriodicHazardType;
begin
  Result := List[Index] as IXMLPeriodicHazardType;
end;

{ TXMLArealistType }

procedure TXMLArealistType.AfterConstruction;
begin
  RegisterChildNode('area', TXMLArea);
  ItemTag := 'area';
  ItemInterface := IXMLArea;
  inherited;
end;

function TXMLArealistType.Get_Area(const Index: Integer): IXMLArea;
begin
  Result := List[Index] as IXMLArea;
end;

function TXMLArealistType.Add: IXMLArea;
begin
  Result := AddItem(-1) as IXMLArea;
end;

function TXMLArealistType.Insert(const Index: Integer): IXMLArea;
begin
  Result := AddItem(Index) as IXMLArea;
end;

{ TXMLArea }

function TXMLArea.Get_Aac: UnicodeString;
begin
  Result := AttributeNodes['aac'].Text;
end;

procedure TXMLArea.Set_Aac(const Value: UnicodeString);
begin
  SetAttribute('aac', Value);
end;

function TXMLArea.Get_Phase: UnicodeString;
begin
  Result := AttributeNodes['phase'].Text;
end;

procedure TXMLArea.Set_Phase(const Value: UnicodeString);
begin
  SetAttribute('phase', Value);
end;

function TXMLArea.Get_Description: UnicodeString;
begin
  Result := AttributeNodes['description'].Text;
end;

procedure TXMLArea.Set_Description(const Value: UnicodeString);
begin
  SetAttribute('description', Value);
end;

function TXMLArea.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLArea.Set_Type_(const Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

{ TXMLPhenomenonlistType }

procedure TXMLPhenomenonlistType.AfterConstruction;
begin
  RegisterChildNode('phenomenon', TXMLPhenomenon);
  ItemTag := 'phenomenon';
  ItemInterface := IXMLPhenomenon;
  inherited;
end;

function TXMLPhenomenonlistType.Get_Phenomenon(const Index: Integer): IXMLPhenomenon;
begin
  Result := List[Index] as IXMLPhenomenon;
end;

function TXMLPhenomenonlistType.Add: IXMLPhenomenon;
begin
  Result := AddItem(-1) as IXMLPhenomenon;
end;

function TXMLPhenomenonlistType.Insert(const Index: Integer): IXMLPhenomenon;
begin
  Result := AddItem(Index) as IXMLPhenomenon;
end;

{ TXMLPhenomenon }

function TXMLPhenomenon.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLPhenomenon.Set_Type_(const Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

{ TXMLText }

procedure TXMLText.AfterConstruction;
begin
  ItemTag := 'p';
  ItemInterface := IXMLNode;
  inherited;
end;

function TXMLText.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLText.Set_Type_(const Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLText.Get_P(const Index: Integer): UnicodeString;
begin
  Result := List[Index].Text;
end;

function TXMLText.Add(const P: UnicodeString): IXMLNode;
begin
  Result := AddItem(-1);
  Result.NodeValue := P;
end;

function TXMLText.Insert(const Index: Integer; const P: UnicodeString): IXMLNode;
begin
  Result := AddItem(Index);
  Result.NodeValue := P;
end;

{ TXMLTextList }

function TXMLTextList.Add: IXMLText;
begin
  Result := AddItem(-1) as IXMLText;
end;

function TXMLTextList.Insert(const Index: Integer): IXMLText;
begin
  Result := AddItem(Index) as IXMLText;
end;

function TXMLTextList.Get_Item(const Index: Integer): IXMLText;
begin
  Result := List[Index] as IXMLText;
end;

{ TXMLHazardType }

function TXMLHazardType.Get_Starttimeutc: UnicodeString;
begin
  Result := AttributeNodes['start-time-utc'].Text;
end;

procedure TXMLHazardType.Set_Starttimeutc(const Value: UnicodeString);
begin
  SetAttribute('start-time-utc', Value);
end;

function TXMLHazardType.Get_Starttimelocal: UnicodeString;
begin
  Result := AttributeNodes['start-time-local'].Text;
end;

procedure TXMLHazardType.Set_Starttimelocal(const Value: UnicodeString);
begin
  SetAttribute('start-time-local', Value);
end;

function TXMLHazardType.Get_Endtimeutc: UnicodeString;
begin
  Result := AttributeNodes['end-time-utc'].Text;
end;

procedure TXMLHazardType.Set_Endtimeutc(const Value: UnicodeString);
begin
  SetAttribute('end-time-utc', Value);
end;

function TXMLHazardType.Get_Endtimelocal: UnicodeString;
begin
  Result := AttributeNodes['end-time-local'].Text;
end;

procedure TXMLHazardType.Set_Endtimelocal(const Value: UnicodeString);
begin
  SetAttribute('end-time-local', Value);
end;

{ TXMLHazardTypeList }

function TXMLHazardTypeList.Add: IXMLHazardType;
begin
  Result := AddItem(-1) as IXMLHazardType;
end;

function TXMLHazardTypeList.Insert(const Index: Integer): IXMLHazardType;
begin
  Result := AddItem(Index) as IXMLHazardType;
end;

function TXMLHazardTypeList.Get_Item(const Index: Integer): IXMLHazardType;
begin
  Result := List[Index] as IXMLHazardType;
end;

{ TXMLWarningType }

procedure TXMLWarningType.AfterConstruction;
begin
  RegisterChildNode('warning-info', TXMLWarninginfoType);
  RegisterChildNode('area', TXMLAreaType);
  FArea := CreateCollection(TXMLAreaTypeList, IXMLAreaType, 'area') as IXMLAreaTypeList;
  inherited;
end;

function TXMLWarningType.Get_Warninginfo: IXMLWarninginfoType;
begin
  Result := ChildNodes['warning-info'] as IXMLWarninginfoType;
end;

function TXMLWarningType.Get_Area: IXMLAreaTypeList;
begin
  Result := FArea;
end;

{ TXMLWarninginfoType }

procedure TXMLWarninginfoType.AfterConstruction;
begin
  RegisterChildNode('text', TXMLTextelementType);
  ItemTag := 'text';
  ItemInterface := IXMLTextelementType;
  inherited;
end;

function TXMLWarninginfoType.Get_Text(const Index: Integer): IXMLTextelementType;
begin
  Result := List[Index] as IXMLTextelementType;
end;

function TXMLWarninginfoType.Add: IXMLTextelementType;
begin
  Result := AddItem(-1) as IXMLTextelementType;
end;

function TXMLWarninginfoType.Insert(const Index: Integer): IXMLTextelementType;
begin
  Result := AddItem(Index) as IXMLTextelementType;
end;

{ TXMLTextelementType }

procedure TXMLTextelementType.AfterConstruction;
begin
  ItemTag := 'p';
  ItemInterface := IXMLNode;
  inherited;
end;

function TXMLTextelementType.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLTextelementType.Set_Type_(const Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLTextelementType.Get_P(const Index: Integer): UnicodeString;
begin
  Result := List[Index].Text;
end;

function TXMLTextelementType.Add(const P: UnicodeString): IXMLNode;
begin
  Result := AddItem(-1);
  Result.NodeValue := P;
end;

function TXMLTextelementType.Insert(const Index: Integer; const P: UnicodeString): IXMLNode;
begin
  Result := AddItem(Index);
  Result.NodeValue := P;
end;

{ TXMLTextelementTypeList }

function TXMLTextelementTypeList.Add: IXMLTextelementType;
begin
  Result := AddItem(-1) as IXMLTextelementType;
end;

function TXMLTextelementTypeList.Insert(const Index: Integer): IXMLTextelementType;
begin
  Result := AddItem(Index) as IXMLTextelementType;
end;

function TXMLTextelementTypeList.Get_Item(const Index: Integer): IXMLTextelementType;
begin
  Result := List[Index] as IXMLTextelementType;
end;

{ TXMLAreaType }

procedure TXMLAreaType.AfterConstruction;
begin
  RegisterChildNode('warning-summary', TXMLWarningsummaryType);
  RegisterChildNode('forecast-period', TXMLForecastperiodType);
  RegisterChildNode('hazard', TXMLHazardType);
  RegisterChildNode('warning-group', TXMLWarninggroupType);
  FWarningsummary := CreateCollection(TXMLWarningsummaryTypeList, IXMLWarningsummaryType, 'warning-summary') as IXMLWarningsummaryTypeList;
  FForecastperiod := CreateCollection(TXMLForecastperiodTypeList, IXMLForecastperiodType, 'forecast-period') as IXMLForecastperiodTypeList;
  FHazard := CreateCollection(TXMLHazardTypeList, IXMLHazardType, 'hazard') as IXMLHazardTypeList;
  FWarninggroup := CreateCollection(TXMLWarninggroupTypeList, IXMLWarninggroupType, 'warning-group') as IXMLWarninggroupTypeList;
  inherited;
end;

function TXMLAreaType.Get_Aac: UnicodeString;
begin
  Result := AttributeNodes['aac'].Text;
end;

procedure TXMLAreaType.Set_Aac(const Value: UnicodeString);
begin
  SetAttribute('aac', Value);
end;

function TXMLAreaType.Get_Parentaac: UnicodeString;
begin
  Result := AttributeNodes['parent-aac'].Text;
end;

procedure TXMLAreaType.Set_Parentaac(const Value: UnicodeString);
begin
  SetAttribute('parent-aac', Value);
end;

function TXMLAreaType.Get_Description: UnicodeString;
begin
  Result := AttributeNodes['description'].Text;
end;

procedure TXMLAreaType.Set_Description(const Value: UnicodeString);
begin
  SetAttribute('description', Value);
end;

function TXMLAreaType.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLAreaType.Set_Type_(const Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLAreaType.Get_Warningsummary: IXMLWarningsummaryTypeList;
begin
  Result := FWarningsummary;
end;

function TXMLAreaType.Get_Forecastperiod: IXMLForecastperiodTypeList;
begin
  Result := FForecastperiod;
end;

function TXMLAreaType.Get_Hazard: IXMLHazardTypeList;
begin
  Result := FHazard;
end;

function TXMLAreaType.Get_Warninggroup: IXMLWarninggroupTypeList;
begin
  Result := FWarninggroup;
end;

{ TXMLAreaTypeList }

function TXMLAreaTypeList.Add: IXMLAreaType;
begin
  Result := AddItem(-1) as IXMLAreaType;
end;

function TXMLAreaTypeList.Insert(const Index: Integer): IXMLAreaType;
begin
  Result := AddItem(Index) as IXMLAreaType;
end;

function TXMLAreaTypeList.Get_Item(const Index: Integer): IXMLAreaType;
begin
  Result := List[Index] as IXMLAreaType;
end;

{ TXMLPeriodicwarningsummaryType }

procedure TXMLPeriodicwarningsummaryType.AfterConstruction;
begin
  ItemTag := 'p';
  ItemInterface := IXMLNode;
  inherited;
end;

function TXMLPeriodicwarningsummaryType.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLPeriodicwarningsummaryType.Set_Type_(const Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLPeriodicwarningsummaryType.Get_Productidentifier: UnicodeString;
begin
  Result := AttributeNodes['product-identifier'].Text;
end;

procedure TXMLPeriodicwarningsummaryType.Set_Productidentifier(const Value: UnicodeString);
begin
  SetAttribute('product-identifier', Value);
end;

function TXMLPeriodicwarningsummaryType.Get_Producturl: UnicodeString;
begin
  Result := AttributeNodes['product-url'].Text;
end;

procedure TXMLPeriodicwarningsummaryType.Set_Producturl(const Value: UnicodeString);
begin
  SetAttribute('product-url', Value);
end;

function TXMLPeriodicwarningsummaryType.Get_Aac: UnicodeString;
begin
  Result := AttributeNodes['aac'].Text;
end;

procedure TXMLPeriodicwarningsummaryType.Set_Aac(const Value: UnicodeString);
begin
  SetAttribute('aac', Value);
end;

function TXMLPeriodicwarningsummaryType.Get_Area: UnicodeString;
begin
  Result := AttributeNodes['area'].Text;
end;

procedure TXMLPeriodicwarningsummaryType.Set_Area(const Value: UnicodeString);
begin
  SetAttribute('area', Value);
end;

function TXMLPeriodicwarningsummaryType.Get_Areatype: UnicodeString;
begin
  Result := AttributeNodes['area-type'].Text;
end;

procedure TXMLPeriodicwarningsummaryType.Set_Areatype(const Value: UnicodeString);
begin
  SetAttribute('area-type', Value);
end;

function TXMLPeriodicwarningsummaryType.Get_P(const Index: Integer): UnicodeString;
begin
  Result := List[Index].Text;
end;

function TXMLPeriodicwarningsummaryType.Add(const P: UnicodeString): IXMLNode;
begin
  Result := AddItem(-1);
  Result.NodeValue := P;
end;

function TXMLPeriodicwarningsummaryType.Insert(const Index: Integer; const P: UnicodeString): IXMLNode;
begin
  Result := AddItem(Index);
  Result.NodeValue := P;
end;

{ TXMLPeriodicwarningsummaryTypeList }

function TXMLPeriodicwarningsummaryTypeList.Add: IXMLPeriodicwarningsummaryType;
begin
  Result := AddItem(-1) as IXMLPeriodicwarningsummaryType;
end;

function TXMLPeriodicwarningsummaryTypeList.Insert(const Index: Integer): IXMLPeriodicwarningsummaryType;
begin
  Result := AddItem(Index) as IXMLPeriodicwarningsummaryType;
end;

function TXMLPeriodicwarningsummaryTypeList.Get_Item(const Index: Integer): IXMLPeriodicwarningsummaryType;
begin
  Result := List[Index] as IXMLPeriodicwarningsummaryType;
end;

{ TXMLWarningsummaryType }

function TXMLWarningsummaryType.Get_Starttimeutc: UnicodeString;
begin
  Result := AttributeNodes['start-time-utc'].Text;
end;

procedure TXMLWarningsummaryType.Set_Starttimeutc(const Value: UnicodeString);
begin
  SetAttribute('start-time-utc', Value);
end;

function TXMLWarningsummaryType.Get_Starttimelocal: UnicodeString;
begin
  Result := AttributeNodes['start-time-local'].Text;
end;

procedure TXMLWarningsummaryType.Set_Starttimelocal(const Value: UnicodeString);
begin
  SetAttribute('start-time-local', Value);
end;

function TXMLWarningsummaryType.Get_Endtimeutc: UnicodeString;
begin
  Result := AttributeNodes['end-time-utc'].Text;
end;

procedure TXMLWarningsummaryType.Set_Endtimeutc(const Value: UnicodeString);
begin
  SetAttribute('end-time-utc', Value);
end;

function TXMLWarningsummaryType.Get_Endtimelocal: UnicodeString;
begin
  Result := AttributeNodes['end-time-local'].Text;
end;

procedure TXMLWarningsummaryType.Set_Endtimelocal(const Value: UnicodeString);
begin
  SetAttribute('end-time-local', Value);
end;

{ TXMLWarningsummaryTypeList }

function TXMLWarningsummaryTypeList.Add: IXMLWarningsummaryType;
begin
  Result := AddItem(-1) as IXMLWarningsummaryType;
end;

function TXMLWarningsummaryTypeList.Insert(const Index: Integer): IXMLWarningsummaryType;
begin
  Result := AddItem(Index) as IXMLWarningsummaryType;
end;

function TXMLWarningsummaryTypeList.Get_Item(const Index: Integer): IXMLWarningsummaryType;
begin
  Result := List[Index] as IXMLWarningsummaryType;
end;

{ TXMLForecastperiodType }

procedure TXMLForecastperiodType.AfterConstruction;
begin
  RegisterChildNode('warning-summary', TXMLPeriodicwarningsummaryType);
  RegisterChildNode('element', TXMLElementType);
  RegisterChildNode('text', TXMLTextelementType);
  RegisterChildNode('hazard', TXMLPeriodicHazardType);
  FWarningsummary := CreateCollection(TXMLPeriodicwarningsummaryTypeList, IXMLPeriodicwarningsummaryType, 'warning-summary') as IXMLPeriodicwarningsummaryTypeList;
  FElement := CreateCollection(TXMLElementTypeList, IXMLElementType, 'element') as IXMLElementTypeList;
  FText := CreateCollection(TXMLTextelementTypeList, IXMLTextelementType, 'text') as IXMLTextelementTypeList;
  FHazard := CreateCollection(TXMLPeriodicHazardTypeList, IXMLPeriodicHazardType, 'hazard') as IXMLPeriodicHazardTypeList;
  inherited;
end;

function TXMLForecastperiodType.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLForecastperiodType.Set_Index(const Value: LongWord);
begin
  SetAttribute('index', Value);
end;

function TXMLForecastperiodType.Get_Endindex: LongWord;
begin
  Result := AttributeNodes['end-index'].NodeValue;
end;

procedure TXMLForecastperiodType.Set_Endindex(const Value: LongWord);
begin
  SetAttribute('end-index', Value);
end;

function TXMLForecastperiodType.Get_Indextag: UnicodeString;
begin
  Result := AttributeNodes['index-tag'].Text;
end;

procedure TXMLForecastperiodType.Set_Indextag(const Value: UnicodeString);
begin
  SetAttribute('index-tag', Value);
end;

function TXMLForecastperiodType.Get_Starttimeutc: UnicodeString;
begin
  Result := AttributeNodes['start-time-utc'].Text;
end;

procedure TXMLForecastperiodType.Set_Starttimeutc(const Value: UnicodeString);
begin
  SetAttribute('start-time-utc', Value);
end;

function TXMLForecastperiodType.Get_Starttimelocal: UnicodeString;
begin
  Result := AttributeNodes['start-time-local'].Text;
end;

procedure TXMLForecastperiodType.Set_Starttimelocal(const Value: UnicodeString);
begin
  SetAttribute('start-time-local', Value);
end;

function TXMLForecastperiodType.Get_Endtimeutc: UnicodeString;
begin
  Result := AttributeNodes['end-time-utc'].Text;
end;

procedure TXMLForecastperiodType.Set_Endtimeutc(const Value: UnicodeString);
begin
  SetAttribute('end-time-utc', Value);
end;

function TXMLForecastperiodType.Get_Endtimelocal: UnicodeString;
begin
  Result := AttributeNodes['end-time-local'].Text;
end;

procedure TXMLForecastperiodType.Set_Endtimelocal(const Value: UnicodeString);
begin
  SetAttribute('end-time-local', Value);
end;

function TXMLForecastperiodType.Get_Enddatelocal: UnicodeString;
begin
  Result := AttributeNodes['end-date-local'].Text;
end;

procedure TXMLForecastperiodType.Set_Enddatelocal(const Value: UnicodeString);
begin
  SetAttribute('end-date-local', Value);
end;

function TXMLForecastperiodType.Get_Warningsummary: IXMLPeriodicwarningsummaryTypeList;
begin
  Result := FWarningsummary;
end;

function TXMLForecastperiodType.Get_Element: IXMLElementTypeList;
begin
  Result := FElement;
end;

function TXMLForecastperiodType.Get_Text: IXMLTextelementTypeList;
begin
  Result := FText;
end;

function TXMLForecastperiodType.Get_Hazard: IXMLPeriodicHazardTypeList;
begin
  Result := FHazard;
end;

{ TXMLForecastperiodTypeList }

function TXMLForecastperiodTypeList.Add: IXMLForecastperiodType;
begin
  Result := AddItem(-1) as IXMLForecastperiodType;
end;

function TXMLForecastperiodTypeList.Insert(const Index: Integer): IXMLForecastperiodType;
begin
  Result := AddItem(Index) as IXMLForecastperiodType;
end;

function TXMLForecastperiodTypeList.Get_Item(const Index: Integer): IXMLForecastperiodType;
begin
  Result := List[Index] as IXMLForecastperiodType;
end;

{ TXMLElementType }

function TXMLElementType.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLElementType.Set_Type_(const Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLElementType.Get_Units: UnicodeString;
begin
  Result := AttributeNodes['units'].Text;
end;

procedure TXMLElementType.Set_Units(const Value: UnicodeString);
begin
  SetAttribute('units', Value);
end;

function TXMLElementType.Get_Instance: UnicodeString;
begin
  Result := AttributeNodes['instance'].Text;
end;

procedure TXMLElementType.Set_Instance(const Value: UnicodeString);
begin
  SetAttribute('instance', Value);
end;

function TXMLElementType.Get_Sequence: LongWord;
begin
  Result := AttributeNodes['sequence'].NodeValue;
end;

procedure TXMLElementType.Set_Sequence(const Value: LongWord);
begin
  SetAttribute('sequence', Value);
end;

function TXMLElementType.Get_Duration: LongWord;
begin
  Result := AttributeNodes['duration'].NodeValue;
end;

procedure TXMLElementType.Set_Duration(const Value: LongWord);
begin
  SetAttribute('duration', Value);
end;

function TXMLElementType.Get_Timeutc: UnicodeString;
begin
  Result := AttributeNodes['time-utc'].Text;
end;

procedure TXMLElementType.Set_Timeutc(const Value: UnicodeString);
begin
  SetAttribute('time-utc', Value);
end;

function TXMLElementType.Get_Timelocal: UnicodeString;
begin
  Result := AttributeNodes['time-local'].Text;
end;

procedure TXMLElementType.Set_Timelocal(const Value: UnicodeString);
begin
  SetAttribute('time-local', Value);
end;

function TXMLElementType.Get_Starttimeutc: UnicodeString;
begin
  Result := AttributeNodes['start-time-utc'].Text;
end;

procedure TXMLElementType.Set_Starttimeutc(const Value: UnicodeString);
begin
  SetAttribute('start-time-utc', Value);
end;

function TXMLElementType.Get_Starttimelocal: UnicodeString;
begin
  Result := AttributeNodes['start-time-local'].Text;
end;

procedure TXMLElementType.Set_Starttimelocal(const Value: UnicodeString);
begin
  SetAttribute('start-time-local', Value);
end;

function TXMLElementType.Get_Endtimeutc: UnicodeString;
begin
  Result := AttributeNodes['end-time-utc'].Text;
end;

procedure TXMLElementType.Set_Endtimeutc(const Value: UnicodeString);
begin
  SetAttribute('end-time-utc', Value);
end;

function TXMLElementType.Get_Endtimelocal: UnicodeString;
begin
  Result := AttributeNodes['end-time-local'].Text;
end;

procedure TXMLElementType.Set_Endtimelocal(const Value: UnicodeString);
begin
  SetAttribute('end-time-local', Value);
end;

function TXMLElementType.Get_Startmonth: UnicodeString;
begin
  Result := AttributeNodes['start-month'].Text;
end;

procedure TXMLElementType.Set_Startmonth(const Value: UnicodeString);
begin
  SetAttribute('start-month', Value);
end;

function TXMLElementType.Get_Endmonth: UnicodeString;
begin
  Result := AttributeNodes['end-month'].Text;
end;

procedure TXMLElementType.Set_Endmonth(const Value: UnicodeString);
begin
  SetAttribute('end-month', Value);
end;

{ TXMLElementTypeList }

function TXMLElementTypeList.Add: IXMLElementType;
begin
  Result := AddItem(-1) as IXMLElementType;
end;

function TXMLElementTypeList.Insert(const Index: Integer): IXMLElementType;
begin
  Result := AddItem(Index) as IXMLElementType;
end;

function TXMLElementTypeList.Get_Item(const Index: Integer): IXMLElementType;
begin
  Result := List[Index] as IXMLElementType;
end;

{ TXMLWarninggroupType }

procedure TXMLWarninggroupType.AfterConstruction;
begin
  RegisterChildNode('coverage-area', TXMLAreaType);
  ItemTag := 'coverage-area';
  ItemInterface := IXMLAreaType;
  inherited;
end;

function TXMLWarninggroupType.Get_Priorityorder: LongWord;
begin
  Result := AttributeNodes['priority-order'].NodeValue;
end;

procedure TXMLWarninggroupType.Set_Priorityorder(const Value: LongWord);
begin
  SetAttribute('priority-order', Value);
end;

function TXMLWarninggroupType.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLWarninggroupType.Set_Type_(const Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLWarninggroupType.Get_Coveragearea(const Index: Integer): IXMLAreaType;
begin
  Result := List[Index] as IXMLAreaType;
end;

function TXMLWarninggroupType.Add: IXMLAreaType;
begin
  Result := AddItem(-1) as IXMLAreaType;
end;

function TXMLWarninggroupType.Insert(const Index: Integer): IXMLAreaType;
begin
  Result := AddItem(Index) as IXMLAreaType;
end;

{ TXMLWarninggroupTypeList }

function TXMLWarninggroupTypeList.Add: IXMLWarninggroupType;
begin
  Result := AddItem(-1) as IXMLWarninggroupType;
end;

function TXMLWarninggroupTypeList.Insert(const Index: Integer): IXMLWarninggroupType;
begin
  Result := AddItem(Index) as IXMLWarninggroupType;
end;

function TXMLWarninggroupTypeList.Get_Item(const Index: Integer): IXMLWarninggroupType;
begin
  Result := List[Index] as IXMLWarninggroupType;
end;

{ TXMLForecastType }

procedure TXMLForecastType.AfterConstruction;
begin
  RegisterChildNode('area', TXMLAreaType);
  ItemTag := 'area';
  ItemInterface := IXMLAreaType;
  inherited;
end;

function TXMLForecastType.Get_Area(const Index: Integer): IXMLAreaType;
begin
  Result := List[Index] as IXMLAreaType;
end;

function TXMLForecastType.Add: IXMLAreaType;
begin
  Result := AddItem(-1) as IXMLAreaType;
end;

function TXMLForecastType.Insert(const Index: Integer): IXMLAreaType;
begin
  Result := AddItem(Index) as IXMLAreaType;
end;

{ TXMLWarningsummariesType }

procedure TXMLWarningsummariesType.AfterConstruction;
begin
  RegisterChildNode('forecast-area', TXMLAreaType);
  ItemTag := 'forecast-area';
  ItemInterface := IXMLAreaType;
  inherited;
end;

function TXMLWarningsummariesType.Get_Forecastarea(const Index: Integer): IXMLAreaType;
begin
  Result := List[Index] as IXMLAreaType;
end;

function TXMLWarningsummariesType.Add: IXMLAreaType;
begin
  Result := AddItem(-1) as IXMLAreaType;
end;

function TXMLWarningsummariesType.Insert(const Index: Integer): IXMLAreaType;
begin
  Result := AddItem(Index) as IXMLAreaType;
end;

{ TXMLObservationsType }

procedure TXMLObservationsType.AfterConstruction;
begin
  RegisterChildNode('station', TXMLStationType);
  ItemTag := 'station';
  ItemInterface := IXMLStationType;
  inherited;
end;

function TXMLObservationsType.Get_Station(const Index: Integer): IXMLStationType;
begin
  Result := List[Index] as IXMLStationType;
end;

function TXMLObservationsType.Add: IXMLStationType;
begin
  Result := AddItem(-1) as IXMLStationType;
end;

function TXMLObservationsType.Insert(const Index: Integer): IXMLStationType;
begin
  Result := AddItem(Index) as IXMLStationType;
end;

{ TXMLStationType }

procedure TXMLStationType.AfterConstruction;
begin
  RegisterChildNode('period', TXMLObservationperiodType);
  ItemTag := 'period';
  ItemInterface := IXMLObservationperiodType;
  inherited;
end;

function TXMLStationType.Get_Bomid: UnicodeString;
begin
  Result := AttributeNodes['bom-id'].Text;
end;

procedure TXMLStationType.Set_Bomid(const Value: UnicodeString);
begin
  SetAttribute('bom-id', Value);
end;

function TXMLStationType.Get_Wmoid: UnicodeString;
begin
  Result := AttributeNodes['wmo-id'].Text;
end;

procedure TXMLStationType.Set_Wmoid(const Value: UnicodeString);
begin
  SetAttribute('wmo-id', Value);
end;

function TXMLStationType.Get_Aviationid: UnicodeString;
begin
  Result := AttributeNodes['aviation-id'].Text;
end;

procedure TXMLStationType.Set_Aviationid(const Value: UnicodeString);
begin
  SetAttribute('aviation-id', Value);
end;

function TXMLStationType.Get_Description: UnicodeString;
begin
  Result := AttributeNodes['description'].Text;
end;

procedure TXMLStationType.Set_Description(const Value: UnicodeString);
begin
  SetAttribute('description', Value);
end;

function TXMLStationType.Get_Tz: UnicodeString;
begin
  Result := AttributeNodes['tz'].Text;
end;

procedure TXMLStationType.Set_Tz(const Value: UnicodeString);
begin
  SetAttribute('tz', Value);
end;

function TXMLStationType.Get_Stnname: UnicodeString;
begin
  Result := AttributeNodes['stn-name'].Text;
end;

procedure TXMLStationType.Set_Stnname(const Value: UnicodeString);
begin
  SetAttribute('stn-name', Value);
end;

function TXMLStationType.Get_Stnheight: Single;
begin
  Result := XmlStrToFloatExt(AttributeNodes['stn-height'].Text);
end;

procedure TXMLStationType.Set_Stnheight(const Value: Single);
begin
  SetAttribute('stn-height', Value);
end;

function TXMLStationType.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLStationType.Set_Type_(const Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLStationType.Get_Owner: UnicodeString;
begin
  Result := AttributeNodes['owner'].Text;
end;

procedure TXMLStationType.Set_Owner(const Value: UnicodeString);
begin
  SetAttribute('owner', Value);
end;

function TXMLStationType.Get_Lat: Single;
begin
  Result := XmlStrToFloatExt(AttributeNodes['lat'].Text);
end;

procedure TXMLStationType.Set_Lat(const Value: Single);
begin
  SetAttribute('lat', Value);
end;

function TXMLStationType.Get_Lon: Single;
begin
  Result := XmlStrToFloatExt(AttributeNodes['lon'].Text);
end;

procedure TXMLStationType.Set_Lon(const Value: Single);
begin
  SetAttribute('lon', Value);
end;

function TXMLStationType.Get_Forecastdistrictid: UnicodeString;
begin
  Result := AttributeNodes['forecast-district-id'].Text;
end;

procedure TXMLStationType.Set_Forecastdistrictid(const Value: UnicodeString);
begin
  SetAttribute('forecast-district-id', Value);
end;

function TXMLStationType.Get_Period(const Index: Integer): IXMLObservationperiodType;
begin
  Result := List[Index] as IXMLObservationperiodType;
end;

function TXMLStationType.Add: IXMLObservationperiodType;
begin
  Result := AddItem(-1) as IXMLObservationperiodType;
end;

function TXMLStationType.Insert(const Index: Integer): IXMLObservationperiodType;
begin
  Result := AddItem(Index) as IXMLObservationperiodType;
end;

{ TXMLObservationperiodType }

procedure TXMLObservationperiodType.AfterConstruction;
begin
  RegisterChildNode('level', TXMLLevelType);
  RegisterChildNode('element', TXMLElementType);
  FLevel := CreateCollection(TXMLLevelTypeList, IXMLLevelType, 'level') as IXMLLevelTypeList;
  FElement := CreateCollection(TXMLElementTypeList, IXMLElementType, 'element') as IXMLElementTypeList;
  inherited;
end;

function TXMLObservationperiodType.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLObservationperiodType.Set_Index(const Value: LongWord);
begin
  SetAttribute('index', Value);
end;

function TXMLObservationperiodType.Get_Timeutc: UnicodeString;
begin
  Result := AttributeNodes['time-utc'].Text;
end;

procedure TXMLObservationperiodType.Set_Timeutc(const Value: UnicodeString);
begin
  SetAttribute('time-utc', Value);
end;

function TXMLObservationperiodType.Get_Timelocal: UnicodeString;
begin
  Result := AttributeNodes['time-local'].Text;
end;

procedure TXMLObservationperiodType.Set_Timelocal(const Value: UnicodeString);
begin
  SetAttribute('time-local', Value);
end;

function TXMLObservationperiodType.Get_Windsrc: UnicodeString;
begin
  Result := AttributeNodes['wind-src'].Text;
end;

procedure TXMLObservationperiodType.Set_Windsrc(const Value: UnicodeString);
begin
  SetAttribute('wind-src', Value);
end;

function TXMLObservationperiodType.Get_Level: IXMLLevelTypeList;
begin
  Result := FLevel;
end;

function TXMLObservationperiodType.Get_Element: IXMLElementTypeList;
begin
  Result := FElement;
end;

{ TXMLLevelType }

procedure TXMLLevelType.AfterConstruction;
begin
  RegisterChildNode('element', TXMLElementType);
  ItemTag := 'element';
  ItemInterface := IXMLElementType;
  inherited;
end;

function TXMLLevelType.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLLevelType.Set_Index(const Value: LongWord);
begin
  SetAttribute('index', Value);
end;

function TXMLLevelType.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLLevelType.Set_Type_(const Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLLevelType.Get_Element(const Index: Integer): IXMLElementType;
begin
  Result := List[Index] as IXMLElementType;
end;

function TXMLLevelType.Add: IXMLElementType;
begin
  Result := AddItem(-1) as IXMLElementType;
end;

function TXMLLevelType.Insert(const Index: Integer): IXMLElementType;
begin
  Result := AddItem(Index) as IXMLElementType;
end;

{ TXMLLevelTypeList }

function TXMLLevelTypeList.Add: IXMLLevelType;
begin
  Result := AddItem(-1) as IXMLLevelType;
end;

function TXMLLevelTypeList.Insert(const Index: Integer): IXMLLevelType;
begin
  Result := AddItem(Index) as IXMLLevelType;
end;

function TXMLLevelTypeList.Get_Item(const Index: Integer): IXMLLevelType;
begin
  Result := List[Index] as IXMLLevelType;
end;

end.