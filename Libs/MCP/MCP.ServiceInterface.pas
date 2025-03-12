unit MCP.ServiceInterface;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.RTTI,
  System.TypInfo;

type
  /// <summary>
  /// Common interface for MCP Service implementations
  /// </summary>
  IMCPService = interface
    ['{F8D6F7A1-C25E-4B24-91AC-C69D0D2AE554}']
    // Core methods
    procedure Run;
    
    // Property getters
    function GetServiceName: string;
    function GetServiceVersion: string;
    
    // Properties
    property ServiceName: string read GetServiceName;
    property ServiceVersion: string read GetServiceVersion;
  end;
  
  /// <summary>
  /// Base for attribute-based MCP tool registration
  /// </summary>
  IMCPTool = interface
    ['{8C2E476B-D1F9-4B6A-8E97-FE42D6BC5AA4}']
    function GetName: string;
    function GetDescription: string;
    function GetParameterInfos: TArray<TValue>;
    
    property Name: string read GetName;
    property Description: string read GetDescription;
    property ParameterInfos: TArray<TValue> read GetParameterInfos;
  end;
  
  /// <summary>
  /// Factory for creating MCP services
  /// </summary>
  TMCPServiceFactory = class
  public
    class function CreateService(const ServiceClassName: string; const Params: array of TValue): IMCPService;
    class function GetAvailableServices: TArray<string>;
  end;

implementation

uses
  MCP.Attributes;

{ TMCPServiceFactory }

class function TMCPServiceFactory.CreateService(const ServiceClassName: string; const Params: array of TValue): IMCPService;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
  Instance: TValue;
begin
  RttiContext := TRttiContext.Create;
  try
    // Find the class type by name
    RttiType := RttiContext.FindType(ServiceClassName);
    if not Assigned(RttiType) or (RttiType.TypeKind <> tkClass) then
      raise Exception.CreateFmt('Service class "%s" not found', [ServiceClassName]);
      
    // Get the constructor
    RttiMethod := nil;
    for var Method in RttiType.GetMethods do
    begin
      if Method.IsConstructor and (Length(Method.GetParameters) = Length(Params)) then
      begin
        RttiMethod := Method;
        Break;
      end;
    end;
    
    if not Assigned(RttiMethod) then
      raise Exception.CreateFmt('Could not find a suitable constructor for "%s"', [ServiceClassName]);
      
    // Create instance using the constructor
    Instance := RttiMethod.Invoke(RttiType.AsInstance.MetaclassType, Params);
    
    // Check if it implements IMCPService
    if not Instance.AsObject.GetInterface(IMCPService, Result) then
      raise Exception.CreateFmt('Class "%s" does not implement IMCPService', [ServiceClassName]);
  finally
    RttiContext.Free;
  end;
end;

class function TMCPServiceFactory.GetAvailableServices: TArray<string>;
var
  RttiContext: TRttiContext;
  RttiTypes: TArray<TRttiType>;
  ServiceList: TList<string>;
begin
  ServiceList := TList<string>.Create;
  try
    RttiContext := TRttiContext.Create;
    try
      RttiTypes := RttiContext.GetTypes;
      
      // Find all classes that implement IMCPService
      for var RttiType in RttiTypes do
      begin
        if (RttiType.TypeKind = tkClass) and (RttiType.AsInstance.MetaclassType.GetInterfaceEntry(IMCPService) <> nil) then
          ServiceList.Add(RttiType.QualifiedName);
      end;
    finally
      RttiContext.Free;
    end;
    
    Result := ServiceList.ToArray;
  finally
    ServiceList.Free;
  end;
end;

end.
