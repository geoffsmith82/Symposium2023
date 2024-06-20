unit Controller.Project.DataObjects;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Swagger.Commons,
  MVCFramework.Serializer.Commons,
  System.SysUtils,
  System.Generics.Collections;

type
  TProjectDProjRequest = class
  private
    FProjectFileName: string;
  public
    [MVCNameAs('dprFileName')]
    property ProjectFileName: string read FProjectFileName write FProjectFileName;
  end;


  TProjectCompileRequest = class
  private
    FProjectFileName: string;
  public
    [MVCNameAs('projectFileName')]
    property ProjectFileName: string read FProjectFileName write FProjectFileName;
  end;

  TCompilationResponse = class
  private
    FStatus: string;
    FMessage: string;
    FCompileId: string;
    FStartTime: TDateTime;
    FEndTime: TDateTime;
    FBuildId: string;
    FConsoleOutput: string;
  public
    [MVCNameAs('status')]
    property Status: string read FStatus write FStatus;

    [MVCNameAs('message')]
    property Message: string read FMessage write FMessage;

    [MVCNameAs('compileId')]
    property CompileId: string read FCompileId write FCompileId;

    [MVCNameAs('startTime')]
    property StartTime: TDateTime read FStartTime write FStartTime;

    [MVCNameAs('endTime')]
    property EndTime: TDateTime read FEndTime write FEndTime;

    [MVCNameAs('buildId')]
    property BuildId: string read FBuildId write FBuildId;

    [MVCNameAs('consoleOutput')]
    property ConsoleOutput: string read FConsoleOutput write FConsoleOutput;
  end;

  TCreateProjectResponse = class
  private
    FId: string;
    FName: string;
    FDescription: string;
    FStartDate: TDateTime;
    FEndDate: TDateTime;
    FStatus: string;
    FCreatedDate: TDateTime;
  public
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property StartDate: TDateTime read FStartDate write FStartDate;
    property EndDate: TDateTime read FEndDate write FEndDate;
    property Status: string read FStatus write FStatus;
    property CreatedDate: TDateTime read FCreatedDate write FCreatedDate;
  end;

  TCreateProjectRequest = class
  private
    FName: string;
    FDescription: string;
    FStartDate: string;
    FEndDate: string;
    FStatus: string;
  public
    [MVCNameAs('name')]
    property Name: string read FName write FName;
    [MVCNameAs('description')]
    property Description: string read FDescription write FDescription;
    [MVCNameAs('startDate')]
    property StartDate: string read FStartDate write FStartDate;
    [MVCNameAs('endDate')]
    property EndDate: string read FEndDate write FEndDate;
    [MVCNameAs('status')]
    property Status: string read FStatus write FStatus;
  end;

  TProjectFile = class
  private
    FId: string;
    FFileName: string;
    FFileType: string;
    FFileSize: Integer;
    FUploadDate: TDateTime;
    FUrl: string;
  public
    property Id: string read FId write FId;
    property FileName: string read FFileName write FFileName;
    property FileType: string read FFileType write FFileType;
    property FileSize: Integer read FFileSize write FFileSize;
    property UploadDate: TDateTime read FUploadDate write FUploadDate;
    property Url: string read FUrl write FUrl;
  end;

  TFilesResponse = class
  private
    FFiles: TObjectList<TProjectFile>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Files: TObjectList<TProjectFile> read FFiles write FFiles;
  end;

  TFileUploadRequest = class
  private
    FFileName: string;
    FFileContent: string;
  public
    [MVCNameAs('fileName')]
    property FileName: string read FFileName write FFileName;

    [MVCNameAs('fileContent')]
    property FileContent: string read FFileContent write FFileContent;
  end;

  TUploadedFileResponse = class
  private
    FId: string;
    FFileName: string;
    FFileType: string;
    FFileSize: Integer;
    FUploadDate: TDateTime;
    FUrl: string;
    FDescription: string;
  public
    [MVCNameAs('id')]
    property Id: string read FId write FId;

    [MVCNameAs('fileName')]
    property FileName: string read FFileName write FFileName;

    [MVCNameAs('fileType')]
    property FileType: string read FFileType write FFileType;

    [MVCNameAs('fileSize')]
    property FileSize: Integer read FFileSize write FFileSize;

    [MVCNameAs('uploadDate')]
    property UploadDate: TDateTime read FUploadDate write FUploadDate;

    [MVCNameAs('url')]
    property Url: string read FUrl write FUrl;

    [MVCNameAs('description')]
    property Description: string read FDescription write FDescription;
  end;

  TFileDiffUpdateRequest = class
  private
    FFileName: string;
    FDiffContent: string;
  public
    [MVCNameAs('fileName')]
    property FileName: string read FFileName write FFileName;

    [MVCNameAs('diffContent')]
    property DiffContent: string read FDiffContent write FDiffContent;
  end;


  TProject = class
  private
    FId: string;
    FName: string;
    FDescription: string;
    FCreatedDate: TDateTime;
    FStatus: string;
  public
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property CreatedDate: TDateTime read FCreatedDate write FCreatedDate;
    property Status: string read FStatus write FStatus;
  end;

  TProjectsResponse = class
  private
    FProjects: TObjectList<TProject>;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Projects: TObjectList<TProject> read FProjects write FProjects;
  end;

implementation

constructor TProjectsResponse.Create;
begin
  inherited Create;
  FProjects := TObjectList<TProject>.Create(True); // True means the list owns the objects
end;

destructor TProjectsResponse.Destroy;
begin
  FProjects.Free;
  inherited;
end;


constructor TFilesResponse.Create;
begin
  inherited Create;
  FFiles := TObjectList<TProjectFile>.Create(True); // True means the list owns the objects
end;

destructor TFilesResponse.Destroy;
begin
  FFiles.Free;
  inherited;
end;


end.
