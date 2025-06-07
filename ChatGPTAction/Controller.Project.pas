unit Controller.Project;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Swagger.Commons,
  MVCFramework.Serializer.Commons,
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections,
  System.Win.ComObj,
  System.Classes,
  JsonDataObjects,
  Winapi.ShLwApi,
  Winapi.ActiveX,
  Vcl.FileCtrl,
  Vcl.Forms,
  Controller.Project.DataObjects
  ;

type
  [MVCPath('/api')]
  TProjectManagementController = class(TMVCController)
  public
    // List Projects
    [MVCPath('/projects')]
    [MVCHTTPMethod([httpGET])]
    procedure ListProjects;

    // Create Project
    [MVCPath('/projects')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateProject;

    // List Project Files
    [MVCPath('/projects/($projectId)/files')]
    [MVCHTTPMethod([httpGET])]
    procedure ListProjectFiles(projectId: String);

    // Add Project File
    [MVCPath('/projects/($projectId)/dproj')]
    [MVCHTTPMethod([httpPOST])]
    procedure AddDProjFile(const projectId: String);

    // Add Project File
    [MVCPath('/projects/($projectId)/checkproblems')]
    [MVCHTTPMethod([httpPOST])]
    procedure CheckProjectForProblems(const projectId: String);

    // Add Project File
    [MVCPath('/projects/($projectId)/files')]
    [MVCHTTPMethod([httpPOST])]
    procedure AddProjectFile(const projectId: String);

    // Get Project File
    [MVCPath('/projects/($projectId)/files/($fileId)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetProjectFile(projectId: String; fileId: String);

    // Update Project File
    [MVCPath('/projects/($projectId)/files/($fileId)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateProjectFile(projectId: String; fileId: String);

    // Compile Project
    [MVCPath('/projects/($projectId)/compile')]
    [MVCHTTPMethod([httpPOST])]
    procedure CompileProject(projectId: String);


    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean); override;

    procedure LogMessage(msg: string);
    procedure LogCompilerMessage(msg: string);

  end;

implementation

uses
  System.IOUtils,
  System.NetEncoding,
  FMX.Types,
  MVCFramework.Logger,
  Files.Extra,
  udmCompiler,
  udmFixInsight,
  udmDiff,
  FormUnit1
  ;

const ProjectPaths : string = 'Z:\Programming\ChatGPTAction\Projects';

function ReturnRelativePath(fullPath: string; basePath: string): string;
begin
  // Normalize the paths to ensure comparison is consistent
  fullPath := TPath.GetFullPath(fullPath);
  basePath := TPath.GetFullPath(basePath);

  // Check if the full path starts with the base path
  if StartsText(basePath, fullPath) then
  begin
    // Strip the basePath from the fullPath
    Result := fullPath.Substring(basePath.Length);

    // Remove any leading path separator
    if (Result <> '') and (Result[1] = PathDelim) then
      Result := Result.Substring(1);
  end
  else
  begin
    // If basePath is not part of fullPath, return the full path unchanged
    Result := fullPath;
  end;
end;

function IsPathBelowRoot(const RootPath, TestPath: string): Boolean;
var
  NormalizedRoot, NormalizedTest: string;
begin
  // Normalize the paths to remove any trailing slashes and resolve relative segments
  NormalizedRoot := TPath.GetFullPath(TPath.GetFullPath(RootPath));
  NormalizedTest := TPath.GetFullPath(TPath.GetFullPath(TestPath));

  // Check if the normalized test path starts with the normalized root path
  Result := NormalizedTest.StartsWith(NormalizedRoot, True) and
            (Length(NormalizedTest) > Length(NormalizedRoot)) and
            (NormalizedTest[Length(NormalizedRoot) + 1] = TPath.DirectorySeparatorChar);
end;

procedure TProjectManagementController.ListProjects;
var
  Page, PageSize: Integer;
  SortBy, Filter: String;
  projectList : TProjectsResponse;
  project : TProject;
  dirs : TArray<string>;
  dir: string;
begin
  // Extract query parameters
  Page := StrToIntDef(Context.Request.Params['page'], 1);
  PageSize := StrToIntDef(Context.Request.Params['pageSize'], 10);
  SortBy := Context.Request.Params['sortBy'];
  Filter := Context.Request.Params['filter'];

  try
    // Code to retrieve and paginate projects from storage (e.g., database)
    // Populate the ResponseObj JSON object with project data
    projectList := TProjectsResponse.Create;

    dirs := TDirectory.GetDirectories(ProjectPaths);
    for dir in dirs do
    begin
      project := TProject.Create;
      project.Name := ExtractFilename(dir);
      project.Id := project.Name;
      project.CreatedDate := TDirectory.GetCreationTime(dir);

      projectList.Projects.Add(project);
    end;
    Render(projectList);
  finally

  end;
end;

procedure TProjectManagementController.LogCompilerMessage(msg: string);
begin
  TThread.Queue(nil, procedure
     begin
       Form1.mmoCompilerLog.Lines.Add(msg);
     end);
end;

procedure TProjectManagementController.LogMessage(msg: string);
begin
  TThread.Queue(nil, procedure
     begin
       Form1.mmoRequests.Lines.Add(msg);
     end);
end;

procedure TProjectManagementController.OnBeforeAction(AContext: TWebContext;
  const AActionName: string; var AHandled: Boolean);
var
  FileData : TJSONObject;
  filename : string;
begin
  inherited;
  filename := '';
  if (AActionName='AddProjectFile') or (AActionName='UpdateProjectFile') then
  begin
    try
    FileData := Context.Request.BodyAs<TJSONObject>;
    // Extract file information from the JSON object
    FileName := FileData.S['fileName'];
    finally
      FreeAndNil(FileData);
    end;
  end;


  LogMessage(DateTimeToStr(now) + ' ' + AContext.Request.PathInfo + ' ' + filename + ' ' + Context.Request.Body);
end;

procedure TProjectManagementController.CreateProject;
var
  Project :TCreateProjectRequest;
  ProjectResponse: TCreateProjectResponse;
  projectPath : string;
begin
  Project := Context.Request.BodyAs<TCreateProjectRequest>;
  try
    // Code to create a new project using the data from Project JSON object
    // Store the project in storage (e.g., database)
    if Project.Name.IsEmpty then
      raise Exception.Create('Project name not specified');

    projectPath := TPath.Combine(ProjectPaths, Project.Name);
    TDirectory.CreateDirectory(projectPath);

    ProjectResponse := TCreateProjectResponse.Create;
    ProjectResponse.Id := ExtractFilename(Project.Name);
    ProjectResponse.Name := ExtractFilename(Project.Name);
    ProjectResponse.CreatedDate := TDirectory.GetCreationTime(projectPath);
    StatusCode := 201;
    Render(ProjectResponse);  // Return the created project with 201 status
  except
    on E: Exception do
    begin
      BadRequestResponse(E.Message);
    end;
  end;
end;

procedure TProjectManagementController.ListProjectFiles(projectId: String);
var
  Page, PageSize: Integer;
  SortBy, Filter: String;
  projects : TFilesResponse;
  projectPath : string;
  project : TProjectFile;
  dirs : TArray<string>;
  dir: string;
  relFilename : string;
begin
  // Extract query parameters
  Page := StrToIntDef(Context.Request.Params['page'], 1);
  PageSize := StrToIntDef(Context.Request.Params['pageSize'], 10);
  SortBy := Context.Request.Params['sortBy'];
  Filter := Context.Request.Params['filter'];

  try
    // Code to retrieve and paginate project files from storage (e.g., database)
    // Populate the ResponseObj JSON object with project files data
    projects := TFilesResponse.Create;
    projectPath := TPath.Combine(ProjectPaths, projectId);
    dirs := TDirectory.GetFiles(projectPath, '*', TSearchOption.soAllDirectories);
    for dir in dirs do
    begin
      project := TProjectFile.Create;
      relFilename := GetRelativePath(dir, projectPath);
      if relFilename.StartsWith('.\.git') then
        continue;
      if relFilename.StartsWith('.\Demo\Win32\Debug\CustomCache') or
         relFilename.StartsWith('.\Demo\Win32\Debug\SocialDemo.exe.WebView2') then
        continue;
      if ExtractFileExt(relFilename).ToUpper = '.DCU' then
      begin
        continue;
      end;

      project.FileName := relFilename;
      project.FileSize := TFile.GetSize(dir);
      project.Id := relFilename;
      projects.Files.Add(project);
    end;

    Render(projects);
  finally

  end;
end;

function TryBase64Decode(const Input: string): string;
begin
  try
    Result := TNetEncoding.Base64.Decode(Input);
  except
    // If an error occurs during decoding, return the original input string
    Result := Input;
  end;
end;

procedure TProjectManagementController.AddDProjFile(const projectId: string);
var
  FileData: TProjectDProjRequest;
  NewFile: TUploadedFileResponse;
  projectPath : string;
  fileContent : string;
  fullDprFilename : string;
  dprojFilename : string;
  templateFilename: string;
  guidProject : TGuid;
  srcRes : string;
  dstRes : string;
  JSONBody : TJSONObject;
begin
  FileData := TProjectDProjRequest.Create;
  JSONBody := TJSONObject.Parse(Context.Request.Body) as TJSONObject;
  FileData.dprogFileName := JSONBody.S['dprogFileName'];
  
  try
    // Extract file information from the JSON object
    dprojFilename := FileData.dprogFileName;
    if dprojFilename.IsEmpty then
      raise Exception.Create('dprojFileName was not set');


    if projectId.IsEmpty then
      raise Exception.Create('Must specify a projectId');

    projectPath := TPath.Combine(ProjectPaths, projectId);
    templateFilename := 'Z:\Programming\ChatGPTAction\Projects\Snake\vcltemplate.dproj';
    srcRes := 'Z:\Programming\ChatGPTAction\Projects\Snake\vcltemplate.res';
    fileContent := TFile.ReadAllText(templateFilename);

    if not TDirectory.Exists(projectPath) then
      raise Exception.Create('Project does not exist');
    try
      fullDprFilename := TPath.Combine(projectPath, ChangeFileExt(dprojFilename, '.dpr'));
      dstRes := ChangeFileExt(fullDprFilename, '.res');
      dprojFilename := ChangeFileExt(fullDprFilename,'.dproj');
      guidProject := TGUID.NewGuid;
      fileContent := fileContent.Replace('<!--DPR-->', ExtractFilename(fullDprFilename));
      fileContent := fileContent.Replace('<!--NEWGUID-->', guidProject.ToString);
      TFile.WriteAllText(dprojFilename, fileContent);
      TFile.Copy(srcRes, dstRes);
    finally

    end;

    // Create response JSON object
    NewFile := TUploadedFileResponse.Create;
    try
      NewFile.Id := ExtractFileName(dprojFilename);
      NewFile.Filename := ExtractFileName(dprojFilename);
      NewFile.FileType:= '.dproj';
      NewFile.FileSize := TFile.GetSize(dprojFilename);
      NewFile.UploadDate := now;

      Render(NewFile, False);  // Return the added file with 201 status
    finally
      FreeAndNil(NewFile);
    end;
  except
    on E: Exception do
    begin
      Context.Response.StatusCode := HTTP_STATUS.BadRequest;
      Context.Response.Content := E.Message;
    end;
  end;
end;

procedure TProjectManagementController.AddProjectFile(const projectId: String);
var
  FileData: TFileUploadRequest;
  FileName: string;
  NewFileResponse: TUploadedFileResponse;
  projectPath : string;
  fullPath : string;
  fileContent : string;
  description : string;
  JSONBody : TJSONObject;
begin
  FMX.Types.Log.d(Context.Request.Body);
  try
    JSONBody := TJSONObject.Parse(Context.Request.Body) as TJSONObject;
    FileData.FileName := JSONBody.Values['fileName'];
    FileData.FileContent := JSONBody.Values['fileContent'];    
    // Extract file information from the JSON object
    FileName := FileData.FileName;
    fileContent := FileData.FileContent;
    projectPath := TPath.Combine(ProjectPaths, projectId);
    if projectId.IsEmpty then
      raise Exception.Create('ProjectId was not set');

    if not TDirectory.Exists(projectPath) then
      raise Exception.Create('Project does not exist');
    if ExtractFileExt(filename).ToUpper = '.RES' then
      raise Exception.Create('You cant create RES files');
    if ExtractFileExt(filename).ToUpper = '.DPROJ' then
      raise Exception.Create('Use the CreateDProjFile endpoint instead of this one');


    try
      fullPath := TPath.Combine(projectPath, filename);
      TFile.WriteAllText(fullPath, fileContent);
    finally

    end;

    // Create response JSON object
    NewFileResponse := TUploadedFileResponse.Create;
    try
      NewFileResponse.Id := ExtractFileName(fullPath);

      NewFileResponse.FileName := ExtractFileName(fullPath);
      NewFileResponse.FileType := ExtractFileExt(fullPath);
      NewFileResponse.FileSize := TFile.GetSize(fullPath);
      NewFileResponse.UploadDate := now;
      NewFileResponse.Description := description;

      Render(NewFileResponse);  // Return the added file with 201 status
    finally

    end;

  except
    on E: Exception do
    begin
      LogMessage(E.Message);
      raise e;
    end;
  end;
end;

procedure TProjectManagementController.GetProjectFile(projectId, fileId: String);
var
  projectPath : string;
  filePath : string;
  data : string;
  v : Int64;
begin
  try
    // Code to retrieve the specific file contents from storage (e.g., database)
    projectPath := TPath.Combine(ProjectPaths, projectId);
    filePath :=  TPath.Combine(projectPath, fileId);

    if not IsPathBelowRoot(projectPath, filePath) then
      raise Exception.Create('Trying to escape root');

    data := TFile.ReadAllText(filePath);
    // Set appropriate content type for binary data
    Context.Response.ContentType := 'application/octet-stream';
    Context.Response.Content := data;

  except
    on E: Exception do
    begin
      Context.Response.StatusCode := HTTP_STATUS.NotFound;
      Context.Response.Content := E.Message;
    end;
  end;
end;

procedure TProjectManagementController.UpdateProjectFile(projectId, fileId: String);
var
  FileData: TJSONObject;
  FileName: string;
  NewFile: TProjectFile;
  projectPath: string;
  fullPath: string;
  fileContent: string;
  fs: TFileStream;
  position: Integer;
  decodedContent: TBytes;
  v : Int64;
begin
  FileData := nil;
  try
    FileData := Context.Request.BodyAs<TJSONObject>;
    // Extract file information from the JSON object
    FileName := FileData.S['fileName'];
    fileContent := FileData.S['fileContent'];
    position := FileData.I['position'];
    projectPath := TPath.Combine(ProjectPaths, projectId);
    fullPath :=  TPath.Combine(projectPath, fileId);
    

    if not IsPathBelowRoot(projectPath, fullPath) then
      raise Exception.Create('Trying to escape root');

    if TPath.GetExtension(fullPath).ToUpper = '.DPROJ' then
    begin
      raise Exception.Create('You can not update the dproj file');
    end;


    projectPath := TPath.Combine(ProjectPaths, projectId);

    decodedContent := TEncoding.UTF8.GetBytes(fileContent);

    // Open the file stream and write the decoded content
    fs := TFileStream.Create(fullPath, fmOpenReadWrite);
    try
      fs.Position := position;
      fs.Size := position;
      fs.WriteBuffer(decodedContent, Length(decodedContent));
    finally
      FreeAndNil(fs);
    end;

    NewFile := TProjectFile.Create;
    try
      NewFile.Id := ReturnRelativePath(fullPath, projectPath);
      NewFile.FileName := ExtractFileName(fullPath);
      NewFile.FileType := ExtractFileExt(fullPath);
      NewFile.FileSize := TFile.GetSize(fullPath);

      Render(NewFile, False);  // Return the added file with 201 status
    finally
      FreeAndNil(NewFile);
    end;
  except
    on E: Exception do
    begin
      LogMessage(E.Message);
      Context.Response.StatusCode := HTTP_STATUS.BadRequest;
      Context.Response.Content := E.Message;
    end;
  end;
end;

procedure TProjectManagementController.CheckProjectForProblems(const projectId: String);
var
  projectPath : string;
  projectFile : string;
  fullProjectFile : string;
  files : TArray<string>;
  dmFixInsight: TdmFixInsight;
  CompileData: TProjectCompileRequest;
  ResponseObj : TCompilationResponse;
  JSONBody : TJSONObject;
begin
  projectPath := TPath.Combine(ProjectPaths, projectId);
  if not TDirectory.Exists(projectPath) then
  begin
    raise Exception.Create('ERROR Project directory does not yet exist');
  end;

  files := TDirectory.GetFiles(projectPath, '*.dpr', TSearchOption.soAllDirectories);
  if length(files) = 0 then
  begin
    raise Exception.Create('ERROR Project dpr file does not exist yet');
  end;

  files := TDirectory.GetFiles(projectPath, '*.dproj', TSearchOption.soAllDirectories);
  if length(files) = 0 then
  begin
    raise Exception.Create('ERROR Project dproj file does not exist yet');
  end;

  dmFixInsight := nil;
  CompileData := nil;
  try
    dmFixInsight := TdmFixInsight.Create(nil);
    CompileData := TProjectCompileRequest.Create;

    JSONBody := TJSONObject.Parse(Context.Request.Body) as TJSONObject;
    CompileData.ProjectFileName := JSONBody.Values['projectFileName'];

    projectFile := CompileData.ProjectFileName;
    if TPath.GetExtension(projectFile).ToUpper = '.DPROJ' then
      projectFile := ChangeFileExt(projectFile, '.dpr');
    if projectFile.IsEmpty then
      raise Exception.Create('projectFileName parameter not specified');

    ResponseObj := TCompilationResponse.Create;
    ResponseObj.StartTime := now;

    fullProjectFile  := TPath.Combine(projectPath, projectFile);

    dmFixInsight.RunFixInsight(projectPath, fullProjectFile);
    ResponseObj.EndTime := now;
    ResponseObj.Message := dmFixInsight.OutputLines.Text;
    LogCompilerMessage(dmFixInsight.OutputLines.Text);
    Render(ResponseObj);
  finally
    FreeAndNil(dmFixInsight);
    FreeAndNil(CompileData);
  end;
end;

procedure TProjectManagementController.CompileProject(projectId: String);
var
  dmCompiler: TdmCompiler;
  CompileData: TProjectCompileRequest;
  projectPath : string;
  projectFile : string;
  ResponseObj : TCompilationResponse;
  JSONBody : TJSONObject;
begin
  ResponseObj := nil;
  dmCompiler := nil;
  CompileData := Context.Request.BodyAs<TProjectCompileRequest>;
  try
    JSONBody := TJSONObject.Parse(Context.Request.Body) as TJSONObject;
    CompileData.ProjectFileName := JSONBody.Values['projectFileName'];
  
    try
      ResponseObj := TCompilationResponse.Create;
      dmCompiler := TdmCompiler.Create(nil);
      // Code to compile the project and generate final output
      // Populate the CompileData JSON object with compilation details
      projectFile := CompileData.ProjectFileName;
      if projectFile.IsEmpty then
        raise Exception.Create('No Project file Specified');
      ResponseObj.StartTime := now;
      projectPath := TPath.Combine(ProjectPaths, projectId);

      projectFile := TPath.Combine(projectPath, projectFile);

      dmCompiler.CompileProject(projectPath, projectFile);
      ResponseObj.ConsoleOutput := dmCompiler.OutputLines.Text;
      ResponseObj.EndTime := now;
      LogCompilerMessage(dmCompiler.OutputLines.Text);

      Render(ResponseObj);  // Return the compilation details
    finally
      FreeAndNil(dmCompiler);
    end;
  except
    on E: Exception do
    begin
      StatusCode := 400;
      BadRequestResponse(E.Message);
    end;
  end;
end;

end.

