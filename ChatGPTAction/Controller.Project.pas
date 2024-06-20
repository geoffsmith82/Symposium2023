unit Controller.Project;

interface

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.Swagger.Commons,
  MVCFramework.Serializer.Commons,
  System.SysUtils,
  System.Generics.Collections,
  System.Win.ComObj,
  System.Classes,
  JsonDataObjects,
  Winapi.Windows,
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

    // Update Project via diff File
    [MVCPath('/projects/($projectId)/files/($fileId)/diffupdate')]
    [MVCHTTPMethod([httpPOST])]
    procedure UpdateProjectViaDiffFile(projectId: String; fileId: String);

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
  MVCFramework.Logger,
  Files.Extra,
  Action.FileUtils,
  udmCompiler,
  udmFixInsight,
  udmDiff,
  FormUnit1
  ;

const ProjectPaths : string = 'D:\Programming\ChatGPTAction\Projects';


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


  LogMessage(DateTimeToStr(now) + ' ' + AContext.Request.PathInfo + ' ' + filename);
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
      project.Id := GetFileIDFromFilename(dir).ToString;
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
  dprFilename : string;
begin
  FileData := Context.Request.BodyAs<TProjectDProjRequest>;
  try
    // Extract file information from the JSON object
    dprFilename := FileData.ProjectFileName;
    projectPath := TPath.Combine(ProjectPaths, projectId);
    templateFilename := 'D:\Programming\ChatGPTAction\Projects\Snake\vcltemplate.dproj';
    fileContent := TFile.ReadAllText(templateFilename);

    if not TDirectory.Exists(projectPath) then
      raise Exception.Create('Project does not exist');
    try
      fullDprFilename := TPath.Combine(projectPath, dprFilename);
      dprojFilename := ChangeFileExt(fullDprFilename,'.dproj');
      guidProject := TGUID.NewGuid;
      fileContent := fileContent.Replace('<!--DPR-->', ExtractFilename(fullDprFilename));
      fileContent := fileContent.Replace('<!--NEWGUID-->', guidProject.ToString);
      TFile.WriteAllText(dprojFilename, fileContent);
    finally

    end;

    // Create response JSON object
    NewFile := TUploadedFileResponse.Create;
    try
      NewFile.Id := GetFileIDFromFilename(dprojFilename).ToString;
      NewFile.Filename := ExtractFileName(dprojFilename);
      NewFile.FileType:= '.dproj';
      NewFile.FileSize := TFile.GetSize(dprojFilename);
      NewFile.UploadDate := now;
//      NewFile.Description := description;

      Render(NewFile, False);  // Return the added file with 201 status
    finally
      FreeAndNil(NewFile);
    end;
  except
    on E: Exception do
    begin
      BadRequestResponse(E.Message);
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
begin
  FileData := Context.Request.BodyAs<TFileUploadRequest>;
  try
    // Extract file information from the JSON object
    FileName := FileData.FileName;
    fileContent := FileData.FileContent;
    projectPath := TPath.Combine(ProjectPaths, projectId);
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
      NewFileResponse.Id := GetFileIDFromFilename(fullPath).ToString;
      NewFileResponse.FileName := ExtractFileName(fullPath);
      NewFileResponse.FileType := ExtractFileExt(fullPath);
      NewFileResponse.FileSize := TFile.GetSize(fullPath);
      NewFileResponse.UploadDate := now;
      NewFileResponse.Description := description;
//      NewFile.S['url'] := '/path/to/upload/directory/' + FileName;
//      NewFile.S['description'] := Description;

      Render(NewFileResponse);  // Return the added file with 201 status
    finally
   //   NewFile.Free;
    end;

  except
    on E: Exception do
    begin
      LogMessage(E.Message);
      BadRequestResponse(E.Message);
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
    v := fileId.ToInt64;
    filePath := GetFilenameFromFileID(v);
    //GetFilePathFromObjectID('D:\Programming\project2.dpr', fileId);
    data := TFile.ReadAllText(filePath);
    //TPath.Combine(projectPath, fileId);
    // Set appropriate content type for binary data
    Context.Response.ContentType := 'application/octet-stream';
    Context.Response.Content := data;
    // Render the file content
    // RenderBinary(FileContent);
  except
    on E: Exception do
    begin
      NotFoundResponse(E.Message);
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
    v := fileId.ToInt64;
    fullPath := GetFilenameFromFileID(v);

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
      NewFile.Id := GetFileIDFromFilename(fullPath).ToString;
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
      BadRequestResponse(E.Message);
    end;
  end;
end;


procedure TProjectManagementController.UpdateProjectViaDiffFile(projectId, fileId: String);
var
  dmDiff: TdmDiff;
  projectPath : string;
  v : Int64;
  fullPath: string;
  diffRequest : TFileDiffUpdateRequest;
  tmpFilename : string;
begin
  dmDiff := TdmDiff.Create(nil);
  try
    diffRequest := Context.Request.BodyAs<TFileDiffUpdateRequest>;
    projectPath := TPath.Combine(ProjectPaths, projectId);
    tmpFilename := TPath.GetTempFileName;
    v := fileId.ToInt64;
    fullPath := GetFilenameFromFileID(v);
    TFile.WriteAllText(tmpFilename, diffRequest.DiffContent);

    dmDiff.MergeDiff(projectPath, fullPath, tmpFilename);
  finally
    FreeAndNil(dmDiff);
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
    CompileData := Context.Request.BodyAs<TProjectCompileRequest>;
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
begin
  ResponseObj := nil;
  dmCompiler := nil;
  CompileData := Context.Request.BodyAs<TProjectCompileRequest>;
  try
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

