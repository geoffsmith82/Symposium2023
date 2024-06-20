unit udmCompiler;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  DosCommand
  ;

type
  TdmCompiler = class(TDataModule)
    DosCommand: TDosCommand;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DosCommandNewLine(ASender: TObject; const ANewLine: string; AOutputType: TOutputType);
  private
    { Private declarations }
  public
    { Public declarations }
    OutputLines : TStringList;
    procedure CompileProject(projectDir: string; projectFile:string);
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdmCompiler.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(OutputLines);
end;

procedure TdmCompiler.CompileProject(projectDir, projectFile: string);
begin
  DosCommand.CurrentDir := projectDir;
  DosCommand.CommandLine := 'dcc32 -B  -NS"Vcl;System;Winapi" ' + projectFile;
  DosCommand.Execute;
  Sleep(200);
  repeat
    Application.ProcessMessages;
  until not DosCommand.IsRunning;
  Sleep(2000);
end;

procedure TdmCompiler.DataModuleCreate(Sender: TObject);
begin
  OutputLines := TStringList.Create;
end;

procedure TdmCompiler.DosCommandNewLine(ASender: TObject; const ANewLine:
    string; AOutputType: TOutputType);
begin
  if AOutputType = otEntireLine then
    OutputLines.Add(ANewLine);
end;

end.
