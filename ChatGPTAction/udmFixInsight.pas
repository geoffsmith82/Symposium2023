unit udmFixInsight;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  DosCommand
  ;

type
  TdmFixInsight = class(TDataModule)
    DosCommand: TDosCommand;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DosCommandNewLine(ASender: TObject; const ANewLine: string; AOutputType: TOutputType);
  private
    { Private declarations }
  public
    { Public declarations }
    OutputLines : TStringList;
    procedure RunFixInsight(projectDir, projectFile: string);
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdmFixInsight.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(OutputLines);
end;

procedure TdmFixInsight.DataModuleCreate(Sender: TObject);
begin
  OutputLines := TStringList.Create;
end;

procedure TdmFixInsight.DosCommandNewLine(ASender: TObject; const ANewLine:
    string; AOutputType: TOutputType);
begin
  if AOutputType = otEntireLine then
    OutputLines.Add(ANewLine);
end;

procedure TdmFixInsight.RunFixInsight(projectDir, projectFile: string);
begin
  DosCommand.CurrentDir := projectDir;
  DosCommand.CommandLine := 'C:\Program Files (x86)\FixInsight\FixInsightCL --xml --project=' + projectFile;
  DosCommand.Execute;
  Sleep(200);
  repeat
    Application.ProcessMessages;
  until not DosCommand.IsRunning;
  Sleep(2000);
end;

end.
