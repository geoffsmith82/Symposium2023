unit udmDiff;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  DosCommand
  ;

type
  TdmDiff = class(TDataModule)
    DosCommand: TDosCommand;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DosCommandNewLine(ASender: TObject; const ANewLine: string;
        AOutputType: TOutputType);
  private
    { Private declarations }
  public
    { Public declarations }
    OutputLines : TStringList;
    procedure MergeDiff(projectdir: string; originalFilename, diffFilename: string);
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TdmDiff.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(OutputLines);
end;

procedure TdmDiff.DataModuleCreate(Sender: TObject);
begin
  OutputLines := TStringList.Create;
end;

procedure TdmDiff.DosCommandNewLine(ASender: TObject; const ANewLine: string;
    AOutputType: TOutputType);
begin
  if AOutputType = otEntireLine then
    OutputLines.Add(ANewLine);
end;

{ TdmDiff }

procedure TdmDiff.MergeDiff(projectDir: string; originalFilename, diffFilename: string);
begin
  DosCommand.CurrentDir := projectDir;
  DosCommand.CommandLine := Format('"C:\Program Files\TortoiseSVN\bin\TortoiseMerge.exe" /base:"%s" /theirs:"%s"',
                                   [originalFilename, diffFilename]);
  DosCommand.Execute;
  Sleep(200);
  repeat
    Application.ProcessMessages;
  until not DosCommand.IsRunning;
  Sleep(2000);
end;


end.
