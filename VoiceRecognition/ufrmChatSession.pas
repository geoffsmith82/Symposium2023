unit ufrmChatSession;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.DBCtrls,
  Data.DB,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Stan.Async,
  FireDAC.DApt,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  ufrmVoiceRecognition
  ;

type
  TfrmNewChatSession = class(TForm)
    btnNewSession: TButton;
    btnCancel: TButton;
    DBLookupListBox1: TDBLookupListBox;
    dsChatSessionList: TDataSource;
    tblChatSessionList: TFDTable;
    tblSelectedInitialSessionPrompt: TFDMemTable;
    tblSelectedInitialSessionPromptPromptID: TLargeintField;
    dsSelected: TDataSource;
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnNewSessionClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmNewChatSession: TfrmNewChatSession;

implementation

{$R *.dfm}

procedure TfrmNewChatSession.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmNewChatSession.btnNewSessionClick(Sender: TObject);
var
  SessionID : Int64;
begin
  frmVoiceRecognition.tblSessions.Append;
  try
    frmVoiceRecognition.tblSessions.FieldByName('CreationTime').AsDateTime := now;
    frmVoiceRecognition.tblSessions.FieldByName('SessionName').AsString := tblChatSessionList.FieldByName('Task').AsString;
    frmVoiceRecognition.tblSessions.Post;
    SessionID := frmVoiceRecognition.tblSessions.FieldByName('SessionID').AsLargeInt;

    frmVoiceRecognition.tblConversation.Append;
    try
      frmVoiceRecognition.tblConversation.FieldByName('SessionID').AsLargeInt := SessionID;
      frmVoiceRecognition.tblConversation.FieldByName('User').AsString := 'System';
      frmVoiceRecognition.tblConversation.FieldByName('Message').AsString := 'Task: ' + tblChatSessionList.FieldByName('Task').AsString +
         System.sLineBreak + 'Personality: ' + tblChatSessionList.FieldByName('Personality').AsString +
         System.sLineBreak + 'Prompt: "' + tblChatSessionList.FieldByName('Prompt').AsString +'"';
      frmVoiceRecognition.tblConversation.Post;
      Close;
    except
      frmVoiceRecognition.tblConversation.Cancel;
    end;

  except
    frmVoiceRecognition.tblSessions.Cancel;
  end;
end;

procedure TfrmNewChatSession.FormCreate(Sender: TObject);
begin
  tblChatSessionList.Active := True;
end;

end.
