unit ServerConst1;

interface

uses
  IdContext,
  System.SysUtils
  ;

type
  TMVCParseAuthentication2 = class
  public
    class procedure OnParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: string; var VUsername,
      VPassword: string; var VHandled: Boolean);
  end;

resourcestring
  sPortInUse = '- Error: Port %s already in use';
  sPortSet = '- Port set to %s';
  sServerRunning = '- The Server is already running';
  sStartingServer = '- Starting HTTP Server on port %d';
  sStoppingServer = '- Stopping Server';
  sServerStopped = '- Server Stopped';
  sServerNotRunning = '- The Server is not running';
  sInvalidCommand = '- Error: Invalid Command';
  sIndyVersion = '- Indy Version: ';
  sActive = '- Active: ';
  sPort = '- Port: ';
  sSessionID = '- Session ID CookieName: ';
  sCommands = 'Enter a Command: ' + slineBreak +
    '   - "stop" to stop the server'+ slineBreak +
    '   - "exit" to close the application';

const
  cArrow = '->';
  cCommandStart = 'start';
  cCommandStop = 'stop';
  cCommandStatus = 'status';
  cCommandHelp = 'help';
  cCommandSetPort = 'set port';
  cCommandExit = 'exit';

implementation

{ TMVCParseAuthentication }

class procedure TMVCParseAuthentication2.OnParseAuthentication(
  AContext: TIdContext; const AAuthType, AAuthData: string; var VUsername,
  VPassword: string; var VHandled: Boolean);
begin
  VHandled := True; //SameText(LowerCase(AAuthType), 'Token');
end;

end.
