{
   TFTP supports five types of packets, all of which have been mentioned
   above:
      opcode     operation

        1        Read request (RRQ)
        2        Write request (WRQ)
        3        Data (DATA)
        4        Acknowledgment (ACK)
        5        Error (ERROR)


   Error Codes
     Value       Meaning

       0         Not defined, see error message (if any).
       1         File not found.
       2         Access violation.
       3         Disk full or allocation exceeded.
       4         Illegal TFTP operation.
       5         Unknown transfer ID.
       6         File already exists.
       7         No such user.


}

unit TFTPDaemonThread;

{$IFDEF FPC}
  {$mode delphi}
{$endif}

interface

uses Classes, SysUtils, FTPTSend, Console;

type
  TTFTPDaemonThread = class(TThread)
  private
    { Private declarations }
    TFTPDaemon:TTFTPSend;
    FPath:String;
    FIPAddress:String;
    FPort:String;
    FHandle:THandle;
    FLogMessage:String;
    procedure UpdateLog;
  protected
    procedure Execute; override;
  public
    constructor Create(const Path,IPAddress,Port:String;Handle:THandle);
  end;

implementation

constructor TTFTPDaemonThread.Create(const Path,IPAddress,Port:String;Handle:THandle);
begin
  FPath := Path;
  FIPAddress := IPAddress;
  FPort     := Port;
  FHandle := Handle;
  inherited Create(False);
end;

procedure TTFTPDaemonThread.UpdateLOG;
begin
  ConsoleWindowWriteLn(FHandle,FLogMessage);
end;

procedure TTFTPDaemonThread.Execute;
var RequestType:Word;
    FileName:String;
begin
  TFTPDaemon := TTFTPSend.Create;
  FLogMessage := 'ServerThread created on Port ' + FPort;
  Synchronize(UpdateLog);
  TFTPDaemon.TargetHost := FIPAddress;
  TFTPDaemon.TargetPort := FPort;
  try
    while not terminated do
      begin
        if TFTPDaemon.WaitForRequest(RequestType,FileName)
          then
            begin
              // Fill the Log-Memo whith Infos about the request
              case RequestType of
                1:FLogMessage := 'Read-Request from '
                                 + TFTPDaemon.RequestIP + ':' + TFTPDaemon.RequestPort;
                2:FLogMessage := 'Write-Request from '
                                 + TFTPDaemon.RequestIP + ':' + TFTPDaemon.RequestPort;
              end;
              Synchronize(UpdateLog);
              FLogMessage := 'File: ' + Filename;
              Synchronize(UpdateLog);

              // Process the Request
              case RequestType of
                1:begin  // Read request (RRQ)
                    if FileExists(FPath + FileName)
                      then
                        begin
                          TFTPDaemon.Data.LoadFromFile(FPath + FileName);
                          if TFTPDaemon.ReplySend
                            then
                              begin
                                FLogMessage := '"' + FPath + FileName + '" successfully sent.';
                                Synchronize(UpdateLog);
                              end;
                        end
                      else TFTPDaemon.ReplyError(1,'File not Found');
                  end;
                2:begin  // Write request (WRQ)
                    if not FileExists(FPath + FileName)
                      then
                        begin
                          if TFTPDaemon.ReplyRecv
                            then
                              begin
                                TFTPDaemon.Data.SaveToFile(FPath + FileName);
                                FLogMessage := 'File sucessfully stored to ' + FPath + FileName;
                                Synchronize(UpdateLog);
                              end;
                        end
                      else TFTPDaemon.ReplyError(6,'File already exists');
                  end;
              end;
            end;
      end;
  finally
    TFTPDaemon.Free;
  end;
end;

end.
