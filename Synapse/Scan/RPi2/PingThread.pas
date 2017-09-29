unit PingThread;

{$IFDEF FPC}
  {$mode delphi}
{$endif}

interface

uses Classes, PingSend, IPUtils{$IFDEF ULTIBO}, GlobalConst, GlobalConfig{$ENDIF};

type
     PPingResult = ^TPingResult;
     TPingResult = Record
                     IPAddress:String;
                     Exists:Boolean;
                   end;


type
  TPingThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    PingResult:TPingResult;
    Ready:Boolean;
    constructor Create(Ping:TPingResult);
  end;

implementation

{ TPingThread }

constructor TPingThread.Create(Ping:TPingResult);
begin
  PingResult.IPAddress := Ping.IPAddress;
  inherited Create(False{$IFDEF ULTIBO},SIZE_128K{$ENDIF});
end;

procedure TPingThread.Execute;
var
  Ping:TPingSend;
begin
  Ready := false;
  Ping  := TPingSend.Create;
  Ping.Timeout := 2000;
  PingResult.Exists := Ping.Ping(PingResult.IPAddress);
  Ping.Free;
  Ready := true;
end;

end.
