unit InitUnit;

{$mode delphi}{$H+}

{ The init unit uses a technique also shown in other examples where it gets to }
{ be included in the Initialization/Finalization chain before many other units }
{ and can adjust certain defaults and settings before the other units start.   }
{                                                                              }
{ In fact the init unit can actually be started prior to just about everything }
{  except the platform, threads and heap manager.                              }

interface

uses
  {We shouldn't include much here}
  GlobalConfig,
  GlobalConst;

  
implementation


initialization
 {Disable SysLog Logging}
 SYSLOG_REGISTER_LOGGING:=False;
 
 {Enable Serial Logging}
 SERIAL_REGISTER_LOGGING:=True;
 SERIAL_LOGGING_DEFAULT:=True;
 
 {Setup Default Colors}
 WINDOW_DEFAULT_FORECOLOR:=COLOR_WHITE;
 WINDOW_DEFAULT_BACKCOLOR:=COLOR_BLACK;
 
 {Disable Console Shell}
 CONSOLE_SHELL_ENABLED:=False;
 
end.

