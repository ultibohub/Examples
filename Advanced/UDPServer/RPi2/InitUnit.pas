unit InitUnit;

{$mode objfpc}{$H+}

{ Advanced example - UDP Server                                                }
{                                                                              }
{ This file is used during initialization of the program and allows us to make }
{ some changes to the startup behaviour so the built in SysLog client will send}
{ all logging entries to our example SysLog server.                            }

interface

uses
  GlobalConst,
  GlobalConfig;

implementation

initialization
 {Disable File Logging}
 FILESYS_REGISTER_LOGGING:=False;
 FILESYS_LOGGING_DEFAULT:=False;
 
 {Disable Serial Logging}
 SERIAL_REGISTER_LOGGING:=False;
 SERIAL_LOGGING_DEFAULT:=False;
 
 {Disable Console Logging}
 CONSOLE_REGISTER_LOGGING:=False;
 CONSOLE_LOGGING_DEFAULT:=False;
 
 {Configure SysLog Client}
 {Make sure SysLog will start automatically}
 SYSLOG_AUTOSTART:=True; 
 
 {Make sure SysLog registers as a logging device}
 SYSLOG_REGISTER_LOGGING:=True; 
 
 {And make sure it is the default}
 SYSLOG_LOGGING_DEFAULT:=True; 
 
 {Set SysLog to send logs to our UDP server}
 SYSLOG_SERVER_DEFAULT:='127.0.0.1';
 SYSLOG_PORT_DEFAULT:=514;
 SYSLOG_PROTOCOL_DEFAULT:=LOGGING_PROTOCOL_UDP;
end.

