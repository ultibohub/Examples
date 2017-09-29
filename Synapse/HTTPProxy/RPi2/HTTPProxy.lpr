program HTTPProxy;

{$mode delphi}{$H+}

{ Synapse example - HTTP Proxy                                                 }
{                                                                              }
{ Using Synapse to create a HTTP proxy on port 3128.                           }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B.              }
{                                                                              }
{ This example was adapted for Ultibo from the Synapse httpproxy example.      }
{                                                                              }
{ Note: If you downloaded this example separate from the Synapse zip file      }
{ then you may need to add the path to the Synapse sources to your project     }
{ options.                                                                     }
{                                                                              }
{ To do that go to Project, Project Options... in the Lazarus menu and then    }
{ select Compiler Options, Other unit files (-Fu) and enter the correct path.  }
{                                                                              }

uses
  RaspberryPi2,
  GlobalConst,
  Threads,
  Classes,
  SysUtils,
  Console,
  ProxyThrd;

var
 Handle:THandle;

begin
 {Create our console window}
 Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
  
 ConsoleWindowWriteLn(Handle,'Starting Synapse HTTP Proxy Example');
  
 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(Handle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(Handle,'C:\ drive is ready');
 ConsoleWindowWriteLn(Handle,'');
 
 {Initialize the HTTP Proxy}
 InitService;
 
 {Create the HTTP Proxy}
 TServiceThread.Create;
 
 {Halt the main thread} 
 ThreadHalt(0);
end.
 