program TFTPServer;

{$mode delphi}{$H+}

{ Synapse example - TFTP Server                                                }
{                                                                              }
{ A TFTP (Trivial File Transfer Protocol) server example.                      }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B.              }
{                                                                              }
{ This example was adapted for Ultibo from the Synapse TFTPServer example.     }
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
  TFTPDaemonThread;

var
 Handle:THandle;

begin
 {Create our console window}
 Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
  
 ConsoleWindowWriteLn(Handle,'Starting Synapse FTP Server Example');
  
 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(Handle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(Handle,'C:\ drive is ready');
 ConsoleWindowWriteLn(Handle,'');
 
 {Create the TFTP server thread}
 TTFTPDaemonThread.Create('C:\','0.0.0.0','69',Handle);
 
 {The TTFTPDaemonThread classes uses Synchronize which waits on the MainThread.
  
  Even though Synchronize isn't really needed for writing to the console or other
  operations in Ultibo it still works, the main thread needs to periodically 
  check for events.
  
  Calling CheckSynchronize will process any events waiting from Synchronize calls}
 while True do
  begin
   CheckSynchronize;
   
   Sleep(0);
  end; 
  
 {Halt the main thread} 
 ThreadHalt(0);
end.
 