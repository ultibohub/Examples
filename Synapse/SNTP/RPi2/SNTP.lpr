program SNTP;

{$mode delphi}{$H+}

{ Synapse example - SNTP Client                                                }
{                                                                              }
{ An SNTP (Simple Network Time Protocol) client example.                       }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B.              }
{                                                                              }
{ This example was adapted for Ultibo from the Synapse sntp example.           }
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
  SNTPsend;

var
 Handle:THandle;
 SntpClient:TSntpSend;

begin
 {Create our console window}
 Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
  
 ConsoleWindowWriteLn(Handle,'Starting Synapse SNTP Client Example');
  
 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(Handle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(Handle,'C:\ drive is ready');
 ConsoleWindowWriteLn(Handle,'');
 
 {Wait a few seconds for the network to be ready}
 Sleep(3000);
 
 {Create the SNTP client}
 SntpClient:=TSntpSend.Create;
 try
  SntpClient.TargetHost:='pool.ntp.org';
  ConsoleWindowWriteLn(Handle,'Requesting SNTP time from ' + SntpClient.TargetHost);
  ConsoleWindowWriteLn(Handle,'');
  
  if SntpClient.GetSNTP then
   begin
    ConsoleWindowWriteLn(Handle,'SNTP time is ' + DateTimeToStr(SntpClient.NTPTime) + ' UTC');
   end
  else
   begin
    ConsoleWindowWriteLn(Handle,'Could not contact SNTP server ' + SntpClient.TargetHost);
   end;  
 finally
  SntpClient.Free;
 end;
 
 {Halt the main thread} 
 ThreadHalt(0);
end.
 